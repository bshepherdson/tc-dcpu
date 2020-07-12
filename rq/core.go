package rq

import (
	"bytes"
	"fmt"
	"os"

	"github.com/shepheb/tc-dcpu/common"
)

const (
	cpsrN = 0x08
	cpsrZ = 0x04
	cpsrC = 0x02
	cpsrV = 0x01
	cpsrI = 0x80

	vectorReset = 0x00
	vectorIRQ   = 0x08
)

type rq struct {
	pc   uint16
	sp   uint16
	lr   uint16
	cpsr uint16 // Current CPSR.
	spsr uint16 // Saved CPSR.

	halted      bool
	debug       bool
	cycles      int
	regs        [8]uint16   // General-purpose registers, r0-r7
	ints        [256]uint16 // Pending interrupts.
	intCount    int
	devices     []common.Device
	breakpoints []uint16
	mem         [65536]uint16
}

// Implement the CPU interface.
func (c *rq) Memory() []uint16 {
	return c.mem[0:65536]
}
func (c *rq) ReadReg(r uint16) uint16 {
	return c.regs[r]
}
func (c *rq) WriteReg(r, val uint16) {
	c.regs[r] = val
}
func (c *rq) AddInterrupt(msg uint16) {
	c.addInterrupt(msg)
}
func (c *rq) AddDevice(dev common.Device) {
	c.devices = append(c.devices, dev)
}
func (c *rq) Devices() []common.Device {
	return c.devices
}
func (c *rq) AddBreakpoint(at uint32) {
	c.breakpoints = append(c.breakpoints, uint16(at))
}
func (c *rq) Debugging() *bool {
	return &c.debug
}
func (c *rq) Disassemble() {
	c.disasmROM()
}
func (c *rq) DisassembleOp(at uint32) uint16 {
	return c.disasmOp(uint16(at))
}

func (c *rq) DebugPrompt() {
	var buf bytes.Buffer
	if c.getFlag(cpsrN) {
		buf.WriteRune('N')
	} else {
		buf.WriteRune('-')
	}
	if c.getFlag(cpsrZ) {
		buf.WriteRune('Z')
	} else {
		buf.WriteRune('-')
	}
	if c.getFlag(cpsrC) {
		buf.WriteRune('C')
	} else {
		buf.WriteRune('-')
	}
	if c.getFlag(cpsrV) {
		buf.WriteRune('V')
	} else {
		buf.WriteRune('-')
	}
	fmt.Printf("%04x %s debug> ", c.pc, buf.String())
}
func (c *rq) Exit() {
	// Clean up all devices first.
	for _, d := range c.devices {
		d.Cleanup()
	}

	os.Exit(0)
}

func (c *rq) HardwareDelay(cycles int) {
	if c.cycles < cycles {
		c.cycles = cycles
	}
}

// NewRQ returns a freshly created Risque-16 instance.
func NewRQ() common.CPU {
	return new(rq)
}

// Implementation details.
func (c *rq) pcGet() uint16 {
	v := c.mem[c.pc]
	c.pc++
	return v
}

func (c *rq) pcPeek() uint16 {
	return c.mem[c.pc]
}

func (c *rq) pop() uint16 {
	v := c.mem[c.sp]
	c.sp++
	return v
}

func (c *rq) push(v uint16) {
	c.sp--
	c.mem[c.sp] = v
}

func (c *rq) addInterrupt(v uint16) {
	if c.intCount >= 256 {
		os.Exit(4)
	}

	c.ints[c.intCount] = v
	c.intCount++
}

func (c *rq) popInterrupt() uint16 {
	msg := c.ints[0]
	for i := 1; i < c.intCount; i++ {
		c.ints[i-1] = c.ints[i]
	}
	c.intCount--
	return msg
}

func (c *rq) setNZ(x uint16) {
	c.setFlag(cpsrZ, x == 0)
	c.setFlag(cpsrN, flag(x, 15))
}

func (c *rq) setFlag(mask uint16, b bool) {
	if b {
		c.cpsr |= mask
	} else {
		c.cpsr &^= mask
	}
}

func (c *rq) getFlag(mask uint16) bool {
	return (c.cpsr & mask) != 0
}

func bits(v, at, len uint16) uint16 {
	return (v >> at) & ((1 << len) - 1)
}

func signExtend(len, v uint16) int16 {
	if flag(v, len-1) { // Top bit 1
		return ((int16(-1) >> len) << len) | int16(bits(v, 0, len))
	}
	return int16(v)
}

func flag(v, at uint16) bool {
	return (v>>at)&1 != 0
}

func (c *rq) riOp(op uint16) {
	// 0oooodddXXXXXXXX
	opcode := (op >> 11) & 0xf
	dest := (op >> 8) & 0x7
	lit := op & 0xff

	switch opcode {
	case 0x0:
		c.riSpecOp(dest, lit)
	case 0x1: // MOV
		c.regs[dest] = lit
		c.setNZ(lit)
		c.cycles++
	case 0x2: // NEG
		c.regs[dest] = -lit
		c.setNZ(-lit)
		c.cycles++
	case 0x3: // CMP
		// Subtract and discard the result.
		c.subHelper(c.regs[dest], lit, false)
		c.cycles++
	case 0x4: // ADD
		c.regs[dest] = c.addHelper(c.regs[dest], lit, false)
		c.cycles++
	case 0x5: // SUB
		c.regs[dest] = c.subHelper(c.regs[dest], lit, false)
		c.cycles++
	case 0x6: // MUL
		res32 := uint32(c.regs[dest]) * uint32(lit)
		c.setFlag(cpsrC, res32&0xffff0000 != 0)
		inputNegative := c.regs[dest]&0x8000 != 0
		c.regs[dest] = c.regs[dest] * lit
		c.setNZ(c.regs[dest])
		outputNegative := c.regs[dest]&0x8000 != 0
		// With the literal always positive, the signs should match.
		c.setFlag(cpsrV, inputNegative != outputNegative)
		c.cycles += 4
	case 0x7: // LSL
		c.regs[dest] = c.leftShiftHelper(c.regs[dest], lit)
		c.cycles++
	case 0x8: // LSR
		c.regs[dest] = c.rightShiftHelper(c.regs[dest], lit, false)
		c.cycles++
	case 0x9: // ASR
		c.regs[dest] = c.rightShiftHelper(c.regs[dest], lit, true)
		c.cycles++
	case 0xa: // AND
		c.regs[dest] &= lit
		c.setNZ(c.regs[dest])
		c.cycles++
	case 0xb: // ORR
		c.regs[dest] |= lit
		c.setNZ(c.regs[dest])
		c.cycles++
	case 0xc: // XOR
		c.regs[dest] ^= lit
		c.setNZ(c.regs[dest])
		c.cycles++
	case 0xd: // ADD Rd, PC, #Imm
		c.regs[dest] = c.addHelper(c.pc, lit, false)
		c.cycles++
	case 0xe: // ADD Rd, SP, #Imm
		c.regs[dest] = c.addHelper(c.sp, lit, false)
		c.cycles++
	case 0xf: // MVH
		c.regs[dest] = (c.regs[dest] & 0xff) | (lit << 8)
		c.setNZ(c.regs[dest])
		c.cycles++
	}
}

func (c *rq) riSpecOp(opcode, lit uint16) {
	switch opcode {
	case 0x0: // ADD SP, #Imm
		c.sp += lit
		c.cycles++
	case 0x1: // SUB SP, #Imm
		c.sp -= lit
		c.cycles++
	case 0x2: // SWI #Imm
		// Some of them are special cases:
		// 0-7 prints that register.
		// SWI #1sss0nnn prints a string of length Rn and address Rs
		// c.addInterrupt(lit)
		if lit&0x80 != 0 {
			len := c.regs[lit&7]
			str := c.regs[(lit>>4)&7]
			var buf bytes.Buffer
			for i := uint16(0); i < len; i++ {
				buf.WriteRune(rune(c.mem[str+i]))
			}
			fmt.Printf("%04x: r%d=%d chars at r%d=%04x: \"%s\"\n", c.pc-1, lit&7, len, (lit>>4)&7, str, buf.String())
		} else {
			val := c.regs[lit&7]
			fmt.Printf("%04x: r%d = $%x = %d\n", c.pc-1, lit&7, val, val)
		}

		c.cycles += 4
	default:
		panic(fmt.Sprintf("unknown immediate op: $%x", opcode))
	}
}

func (c *rq) rrrOp(op uint16) {
	// 10ooooobbbaaaddd
	opcode := (op >> 9) & 0x1f
	rb := (op >> 6) & 7
	ra := (op >> 3) & 7
	rd := op & 7

	switch opcode {
	case 0x0:
		c.rrOp(rb, ra, rd)
		return
	case 0x1: // ADD
		c.regs[rd] = c.addHelper(c.regs[ra], c.regs[rb], false)
	case 0x2: // ADC
		c.regs[rd] = c.addHelper(c.regs[ra], c.regs[rb], true)
	case 0x3: // SUB
		c.regs[rd] = c.subHelper(c.regs[ra], c.regs[rb], false)
	case 0x4: // SBC
		c.regs[rd] = c.subHelper(c.regs[ra], c.regs[rb], true)
	case 0x5: // MUL
		av := c.regs[ra]
		bv := c.regs[rb]
		aNegative := av&0x8000 != 0
		bNegative := bv&0x8000 != 0
		c.regs[rd] = av * bv
		res32 := uint32(av) * uint32(bv)
		c.setNZ(c.regs[rd])
		c.setFlag(cpsrC, res32&0xffff0000 != 0)
		// Output sign should be negative iff one input is negative.
		// So if the inputs differ, output should be negative.
		// Otherwise it's a signed overflow.
		c.setFlag(cpsrV, (aNegative != bNegative) == (c.regs[rd]&0x8000 != 0))
		c.cycles += 3 // MUL takes 4 total
	case 0x6: // LSL
		c.regs[rd] = c.leftShiftHelper(c.regs[ra], c.regs[rb])
	case 0x7: // LSR
		c.regs[rd] = c.rightShiftHelper(c.regs[ra], c.regs[rb], false)
	case 0x8: // ASR
		c.regs[rd] = c.rightShiftHelper(c.regs[ra], c.regs[rb], true)
	case 0x9: // AND
		c.regs[rd] = c.regs[ra] & c.regs[rb]
		c.setNZ(c.regs[rd])
	case 0xa: // ORR
		c.regs[rd] = c.regs[ra] | c.regs[rb]
		c.setNZ(c.regs[rd])
	case 0xb: // XOR
		c.regs[rd] = c.regs[ra] ^ c.regs[rb]
		c.setNZ(c.regs[rd])
	default:
		panic(fmt.Sprintf("unknown RRR op: $%x", opcode))
	}
	c.cycles++
}

func (c *rq) rrOp(opcode, ra, rd uint16) {
	switch opcode {
	case 0x0:
		c.rOp(ra, rd)
		return
	case 0x1: // MOV
		c.regs[rd] = c.regs[ra]
		c.setNZ(c.regs[rd])
	case 0x2: // CMP
		c.subHelper(c.regs[rd], c.regs[ra], false) // Discard the result.
	case 0x3: // CMN
		c.addHelper(c.regs[rd], c.regs[ra], false) // Discard the result.
	case 0x4: // ROR
		// Rotating to the right.
		// First, normalize the value in ra to be 0 <= a < 16.
		shift := c.regs[ra] % 16
		c.regs[rd] = (c.regs[rd] >> shift) | (c.regs[rd] << (16 - shift))
		c.setNZ(c.regs[rd])
	case 0x5: // NEG
		c.regs[rd] = -c.regs[ra]
		c.setNZ(c.regs[rd])
	case 0x6: // TST
		c.setNZ(c.regs[rd] & c.regs[ra])
	case 0x7: // MVN
		c.regs[rd] = 0xffff ^ c.regs[ra]
		c.setNZ(c.regs[rd])
	}
	c.cycles++
}

func (c *rq) rOp(opcode, rd uint16) {
	switch opcode {
	case 0x0:
		c.voidOp(rd)
		return
	case 0x1: // BX
		c.pc = c.regs[rd]
	case 0x2: // BLX
		c.lr = c.pc
		c.pc = c.regs[rd]
	case 0x3: // SWI
		c.addInterrupt(c.regs[rd])
		c.cycles += 3 // 4 total.
	case 0x4: // HWN
		c.regs[rd] = uint16(len(c.devices))
		c.cycles += 3 // 4 total.
	case 0x5: // HWQ
		if int(c.regs[rd]) < len(c.devices) {
			d := c.devices[c.regs[rd]]
			id, version, maker := d.DeviceDetails()
			c.regs[1] = uint16(id >> 16)
			c.regs[0] = uint16(id & 0xffff)
			c.regs[2] = version
			c.regs[4] = uint16(maker >> 16)
			c.regs[3] = uint16(maker & 0xffff)
		} else {
			for i := 0; i < 5; i++ {
				c.regs[i] = 0
			}
		}
		c.cycles += 3 // 4 total.
	case 0x6: // HWI
		//fmt.Printf("rd = %d Rd = %d\n", rd, c.regs[rd])
		n := c.regs[rd]
		if int(n) < len(c.devices) {
			c.devices[n].Interrupt(c)
		}
		c.cycles += 3 // 4 total.
	case 0x7: // XSR - exchange SPSR with Rd.
		c.spsr, c.regs[rd] = c.regs[rd], c.spsr
		c.cycles++ // 2 total.
	}
	c.cycles++
}

func (c *rq) voidOp(opcode uint16) {
	switch opcode {
	case 0x0: // RFI
		// Pop r0
		c.regs[0] = c.pop()
		// Pop PC
		c.pc = c.pop()
		// Put SPSR back.
		c.cpsr = c.spsr
		c.cycles += 3 // 4 total.
	case 0x1: // IFS
		c.setFlag(cpsrI, true)
	case 0x2: // IFC
		c.setFlag(cpsrI, false)
	case 0x3: // RET
		c.pc = c.lr
	case 0x4: // POPSP
		c.sp = c.pop()
	case 0x5: // BRK
		c.debug = true
	default:
		panic(fmt.Sprintf("unknown void op: $%x", opcode))
	}
}

func (c *rq) branchOp(op uint16) {
	// All of these have the same meaning except BL. Only the conditions vary.
	doBranch := false
	opcode := (op >> 9) & 0xf
	switch opcode {
	case 0x0: // B
		doBranch = true
	case 0x1: // BL
		doBranch = true
	case 0x2: // BEQ - branch if Z
		doBranch = c.getFlag(cpsrZ)
	case 0x3: // BNE - branch if !Z
		doBranch = !c.getFlag(cpsrZ)
	case 0x4: // BCS - branch if C
		doBranch = c.getFlag(cpsrC)
	case 0x5: // BCC - branch if !C
		doBranch = !c.getFlag(cpsrC)
	case 0x6: // BMI - branch if N
		doBranch = c.getFlag(cpsrN)
	case 0x7: // BPL - branch if !N
		doBranch = !c.getFlag(cpsrN)
	case 0x8: // BVS - branch if V
		doBranch = c.getFlag(cpsrV)
	case 0x9: // BVC - branch if !V
		doBranch = !c.getFlag(cpsrV)
	case 0xa: // BHI - branch if C and !Z
		doBranch = c.getFlag(cpsrC) && !c.getFlag(cpsrZ)
	case 0xb: // BLS - branch if !C or Z
		doBranch = !c.getFlag(cpsrC) || c.getFlag(cpsrZ)
	case 0xc: // BGE - branch if N==V
		doBranch = c.getFlag(cpsrN) == c.getFlag(cpsrV)
	case 0xd: // BLT - branch if N!=V
		doBranch = c.getFlag(cpsrN) != c.getFlag(cpsrV)
	case 0xe: // BGT - branch if !Z && N==V
		doBranch = !c.getFlag(cpsrZ) && (c.getFlag(cpsrN) == c.getFlag(cpsrV))
	case 0xf: // BLE - branch if Z || N!=V
		doBranch = c.getFlag(cpsrZ) || (c.getFlag(cpsrN) != c.getFlag(cpsrV))
	}

	diff := op & 0x1ff
	if diff&0x100 != 0 {
		diff |= 0xfe00 // Sign-extend
	}
	target := c.pc + diff
	slow := false
	if diff == 0xffff {
		target = c.pcGet()
		slow = true
	}

	if doBranch {
		if opcode == 0x1 { // BL
			c.lr = c.pc
		}
		c.pc = target
	} else {
		slow = true
	}

	c.cycles++
	if slow {
		c.cycles++
	}
}

func (c *rq) memOp(op uint16) {
	opcode := (op >> 10) & 0x7
	rd := (op >> 7) & 0x7
	rb := (op >> 4) & 0x7
	lit := op & 0xf

	switch opcode {
	case 0x0: // LDR with post-increment literal
		c.regs[rd] = c.mem[c.regs[rb]]
		c.regs[rb] += lit
	case 0x1: // STR with post-increment literal
		c.mem[c.regs[rb]] = c.regs[rd]
		c.regs[rb] += lit
	case 0x2: // LDR with index literal
		c.regs[rd] = c.mem[c.regs[rb]+lit]
	case 0x3: // STR with index literal
		c.mem[c.regs[rb]+lit] = c.regs[rd]
	case 0x4: // LDR with index register
		c.regs[rd] = c.mem[c.regs[rb]+c.regs[lit&7]]
		c.cycles++
	case 0x5: // STR with index register
		c.mem[c.regs[rb]+c.regs[lit&7]] = c.regs[rd]
		c.cycles++
	case 0x6: // LDR from SP with index literal
		c.regs[rd] = c.mem[c.sp+lit]
	case 0x7: // STR at SP with index literal
		c.mem[c.sp+lit] = c.regs[rd]
	}
	c.cycles++
}

func (c *rq) multiStoreOp(op uint16) {
	// 111oobbbrrrrrrrr
	opcode := (op >> 11) & 3
	rb := (op >> 8) & 7
	regs := op & 0xff
	switch opcode {
	case 0x0: // POP
		sp, count := c.multiLoadHelper(regs, rb != 0, c.sp)
		c.sp = sp
		if rb != 0 {
			c.cycles++ // One extra for popping PC.
		}
		c.cycles += count
	case 0x1: // PUSH
		// PUSH is special, we need to figure out how many to shift.
		count := 0
		for i := uint(0); i < 8; i++ {
			if regs&(1<<i) != 0 {
				count++
			}
		}
		if rb != 0 {
			count++
		}

		c.cycles += count
		c.sp -= uint16(count)
		c.multiStoreHelper(regs, rb != 0, c.sp)
	case 0x2: // LDMIA
		b, count := c.multiLoadHelper(regs, false, c.regs[rb])
		c.regs[rb] = b
		c.cycles += count
	case 0x3: // STMIA
		b, count := c.multiStoreHelper(regs, false, c.regs[rb])
		c.regs[rb] = b
		c.cycles += count
	}
}

func (c *rq) multiLoadHelper(regs uint16, pc bool, base uint16) (uint16, int) {
	count := 0
	for i := uint(0); i < 8; i++ {
		if regs&(1<<i) != 0 {
			c.regs[i] = c.mem[base]
			base++
			count++
		}
	}
	if pc {
		c.pc = c.mem[base]
		base++
		count++
	}
	return base, count
}

func (c *rq) multiStoreHelper(regs uint16, lr bool, base uint16) (uint16, int) {
	count := 0
	for i := uint(0); i < 8; i++ {
		if regs&(1<<i) != 0 {
			c.mem[base] = c.regs[i]
			base++
			count++
		}
	}
	if lr {
		c.mem[base] = c.lr
		base++
		count++
	}
	return base, count
}

// Sets all four flags based on the addition operation.
// Returns the resulting value.
func (c *rq) addHelper(lhs, rhs uint16, withCarry bool) uint16 {
	lhSign := flag(lhs, 15)
	rhSign := flag(rhs, 15)

	var carry uint16
	if withCarry && c.getFlag(cpsrC) {
		carry = 1
	}

	res := lhs + rhs + carry
	// Overflow happens when the two inputs have the same sign, and the result
	// sign differs.
	// If the input signs are different, then there can't be overflow.
	c.setFlag(cpsrV, (lhSign == rhSign) && (flag(res, 15) != lhSign))
	c.setNZ(res)
	// Carry happens when bits are carried out of the lower 16 bits.
	c.setFlag(cpsrC, ((uint32(lhs)+uint32(rhs))>>16) != 0)
	return res
}

// Sets all four flags based on the subtraction operation.
// Returns the resulting value.
func (c *rq) subHelper(lhs, rhs uint16, withCarry bool) uint16 {
	lhSign := flag(lhs, 15)
	rhSign := !flag(rhs, 15) // Negated for the overflow calculations

	// Carry flag is negated here.
	var carry uint16
	if withCarry {
		carry = 1 // It's NOT C-bit subtracted.
		if c.getFlag(cpsrC) {
			carry = 0
		}
	}

	// Carry flag is used as a borrow. That is, we imagine setting an extra, 17th
	// bit on the LHS, and then subtracting. The output C bit is that same bit
	// after the subtraction.
	//fmt.Printf("sub: lhs %04x rhs %04x extra carry %04x\n", lhs, rhs, carry)
	res32 := (0x10000 | uint32(lhs)) - uint32(rhs) - uint32(carry)
	//fmt.Printf("res %08x\n", res32)
	c.setFlag(cpsrC, (res32&0x10000) != 0)
	res := uint16(res32)
	// Overflow happens when the two inputs have the same sign, and the result
	// sign differs.
	// If the input signs are different, then there can't be overflow.
	c.setFlag(cpsrV, (lhSign == rhSign) && (flag(res, 15) != lhSign))
	c.setNZ(res)
	return res
}

func (c *rq) leftShiftHelper(lhs, rhs uint16) uint16 {
	res32 := uint32(lhs) << rhs
	res := uint16(res32)
	c.setNZ(res)
	c.setFlag(cpsrC, (res32&0x10000) != 0)
	return res
}

func (c *rq) rightShiftHelper(lhs, rhs uint16, signed bool) uint16 {
	res := lhs >> rhs
	lastBit := (lhs >> (rhs - 1)) & 1
	if signed {
		res = uint16(int16(lhs) >> rhs)
		lastBit = uint16((int16(lhs) >> (rhs - 1)) & 1)
	}
	c.setFlag(cpsrC, lastBit != 0)
	c.setNZ(res)
	return res
}

// Runs a single cycle. That might mean doing nothing.
// Returns true if an instruction was executed, false if not.
func (c *rq) RunOp() bool {
	// Tick the hardware devices.
	for _, dev := range c.devices {
		dev.Tick(c)
	}

	if c.cycles > 1 {
		c.cycles--
		return false
	}

	c.cycles = 0

	// Check for interrupts before running the operation.
	if c.getFlag(cpsrI) && c.intCount > 0 {
		// TODO: Use a circular buffer or something to stop this being so costly.
		msg := c.popInterrupt()
		c.spsr = c.cpsr
		c.cpsr = 0 // All conditions clear, interrupts disabled.
		c.push(c.pc)
		c.push(c.regs[0])
		c.regs[0] = msg
		c.pc = vectorIRQ
	}

	x := c.pcGet()
	if x&0x8000 == 0 { // RI ops have the top bit clear.
		c.riOp(x)
	} else {
		switch x >> 13 {
		case 4: // Register format
			c.rrrOp(x)
		case 5: // Branch format
			c.branchOp(x)
		case 6: // Memory format
			c.memOp(x)
		case 7: // Multi-store format
			c.multiStoreOp(x)
		}
	}
	return true
}

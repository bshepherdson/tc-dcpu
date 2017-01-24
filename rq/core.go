package rq

import (
	"emulator/common"
	"fmt"
	"os"
)

const (
	cpsrMode          uint16 = 0x1f
	cpsrModeUser             = 0x10
	cpsrModeSWI              = 0x11
	cpsrModeIRQ              = 0x12
	cpsrModeAbort            = 0x18
	cpsrModeUndefined        = 0x1c

	cpsrN = 0x8000
	cpsrZ = 0x4000
	cpsrC = 0x2000
	cpsrV = 0x1000
	cpsrI = 0x0800

	vectorReset     = 0x00
	vectorIRQ       = 0x08
	vectorSWI       = 0x10
	vectorAbort     = 0x18
	vectorUndefined = 0x20
)

type rq struct {
	pc      *uint16 // Points to the current mode's PC_*
	usrPC   uint16  // PC for User mode.
	irqPC   uint16  // PC for IRQ mode.
	sp      *uint16 // Points to the current mode's SP_*
	usrSP   uint16  // SP for User mode.
	irqSP   uint16  // SP for IRQ mode.
	lr      *uint16 // Points to the current mode's LR_*
	usrLR   uint16  // LR for User mode.
	irqLR   uint16  // LR for IRQ mode.
	swiLR   uint16  // LR for SWI mode.
	cpsr    uint16  // Current CPSR.
	irqSPSR uint16  // Saved CPSR in IRQ mode.
	swiSPSR uint16  // Saved CPSR in SWI mode.

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
func (c *rq) AddBreakpoint(at uint16) {
	c.breakpoints = append(c.breakpoints, at)
}
func (c *rq) Debugging() *bool {
	return &c.debug
}
func (c *rq) Disassemble() {
	c.disasmROM()
}
func (c *rq) DisassembleOp(at uint16) uint16 {
	return c.disasmOp(at)
}

func (c *rq) DebugPrompt() {
	fmt.Printf("%04x debug> ", *c.pc)
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
	c := new(rq)
	c.cpsr = cpsrModeUser // Startup state is User mode, interrupts on.
	c.setFlag(cpsrI, true)
	c.setMode() // Now the PC and SP pointers are correct.

	c.debug = true

	return c
}

// Sets the PC, LR and SP pointers appropriately for the current mode.
// DOES NOT adjust CPSR and the SPSR_*s, those need more care.
func (c *rq) setMode() {
	switch c.cpsr & cpsrMode {
	case cpsrModeUser:
		c.pc = &c.usrPC
		c.sp = &c.usrSP
		c.lr = &c.usrLR
	case cpsrModeIRQ:
		c.pc = &c.irqPC
		c.sp = &c.irqSP
		c.lr = &c.irqLR
	case cpsrModeSWI:
		c.pc = &c.usrPC
		c.sp = &c.usrPC
		c.lr = &c.swiLR
	}
}

// Implementation details.
func (c *rq) pcGet() uint16 {
	v := c.mem[*c.pc]
	*c.pc++
	return v
}

func (c *rq) pcPeek() uint16 {
	return c.mem[*c.pc]
}

func (c *rq) pop() uint16 {
	v := c.mem[*c.sp]
	*c.sp++
	return v
}

func (c *rq) push(v uint16) {
	*c.sp--
	c.mem[*c.sp] = v
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

// Examines the top 4 bits of the opcode and redirects to the right format
// handlers.
var formatHandlers = []func(c *rq, op uint16){
	format1Or2,
	format1Or2,
	format3,
	format3,

	// 4
	format4Thru8,
	format9Or10,
	format11,
	format11,

	// 8
	format12,
	format13,
	format14,
	format15Or16,

	// c
	format17,
	format18Or19,
	format20,
	format21Or22,
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

// Shifts (1) or 3-value add/sub (2).
// Shifts set NZC-, math sets all.
func format1Or2(c *rq, op uint16) {
	code := bits(op, 11, 2)
	if code == 3 {
		format2(c, op)
		return
	}

	// Format 1
	rs := bits(op, 3, 3)
	rd := bits(op, 0, 3)
	imm := bits(op, 6, 5)

	var res uint16
	if imm == 0 {
		res = c.regs[rs]
	} else {
		switch bits(op, 11, 2) {
		case 0: // LSL
			res = lslHelper(c, c.regs[rs], imm)
		case 1: // LSR
			res = lsrHelper(c, c.regs[rs], imm)
		case 2: // ASR
			res = asrHelper(c, c.regs[rs], imm)
		}
	}
	c.setNZ(res)
	c.regs[rd] = res
	c.cycles++
}

// Helpers for shifts. These set NZC-.
func lslHelper(c *rq, lhs, rhs uint16) uint16 {
	c.setFlag(cpsrC, flag(lhs, 16-rhs))
	res := lhs << rhs
	c.setNZ(res)
	return res
}
func lsrHelper(c *rq, lhs, rhs uint16) uint16 {
	c.setFlag(cpsrC, flag(lhs, rhs-1))
	res := lhs >> rhs
	c.setNZ(res)
	return res
}
func asrHelper(c *rq, lhs, rhs uint16) uint16 {
	// Special case: cap the shift-out at 15.
	if rhs >= 16 {
		c.setFlag(cpsrC, flag(lhs, 15))
	} else {
		c.setFlag(cpsrC, flag(lhs, rhs-1))
	}
	res := uint16(int16(lhs) >> rhs)
	c.setNZ(res)
	return res
}

// Sets all four flags based on the addition operation.
// Returns the resulting value.
func addHelper(c *rq, lhs, rhs uint16, withCarry bool) uint16 {
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
func subHelper(c *rq, lhs, rhs uint16, withCarry bool) uint16 {
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

	res := lhs - rhs - carry
	// Overflow happens when the two inputs have the same sign, and the result
	// sign differs.
	// If the input signs are different, then there can't be overflow.
	c.setFlag(cpsrV, (lhSign == rhSign) && (flag(res, 15) != lhSign))
	c.setNZ(res)
	// Carry/borrow happens when the RHS is unsigned-greater than the LHS.
	c.setFlag(cpsrC, rhs > lhs) // Unsigned borrow flag.
	return res
}

// Overflow bits: When the two inputs have the same sign, and the output
// differs.
// For subtraction, invert the RHS bit.
func format2(c *rq, op uint16) {
	rd := bits(op, 0, 3)
	lhs := c.regs[bits(op, 3, 1)]
	rhs := bits(op, 6, 3)
	if !flag(op, 10) {
		rhs = c.regs[rhs] // When the flag is false, it's a register.
	}

	var res uint16
	if flag(op, 9) {
		res = subHelper(c, lhs, rhs, false)
	} else {
		res = addHelper(c, lhs, rhs, false)
	}
	c.regs[rd] = res
	c.cycles++
}

// Format 3: Math with 8-bit immediate.
func format3(c *rq, op uint16) {
	rd := bits(op, 8, 3)
	lhs := c.regs[rd]
	imm := bits(op, 0, 8)
	switch bits(op, 11, 2) {
	case 0: // MOV
		c.setNZ(imm)
		c.regs[rd] = imm
	case 1: // CMP
		subHelper(c, lhs, imm, false) // Discard result.
	case 2: // ADD
		c.regs[rd] = addHelper(c, lhs, imm, false)
	case 3: // SUB
		c.regs[rd] = subHelper(c, lhs, imm, false)
	}

	c.cycles++
}

func format4Thru8(c *rq, op uint16) {
	if bits(op, 10, 6) == 0x10 {
		format4(c, op)
	} else if bits(op, 7, 8) == 0x08c {
		format5(c, op)
	} else if bits(op, 7, 8) == 0x08d {
		format6(c, op)
	} else if bits(op, 8, 7) == 0x047 {
		format7(c, op)
	} else if bits(op, 11, 5) == 0x09 {
		format8(c, op)
	} else {
		fmt.Printf("Error with decoding: %016b is not valid\n", op)
	}
}

// Format 4: Arithmetic on registers.
func format4(c *rq, op uint16) {
	rs := bits(op, 3, 3)
	rd := bits(op, 0, 3)
	lhs := c.regs[rd]
	rhs := c.regs[rs]
	var res uint16

	dropResult := false

	switch bits(op, 6, 4) {
	case 0x0: // AND
		res = lhs & rhs
	case 0x1: // EOR
		res = lhs ^ rhs
	case 0x2: // LSL
		res = lslHelper(c, lhs, rhs)
	case 0x3: // LSR
		res = lsrHelper(c, lhs, rhs)
	case 0x4: // ASR
		res = asrHelper(c, lhs, rhs)
	case 0x5: // ADC
		res = addHelper(c, lhs, rhs, true)
	case 0x6: // SBC
		res = subHelper(c, lhs, rhs, true)
	case 0x7: // ROR
		rhs = rhs % 16 // Reduce it to a single pass.
		big := uint32(lhs)
		big = big | (big << 16)  // Two copies of it.
		res = uint16(big >> rhs) // Slide down and truncate.
	case 0x8: // TST - Bit test.
		res = lhs & rhs
		dropResult = true // Flags only.
	case 0x9: // NEG - Integer negation
		res = -rhs
	case 0xa: // CMP
		res = subHelper(c, lhs, rhs, false)
		dropResult = true
	case 0xb: // CMN
		res = addHelper(c, lhs, rhs, false)
		dropResult = true
	case 0xc: // ORR
		res = lhs | rhs
	case 0xd: // MUL
		c.cycles += 3   // 3 extra, for 4 total.
		res = lhs * rhs // Only sets NZ.
	case 0xe: // BIC - Bit clear
		res = lhs &^ rhs
	case 0xf: // MVN - Negated move
		res = 0xffff ^ rhs
	}

	c.cycles++
	c.setNZ(res)
	if !dropResult {
		c.regs[rd] = res
	}
}

// Format 5: BX and BLX
func format5(c *rq, op uint16) {
	ra := bits(op, 0, 3)
	addr := c.regs[ra]

	if flag(op, 6) { // Link
		*c.lr = *c.pc
	}
	*c.pc = addr
	c.cycles++
}

// Format 6: Hardware
func format6(c *rq, op uint16) {
	rd := bits(op, 0, 3)
	switch bits(op, 5, 2) {
	case 0: // HWN
		c.regs[rd] = uint16(len(c.devices))
	case 1: // HWQ
		dev := c.regs[rd]
		var id, manufacturer uint32
		var version uint16
		if int(dev) < len(c.devices) {
			id, version, manufacturer = c.devices[dev].DeviceDetails()
		} else {
			id, version, manufacturer = 0, 0, 0
		}

		c.regs[1] = uint16(id >> 16) // ID hi
		c.regs[0] = uint16(id)       // ID lo
		c.regs[2] = version
		c.regs[4] = uint16(manufacturer >> 16) // Maker hi
		c.regs[3] = uint16(manufacturer)       // Maker lo
	case 2: // HWI
		dev := c.regs[rd]
		if int(dev) < len(c.devices) {
			c.devices[dev].Interrupt(c)
		}
	default:
		fmt.Printf("Illegal opcode in format 6\n")
	}
	c.cycles += 4
}

// Format 7: Manipulating CPSR
func format7(c *rq, op uint16) {
	switch bits(op, 5, 3) {
	case 0: // RFI
		c.cpsr = c.irqSPSR
		c.regs[0] = c.pop()
		c.setMode()
	case 1: // RSI
		lr := c.swiLR
		c.cpsr = c.swiSPSR
		c.setMode()
		*c.pc = lr
	case 2: // IFS - Enable interrupts.
		c.setFlag(cpsrI, true)
	case 3: // IFC - Disable interrupts.
		c.setFlag(cpsrI, false)
	case 4: // MRS - Move gp register to SPSR for this mode.
		rd := bits(op, 0, 3)
		switch c.cpsr & cpsrMode {
		case cpsrModeUser:
			fmt.Printf("Illegal operation: MRS in User mode\n")
		case cpsrModeIRQ:
			c.irqSPSR = c.regs[rd]
		case cpsrModeSWI:
			c.swiSPSR = c.regs[rd]
		}
	case 5: // MSR - Move SPSR for this mode into gp register.
		rd := bits(op, 0, 3)
		switch c.cpsr & cpsrMode {
		case cpsrModeUser:
			fmt.Printf("Illegal operation: MSR in User mode\n")
		case cpsrModeIRQ:
			c.regs[rd] = c.irqSPSR
		case cpsrModeSWI:
			c.regs[rd] = c.swiSPSR
		}
	default:
		fmt.Printf("Illegal opcode in format 7\n")
	}
	c.cycles++
}

// Format 8: PC-relative loads
func format8(c *rq, op uint16) {
	rd := bits(op, 8, 3)
	imm := bits(op, 0, 8)
	c.regs[rd] = c.mem[*c.pc+imm]
	c.cycles++
}

func loadStoreHelper(c *rq, load, post bool, rd, rb, offset uint16) {
	b := c.regs[rb]

	if load {
		if post {
			c.regs[rd] = c.mem[b]
			c.regs[rb] += offset
		} else {
			c.regs[rd] = c.mem[b+offset]
		}
	} else { // Store
		if post {
			c.mem[b] = c.regs[rd]
			c.regs[rb] += offset
		} else {
			c.mem[b+offset] = c.regs[rd]
		}
	}
}

// Format 9: Load/store with register offset
// Format 10: Long literals
func format9Or10(c *rq, op uint16) {
	if flag(op, 9) { // Format 10, long literal
		c.regs[bits(op, 0, 3)] = c.mem[*c.pc]
		*c.pc++
		return
	}

	loadStoreHelper(c, flag(op, 11), flag(op, 10), bits(op, 0, 3), bits(op, 3, 3),
		c.regs[bits(op, 6, 3)])
	c.cycles++
}

// Format 11: Load/store with immediate offset
func format11(c *rq, op uint16) {
	loadStoreHelper(c, flag(op, 12), flag(op, 11), bits(op, 0, 3), bits(op, 3, 3),
		bits(op, 6, 5))
	c.cycles++
}

// Format 12: Unused
func format12(c *rq, op uint16) {
	fmt.Printf("Illegal operation: Format 12 is undefined.\n")
}

// Format 13: SP-relative load/store
func format13(c *rq, op uint16) {
	rd := bits(op, 8, 3)
	offset := bits(op, 0, 8)
	if flag(op, 11) { // Load
		c.regs[rd] = c.mem[*c.sp+offset]
	} else {
		c.mem[*c.sp+offset] = c.regs[rd]
	}
	c.cycles++
}

// Format 14: Load address
func format14(c *rq, op uint16) {
	var base uint16
	if flag(op, 11) { // Source: 1 = SP
		base = *c.sp
	} else {
		base = *c.pc
	}

	c.regs[bits(op, 8, 3)] = base + bits(op, 0, 8)
	c.cycles++
}

func format15Or16(c *rq, op uint16) {
	if flag(op, 10) {
		format16(c, op)
	} else {
		format15(c, op)
	}
}

// Format 15: Adjust SP
func format15(c *rq, op uint16) {
	imm := bits(op, 0, 7)
	if flag(op, 8) { // Subtract
		*c.sp -= imm
	} else {
		*c.sp += imm
	}
	c.cycles++
}

func countOnes(x uint16) int {
	count := 0
	for i := uint16(0); i < 16; i++ {
		if flag(x, i) {
			count++
		}
	}
	return count
}

// Returns the new base, after the operation is complete.
func multiStore(c *rq, base, rlist uint16) uint16 {
	for i := uint16(0); i < 8; i++ {
		if flag(rlist, i) {
			c.mem[base] = c.regs[i]
			base++
		}
	}
	return base
}

// Returns the new base, after the operation is complete.
func multiLoad(c *rq, base, rlist uint16) uint16 {
	for i := uint16(0); i < 8; i++ {
		if flag(rlist, i) {
			c.regs[i] = c.mem[base]
			base++
		}
	}
	return base
}

// Format 16: Push/pop registers
func format16(c *rq, op uint16) {
	rlist := bits(op, 0, 8)
	withBig := flag(op, 8)
	count := countOnes(rlist)

	if flag(op, 11) { // Pop/Load
		*c.sp = multiLoad(c, *c.sp, rlist)
		if withBig {
			*c.pc = c.pop()
			c.cycles += 2 // 2 extra cycles for popping PC
		}
	} else { // Push/store
		if withBig {
			c.push(*c.lr)
			c.cycles++ // 1 extra cycle for pushing LR
		}
		*c.sp -= uint16(count)
		multiStore(c, *c.sp, rlist)
	}
	c.cycles += count
}

// Multiple load/store
func format17(c *rq, op uint16) {
	rb := bits(op, 8, 3)
	rlist := bits(op, 0, 8)

	if flag(op, 11) { // Load
		c.regs[rb] = multiLoad(c, c.regs[rb], rlist)
	} else {
		c.regs[rb] = multiStore(c, c.regs[rb], rlist)
	}
	c.cycles += countOnes(rlist)
}

// Format 18: Conditional branches
// Format 19: SWI (condition code $f)
func format18Or19(c *rq, op uint16) {
	offset := signExtend(8, bits(op, 0, 8))
	fmt.Printf("Conditional branch: original %02x -> %04x = %d\n", bits(op, 0, 8),
		uint16(offset), int(offset))

	n := c.getFlag(cpsrN)
	z := c.getFlag(cpsrZ)
	r := c.getFlag(cpsrC)
	v := c.getFlag(cpsrV)

	jump := false
	switch bits(op, 8, 4) {
	case 0x0: // BEQ - Z set
		jump = z
	case 0x1: // BNE - Z clear
		jump = !z
	case 0x2: // BCS - C set (unsigned higher or same)
		jump = r
	case 0x3: // BCC - C clear (unsigned lower)
		jump = !r
	case 0x4: // BMI - N set (negative)
		jump = n
	case 0x5: // BPL - C clear (positive or zero)
		jump = !n
	case 0x6: // BVS - V set (overflow)
		jump = v
	case 0x7: // BVC - V clear (no overflow)
		jump = !v
	case 0x8: // BHI - C set and Z clear (unsigned higher)
		jump = r && !z
	case 0x9: // BLS - C clear or Z set (unsigned lower or equal)
		jump = !r || z
	case 0xa: // BGE - N and V match (signed greater or equal)
		jump = n == v
	case 0xb: // BLT - N and V differ (signed less than)
		jump = n != v
	case 0xc: // BGT - Z clear, and N and V match (signed greater than)
		jump = !z && (n == v)
	case 0xd: // BLE - Z set, or N and V differ (signed less than or equal)
		jump = z || (n != v)

	case 0xf: // Software interrupt
		c.swiSPSR = c.cpsr
		c.cpsr = cpsrModeSWI // Disables interrupts, clears flags.
		c.setMode()
		*c.lr = *c.pc // Saves the shared PC in the swiLR.
		*c.pc = vectorSWI
		return

	default:
		fmt.Printf("Illegal operation: Bad conditional branch\n")
	}

	if jump {
		*c.pc += uint16(offset)
		c.cycles++
	} else {
		c.cycles += 2
	}
}

// Format 20: Unconditional branch
func format20(c *rq, op uint16) {
	offset := signExtend(11, bits(op, 0, 11))
	*c.pc += uint16(offset)
	c.cycles++
}

// Format 21: Immediate branch and link
// Format 22: Long branch and link
func format21Or22(c *rq, op uint16) {
	if flag(op, 11) { // Format 21, just one instruction.
		*c.lr = *c.pc
		*c.pc = bits(op, 0, 11)
		c.cycles++
		return
	}

	// Format 22: Long form branch-and-link
	if flag(op, 10) { // H = 1, set the high bits.
		*c.lr = bits(op, 0, 8) << 8
	} else {
		addr := *c.lr | bits(op, 0, 8)
		*c.lr = *c.pc
		*c.pc = addr
	}
	c.cycles++
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
		c.irqSPSR = c.cpsr
		c.cpsr = cpsrModeIRQ // Flags clear, interrupts disabled, IRQ mode.
		c.setMode()
		c.push(c.regs[0])
		c.regs[0] = msg
		*c.pc = vectorIRQ
	}

	x := c.pcGet()
	formatHandlers[bits(x, 12, 4)](c, x)
	return true
}

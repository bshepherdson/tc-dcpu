package dcpu

import (
	"fmt"
	"os"

	"github.com/shepheb/tc-dcpu/common"
)

type dcpu struct {
	pc          uint16
	ia          uint16
	ex          uint16
	sp          uint16
	skipping    bool
	queueing    bool
	blocked     bool
	halted      bool
	debug       bool
	cycles      int
	regs        [8]uint16   // General-purpose registers: A B C X Y Z I J
	ints        [256]uint16 // Pending interrupts.
	intCount    int
	devices     []common.Device
	breakpoints []uint16
	mem         [65536]uint16
}

const ra uint16 = 0
const rb uint16 = 1
const rc uint16 = 2
const rx uint16 = 3
const ry uint16 = 4
const rz uint16 = 5
const ri uint16 = 6
const rj uint16 = 7

// Implement the CPU interface.
func (d *dcpu) Memory() []uint16 {
	return d.mem[0:65536]
}
func (d *dcpu) ReadReg(r uint16) uint16 {
	return d.regs[r]
}
func (d *dcpu) WriteReg(r, val uint16) {
	d.regs[r] = val
}
func (d *dcpu) AddInterrupt(msg uint16) {
	d.addInterrupt(msg)
}
func (d *dcpu) AddBreakpoint(at uint32) {
	d.breakpoints = append(d.breakpoints, uint16(at))
}
func (d *dcpu) AddDevice(dev common.Device) {
	d.devices = append(d.devices, dev)
}
func (d *dcpu) Devices() []common.Device {
	return d.devices
}
func (d *dcpu) Disassemble() {
	disasmROM(d.Memory())
}
func (d *dcpu) DisassembleOp(at uint32) uint16 {
	return uint16(disasmOp(d.mem[:], uint16(at), d.mem[at]))
}
func (d *dcpu) Debugging() *bool {
	return &d.debug
}

func (d *dcpu) Speed() int {
	return 100000
}

func (d *dcpu) HardwareDelay(cycles int) {
	if d.cycles < cycles {
		d.cycles = cycles
	}
}

func (d *dcpu) DebugPrompt() {
	fmt.Printf("%04x debug> ", d.pc)
}

func (d *dcpu) Exit() {
	os.Exit(0)
}

// NewDCPU returns a freshly created DCPU instance.
func NewDCPU() common.CPU {
	return new(dcpu)
}

// Implementation details.
func (d *dcpu) pcGet() uint16 {
	v := d.mem[d.pc]
	d.pc++
	return v
}

func (d *dcpu) pcPeek() uint16 {
	return d.mem[d.pc]
}

func (d *dcpu) pop() uint16 {
	v := d.mem[d.sp]
	d.sp++
	return v
}

func (d *dcpu) push(v uint16) {
	d.sp--
	d.mem[d.sp] = v
}

func (d *dcpu) addInterrupt(v uint16) {
	if d.intCount >= 256 {
		os.Exit(4)
	}

	d.ints[d.intCount] = v
	d.intCount++
}

func (d *dcpu) popInterrupt() uint16 {
	msg := d.ints[0]
	for i := 1; i < d.intCount; i++ {
		d.ints[i-1] = d.ints[i]
	}
	d.intCount--
	return msg
}

func (d *dcpu) readArg(arg uint16, consume bool) uint16 {
	if arg < 0x08 {
		return d.regs[arg]
	}
	if arg < 0x10 {
		return d.mem[d.regs[arg&0x7]]
	}
	if arg < 0x18 {
		d.cycles++
		if consume {
			return d.mem[d.regs[arg&0x7]+d.pcGet()]
		}
		return d.mem[d.regs[arg&0x7]+d.pcPeek()]
	}

	if arg >= 0x20 { // Inline literal.
		return arg - 0x21
	}

	switch arg {
	case 0x18: // POP
		return d.pop()
	case 0x19: // PEEK
		return d.mem[d.sp]
	case 0x1a: // PICK n
		d.cycles++
		if consume {
			return d.mem[d.sp+d.pcGet()]
		}
		return d.mem[d.sp+d.pcPeek()]
	case 0x1b:
		return d.sp
	case 0x1c:
		return d.pc
	case 0x1d:
		return d.ex
	case 0x1e:
		d.cycles++
		if consume {
			return d.mem[d.pcGet()]
		}
		return d.mem[d.pcPeek()]
	case 0x1f:
		d.cycles++
		if consume {
			return d.pcGet()
		}
		return d.pcPeek()
	}

	// Can't happen
	os.Exit(2)
	return 0
}

// Skips over it without really performing it.
func (d *dcpu) skipArg(a uint16) {
	//  [A+lit]                   PICK n       [lit]        lit
	if (0x10 <= a && a < 0x18) || a == 0x1a || a == 0x1e || a == 0x1f {
		d.pc++
	}
}

func (d *dcpu) readArgs(a, b uint16) (uint16, uint16) {
	av := d.readArg(a, true)
	bv := d.readArg(b, false)
	return av, bv
}

func (d *dcpu) writeArg(arg uint16, val uint16) {
	if arg < 0x08 {
		d.regs[arg] = val
	} else if arg < 0x10 {
		d.mem[d.regs[arg&7]] = val
	} else if arg < 0x18 {
		d.mem[d.regs[arg&7]+d.pcGet()] = val
		d.cycles++
	} else if arg == 0x18 {
		d.push(val)
	} else if arg == 0x19 {
		d.mem[d.sp] = val
	} else if arg == 0x1a {
		d.mem[d.sp+d.pcGet()] = val
		d.cycles++
	} else if arg == 0x1b {
		d.sp = val
	} else if arg == 0x1c {
		d.pc = val
	} else if arg == 0x1d {
		d.ex = val
	} else if arg == 0x1e {
		d.mem[d.pcGet()] = val
		d.cycles++
	}
	// Otherwise, silently dropped.
}

// 2-argument instructions
func (d *dcpu) runMainOp(op, a, b uint16) {
	if 0x10 <= op && op < 0x18 {
		// Branching opcodes all have the same flavour.
		var branch bool // true means run the next op! false means skip
		av := d.readArg(a, true)
		bv := d.readArg(b, true)
		switch op {
		case 0x10: // IFB - branch with bits in common
			branch = av&bv != 0
		case 0x11: // IFC - branch with no bits in common
			branch = av&bv == 0
		case 0x12: // IFE - branch if equal
			branch = av == bv
		case 0x13: // IFN - branch if not equal
			branch = av != bv
		case 0x14: // IFG - branch if b > a
			branch = bv > av
		case 0x15: // IFA - branch if b > a, signed
			branch = int16(bv) > int16(av)
		case 0x16: // IFL - branch if b < a
			branch = bv < av
		case 0x17: // IFU - branch if b < a, signed
			branch = int16(bv) < int16(av)
		}

		d.skipping = !branch
		d.cycles += 2
		if d.skipping { // Extra cycle on a skip.
			d.cycles++
		}
	}

	switch op {
	case 0x00:
		d.runSpecialOp(b, a)

	case 0x01: // SET b, a
		d.writeArg(b, d.readArg(a, true))
		d.cycles++

	case 0x02: // ADD b, a
		av, bv := d.readArgs(a, b)
		res32 := uint32(av) + uint32(bv)
		d.writeArg(b, uint16(res32&0xffff))
		d.ex = 0
		if res32 >= 0x10000 {
			d.ex = 1
		}
		d.cycles += 2

	case 0x03: // SUB b, a  - b=b-a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, bv-av)
		d.ex = 0
		if av > bv {
			d.ex = 0xffff
		}
		d.cycles += 2

	case 0x04: // MUL b, a
		av, bv := d.readArgs(a, b)
		res32 := uint32(av) * uint32(bv)
		d.writeArg(b, uint16(res32&0xffff))
		d.ex = uint16(res32 >> 16)
		d.cycles += 2

	case 0x05: // MLI b, a
		av, bv := d.readArgs(a, b)
		res32 := int32(int16(av)) * int32(int16(bv))
		d.writeArg(b, uint16(res32&0xffff))
		d.ex = uint16(res32 >> 16)
		d.cycles += 2

	case 0x06: // DIV b, a    (b = b/a, or 0)
		av, bv := d.readArgs(a, b)
		res := uint16(0)
		ex := uint16(0)
		if av != 0 {
			res = bv / av
			ex = uint16((uint32(bv) << 16) / uint32(av))
		}
		d.writeArg(b, res)
		d.ex = ex
		d.cycles += 3

	case 0x07: // DVI b, a    (b = b/a, or 0)
		av, bv := d.readArgs(a, b)
		res := int16(0)
		ex := uint16(0)
		if av != 0 {
			res = int16(bv) / int16(av)
			ex = uint16((int32(int16(bv)) << 16) / int32(int16(av)))
		}
		d.writeArg(b, uint16(res))
		d.ex = ex
		d.cycles += 3

	case 0x08: // MOD b, a
		av, bv := d.readArgs(a, b)
		res := uint16(0)
		if av != 0 {
			res = bv % av
		}
		d.writeArg(b, res)
		d.cycles += 3

	case 0x09: // MDI b, a
		av, bv := d.readArgs(a, b)
		res := int16(0)
		if av != 0 {
			res = int16(bv) % int16(av)
		}
		d.writeArg(b, uint16(res))
		d.cycles += 3

	case 0x0a: // AND b, a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, av&bv)
		d.cycles++
	case 0x0b: // BOR b, a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, av|bv)
		d.cycles++
	case 0x0c: // XOR b, a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, av^bv)
		d.cycles++

	case 0x0d: // SHR b, a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, bv>>av)
		d.ex = uint16((uint32(bv) << 16) >> av)
		d.cycles++
	case 0x0e: // ASR b, a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, uint16(int16(bv)>>av))
		d.ex = uint16((int32(int16(bv)) << 16) >> av)
		d.cycles++
	case 0x0f: // SHL b, a
		av, bv := d.readArgs(a, b)
		d.writeArg(b, bv<<av)
		d.ex = uint16((uint32(bv) << av) >> 16)
		d.cycles++

	case 0x1a: // ADX b, a
		av, bv := d.readArgs(a, b)
		res32 := uint32(av) + uint32(bv) + uint32(d.ex)
		d.writeArg(b, uint16(res32))
		d.ex = 0
		if res32&0xffff0000 != 0 {
			d.ex = 1
		}
		d.cycles += 3

	case 0x1b: // SBX b, a
		av, bv := d.readArgs(a, b)
		oldEX := d.ex
		d.writeArg(b, bv-av+oldEX)
		d.ex = 0
		if bv < av+d.ex {
			d.ex = 0xffff
		}
		d.cycles += 3

	case 0x1e: // STI b, a
		av := d.readArg(a, true)
		d.writeArg(b, av)
		d.regs[ri]++
		d.regs[rj]++
		d.cycles += 2
	case 0x1f: // STI b, a
		av := d.readArg(a, true)
		d.writeArg(b, av)
		d.regs[ri]--
		d.regs[rj]--
		d.cycles += 2
	}
}

func (d *dcpu) runSpecialOp(op, a uint16) {
	switch op {
	case 0x01: // JSR a
		av := d.readArg(a, true)
		d.push(d.pc)
		d.pc = av
		d.cycles += 3

	case 0x08: // INT a - software interrupt
		d.addInterrupt(d.readArg(a, true))
		d.cycles += 4

	case 0x09: // IAG a - store IA in a
		d.writeArg(a, d.ia)
		d.cycles++
	case 0x0a: // IAS a - store a into IA
		d.ia = d.readArg(a, true)
		d.cycles++

	case 0x0b: // RFI a - disable interrupt queueing. a = pop, pc = pop. A ignored.
		d.readArg(a, true) // But we need to consume A anyway.
		d.regs[ra] = d.pop()
		d.pc = d.pop()
		d.queueing = false
		d.cycles += 3

	case 0x0c: // IAQ a - if a != 0, interrupt queueing is on. a == 0, queue off
		av := d.readArg(a, true)
		d.queueing = av != 0
		d.cycles += 2

	case 0x10: // HWN a - a = number of connected devices
		d.writeArg(a, uint16(len(d.devices)))
		d.cycles += 2

	case 0x11: // HWQ a - Hardware details for device I.
		av := d.readArg(a, true)
		if int(av) >= len(d.devices) {
			for i := 0; i < 5; i++ {
				d.regs[i] = 0
			}
		} else {
			id, version, manufacturer := d.devices[av].DeviceDetails()
			d.regs[ra] = uint16(id)
			d.regs[rb] = uint16(id >> 16)
			d.regs[rc] = version
			d.regs[rx] = uint16(manufacturer)
			d.regs[ry] = uint16(manufacturer >> 16)
		}
		d.cycles += 4

	case 0x12: // HWI a - send interrupt to device a
		av := d.readArg(a, true)
		if int(av) < len(d.devices) {
			d.devices[av].Interrupt(d)
		} // Silently do nothing if there's no such device.
		d.cycles += 4

	case 0x13: // LOG a
		av := d.readArg(a, true)
		d.cycles++
		fmt.Printf("Log: 0x%04x %d %c\n", av, int16(av), av)
	case 0x14: // BRK a
		fmt.Printf("Hit breakpoint %d\n", d.readArg(a, true))
		d.debug = true
		d.cycles++
	case 0x15: // HLT a
		d.halted = true
	}
}

// Runs a single cycle. That might mean doing nothing,
// Returns true if an instruction was executed, false if not.
func (d *dcpu) RunOp() bool {
	// Tick the hardware devices.
	for _, dev := range d.devices {
		dev.Tick(d)
	}

	if d.blocked {
		return false
	}

	if d.cycles > 1 {
		d.cycles--
		return false
	}

	d.cycles = 0

	// If we're skipping, check whether this is a branching opcode or not.
	if d.skipping {
		// Opcode is aaaaaabbbbbooooo
		x := d.pcGet()
		op := x & 31
		b := (x >> 5) & 31
		a := (x >> 10) & 63

		// Not a branch instruction, so skip over the instruction and its arguments.
		if op != 0 { // If op == 0, then there's no b to be skipped.
			d.skipArg(b)
		}
		d.skipArg(a)

		if 0x10 <= op && op < 0x18 { // Branching
			// Keep skipping at the cost of a cycle.
			d.cycles++
			return false
		}

		// PC should now be pointed at the next instruction to run.
		d.skipping = false
		// We're done skipping, let the below run the following instruction.
	}

	// Check for interrupts before running the operation.
	if !d.queueing && d.intCount > 0 {
		// TODO: Use a circular buffer or something to stop this being so costly.
		msg := d.popInterrupt()
		if d.ia != 0 {
			d.push(d.pc)
			d.push(d.regs[ra])
			d.pc = d.ia
			d.regs[ra] = msg
		}
		d.halted = false
	}

	if d.halted {
		return false
	}

	// Opcode is aaaaaabbbbbooooo
	x := d.pcGet()
	op := x & 31
	b := (x >> 5) & 31
	a := (x >> 10) & 63

	// Not skipping, we're really running this one.
	d.runMainOp(op, a, b)
	return true
}

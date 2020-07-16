package mocha

import (
	"fmt"
	"os"

	"github.com/shepheb/tc-dcpu/common"
)

type m86k struct {
	pc uint32
	sp uint32
	ex uint32
	ia uint16

	halted   bool
	debug    bool
	skipping bool
	queueing bool
	blocked  bool
	cycles   int

	regs     [8]uint32
	ints     [256]uint16
	intCount int

	devices     []common.Device
	breakpoints []uint32
	mem         [16 * 1024 * 1024]uint16
}

func (c *m86k) Memory() []uint16 {
	return c.mem[:]
}

// Reads the low word, for use by devices.
func (c *m86k) ReadReg(r uint16) uint16 {
	return uint16(c.regs[r])
}

func (c *m86k) WriteReg(r, val uint16) {
	c.regs[r] = writeLow(c.regs[r], val)
}

func (c *m86k) AddInterrupt(msg uint16) {
	if c.intCount >= 256 {
		os.Exit(4)
	}

	c.ints[c.intCount] = msg
	c.intCount++
}

func (c *m86k) popInterrupt() uint16 {
	ret := c.ints[0]
	for i := 1; i < c.intCount; i++ {
		c.ints[i-1] = c.ints[i]
	}
	c.intCount--
	return ret
}

func (c *m86k) AddDevice(dev common.Device) {
	c.devices = append(c.devices, dev)
}

func (c *m86k) Devices() []common.Device {
	return c.devices
}

func (c *m86k) Speed() int {
	return 2000000 // 2MHz
}

func (c *m86k) AddBreakpoint(at uint32) {
	c.breakpoints = append(c.breakpoints, at)
}

func (c *m86k) Debugging() *bool {
	return &c.debug
}

func (c *m86k) Disassemble() {
	last := 0
	for i, w := range c.mem {
		if w != 0 {
			last = i
		}
	}

	fmt.Printf("Last: $%x\n", last)

	for i := 0; i <= last; {
		i += int(c.DisassembleOp(uint32(i)))
	}
}

func (c *m86k) DebugPrompt() {
	fmt.Printf("%08x debug> ", c.pc)
}

func (c *m86k) Exit() {
	// Clean up devices first.
	for _, d := range c.devices {
		d.Cleanup()
	}
	os.Exit(0)
}

func (c *m86k) HardwareDelay(cycles int) {
	if c.cycles < cycles {
		c.cycles = cycles
	}
}

func NewMocha86k() common.CPU {
	return new(m86k)
}

func writeLow(long uint32, word uint16) uint32 {
	return (0xffff0000 & long) | uint32(word)
}

func (c *m86k) readWord(addr uint32) uint16 {
	if int(addr) >= len(c.mem) {
		fmt.Printf("WARN: Out-of-bounds read at %08x before PC %08x\n", addr, c.pc)
		return 0
	}
	return c.mem[addr]
}

func (c *m86k) writeWord(addr uint32, word uint16) {
	if int(addr) >= len(c.mem) {
		fmt.Printf("WARN: Out-of-bounds write at %08x before PC %08x\n", addr, c.pc)
	} else {
		c.mem[addr] = word
	}
}

func (c *m86k) consumeWord() uint16 {
	val := c.readWord(c.pc)
	c.pc++
	return val
}

// Runs a single cycle, and returns false if nothing happened.
func (c *m86k) RunOp() bool {
	// Tick the hardware devices.
	for _, dev := range c.devices {
		dev.Tick(c)
	}

	if c.blocked {
		return false
	}

	if c.cycles > 1 {
		c.cycles--
		return false
	}

	c.cycles = 0

	// If we're skipping, check whether this is a branching opcode or not.
	if c.skipping {
		x := c.consumeWord()
		isUnary := x&0x7c00 == 0x0800
		isBinary := x&0x7000 == 0x7000
		nextWord := c.mem[c.pc] // Peek without moving PC yet.
		isBinary = isBinary && nextWord&0x18 == 0x10

		// Whether it's a branch or not, we need to skip over it.
		c.skipInstruction(x)

		// Not a branch instruction, so skip over the instruction and its arguments.
		if isUnary || isBinary { // If op == 0, then there's no b to be skipped.
			c.cycles++ // Costs an extra cycle to skip thus.
			return false
		}

		// If we're still here, then we just skipped a real instruction, not a
		// branching one, so stop skipping.
		// PC should now be pointed at the next instruction to run.
		c.skipping = false
		// We're done skipping, let the below run the following instruction.
	}

	// Check for interrupts before running the operation.
	if !c.queueing && c.intCount > 0 {
		// TODO: Use a circular buffer or something to stop this being so costly.
		msg := c.popInterrupt()
		if c.ia != 0 {
			// Enter the interrupt handler.
			c.runMC([]mc{
				mcGetPC, mcPushLongword, // Push PC
				mcLit(0), mcReadReg32, mcPushLongword, // Push A
				mcLit16(msg), mcLit(0), mcWriteReg32, // Write msg to A
				mcLit16(c.ia), mcPutPC, // PC := IA
			})
			c.queueing = true
		}
		c.halted = false
	}

	if c.halted {
		return false
	}

	// Opcode is aaaaaabbbbbooooo
	//oldPC := c.pc
	x := c.consumeWord()
	//fmt.Printf("Run: %08x (%04x)\n", oldPC, x)
	handlerFor(x).run(c, x)
	return true
}

type encoding struct {
	mask    uint16
	value   uint16
	handler opHandler
}

type opHandler interface {
	run(c *m86k, opcode uint16)
	skip(c *m86k, opcode uint16)
	disassemble(d *disState, opcode uint16)
}

var handlers = []*encoding{
	&encoding{mask: 0x7fc0, value: 0x0000, handler: handleNullary},
	&encoding{mask: 0x7800, value: 0x0000, handler: handleUnary},
	&encoding{mask: 0x7800, value: 0x0800, handler: handleUnaryBranch},
	&encoding{mask: 0x0000, value: 0x0000, handler: handleBinary},
}

func handlerFor(opcode uint16) opHandler {
	for _, h := range handlers {
		if (h.mask & opcode) == h.value {
			return h.handler
		}
	}
	panic("can't find handler")
}

func (c *m86k) skipInstruction(opcode uint16) {
	handlerFor(opcode).skip(c, opcode)
}

func (c *m86k) skipOperand(operand uint16) {
	width := operandWidth(operand & 0x3f)
	c.pc += width
}

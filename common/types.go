package common

import "bufio"

// CPU is the generic interface to the CPUs, used by the hardware to abstract
// across the CPUs.
type CPU interface {
	Memory() []uint16
	ReadReg(r uint16) uint16
	WriteReg(r, val uint16)
	AddInterrupt(msg uint16)
	AddDevice(Device)
	AddBreakpoint(at uint16)
	Debugging() *bool
	DebugPrompt()
	RegByName(name string) (uint16, string, bool)
	Registers() []string

	RunOp() bool // Runs a single cycle, returning false when nothing happened.
	Disassemble()
	DisassembleOp(at uint16) uint16 // Returns length in instructions.
	Exit()
}

// Device is the interface to all hardware.
type Device interface {
	// Returns the device ID, version and manufacturer.
	DeviceDetails() (uint32, uint16, uint32)
	Interrupt(CPU)
	Tick(CPU)
	Cleanup()
}

// InputReader is shared by the inputs, since os.Stdin is global.
var InputReader *bufio.Reader

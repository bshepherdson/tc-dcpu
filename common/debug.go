package common

import (
	"fmt"
	"os"
)

// DebugCommand captures a self-describing debug command.
type DebugCommand interface {
	Describe() string
	Run(c CPU, args []string)
}

type debugBlob struct {
	desc string
	f    func(CPU, []string)
}

// DebugCommands is a map of command strings to command objects.
var DebugCommands = map[string]DebugCommand{
	"r": newCommand("Dump one or all (r)egisters ('r' vs. 'r <reg>')", cmdRegs),
	"q": newCommand("(Q)uit the emulator", func(CPU, []string) { os.Exit(0) }),

	"c": newCommand("(C)ontinue execution", func(c CPU, s []string) {
		*c.Debugging() = false
	}),

	"s": newCommand("(S)tep forward, run next instruction", func(c CPU, args []string) {
		for !c.RunOp() {
		}
	}),

	"b": newCommand("Set a new (b)reakpoint at the given (hex) location",
		singleHexArg("No breakpoint location specified (needs hex number)",
			"Error parsing the location", func(c CPU, loc uint32) {
				c.AddBreakpoint(loc)
				fmt.Printf("Breakpoint set at PC = %04x\n", loc)
			})),
	"m": newCommand("Print a value from (m)emory",
		singleHexArg("No memory location specified", "Error parsing location",
			func(c CPU, loc uint32) {
				x := c.Memory()[loc]
				fmt.Printf("[%04x] = %04x (%d, '%c')\n", loc, x, int16(x), rune(x))
			})),

	"i": newCommand("Disassemble the (i)nstruction at the given location, or at PC",
		singleHexArg("No PC value given", "Error parsing location",
			func(c CPU, loc uint32) {
				for i := loc; i < loc+16; {
					i += uint32(c.DisassembleOp(i))
				}
			})),

	"db": newCommand("(D)ump memory to the given file in (b)inary (big-endian), ",
		func(c CPU, args []string) {
			if len(args) < 2 {
				fmt.Println("No filename given")
				return
			}

			f, err := os.Create(args[1])
			if err != nil {
				fmt.Printf("Could not open file: %v\n", err)
				return
			}

			mem := c.Memory()
			buf := make([]byte, 0x20000)
			for i := 0; i < 0x10000; i++ {
				w := mem[i]
				buf[i*2] = byte(w >> 8)
				buf[i*2+1] = byte(w)
			}

			f.Write(buf)
			f.Close()
		}),
}

func newCommand(desc string, f func(c CPU, args []string)) DebugCommand {
	d := new(debugBlob)
	d.desc = desc
	d.f = f
	return d
}

func (dbg *debugBlob) Describe() string {
	return dbg.desc
}

func (dbg *debugBlob) Run(c CPU, args []string) {
	dbg.f(c, args)
}

var regNames = map[string]uint16{
	"r0": 0,
	"r1": 1,
	"r2": 2,
	"r3": 3,
	"r4": 4,
	"r5": 5,
	"r6": 6,
	"r7": 7,
}

// Indexed by register width in bits.
var regLines = map[int]string{
	16: "%2s      %04x (%d)\t[%s]      %04x (%d)\n",
	32: "%2s  %08x (%d)\t[%s]  %08x (%d)\n",
}

func showReg(c CPU, name string, val uint32) {
	mem := c.Memory()
	var memval uint16
	if int(val) < len(mem) {
		memval = mem[val]
	}
	fmt.Printf(regLines[c.RegisterWidth(name)], name, val, int32(val),
		name, memval, int16(memval))
}

func cmdRegs(c CPU, args []string) {
	if len(args) > 1 {
		for _, r := range args[1:] {
			value, name, ok := c.RegByName(r)
			if ok {
				showReg(c, name, value)
			} else {
				fmt.Printf("%% Unknown register: %s\n", r)
			}
		}
	} else {
		for _, r := range c.Registers() {
			value, name, _ := c.RegByName(r)
			showReg(c, name, value)
		}
	}
}

func singleHexArg(notSpecifiedMsg, parseErrorMsg string,
	cmd func(c CPU, arg uint32)) func(CPU, []string) {
	return func(c CPU, args []string) {
		if len(args) <= 1 {
			fmt.Println(notSpecifiedMsg)
			return
		}

		var x uint32
		_, err := fmt.Sscanf(args[1], "%x", &x)
		if err != nil {
			fmt.Printf(parseErrorMsg+": %v\n", err)
			return
		}

		cmd(c, x)
	}
}

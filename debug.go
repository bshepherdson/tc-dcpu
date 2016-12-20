package main

import (
	"fmt"
	"os"
	"strings"
)

// DebugCommand captures a self-describing debug command.
type DebugCommand interface {
	Describe() string
	Run(d *dcpu, args []string)
}

type debugBlob struct {
	desc string
	f    func(*dcpu, []string)
}

var debugCommands = map[string]DebugCommand{
	"r": newCommand("Dump one or all (r)egisters ('r' vs. 'r c')", cmdRegs),
	"q": newCommand("(Q)uit the emulator", func(*dcpu, []string) { os.Exit(0) }),

	"c": newCommand("(C)ontinue execution", func(d *dcpu, s []string) {
		d.debug = false
	}),

	"s": newCommand("(S)tep forward, run next instruction", func(d *dcpu, args []string) {
		for !d.runOp() {
		}
	}),

	"b": newCommand("Set a new (b)reakpoint at the given (hex) location",
		singleHexArg("No breakpoint location specified (needs hex number)",
			"Error parsing the location", func(d *dcpu, loc uint16) {
				d.breakpoints = append(d.breakpoints, loc)
				fmt.Printf("Breakpoint set at PC = %04x\n", loc)
			})),
	"m": newCommand("Print a value from (m)emory",
		singleHexArg("No memory location specified", "Error parsing location",
			func(d *dcpu, loc uint16) {
				x := d.mem[loc]
				fmt.Printf("[%04x] = %04x (%d, '%c')\n", loc, x, int16(x), rune(x))
			})),

	"i": newCommand("Disassemble the (i)nstruction at the given location, or at PC",
		singleHexArg("No PC value given", "Error parsing location",
			func(d *dcpu, loc uint16) {
				for i := loc; i < loc+16; {
					i += uint16(disasmOp(d.mem[:], i, d.mem[i]))
				}
			})),
}

func newCommand(desc string, f func(*dcpu, []string)) DebugCommand {
	d := new(debugBlob)
	d.desc = desc
	d.f = f
	return d
}

func (dbg *debugBlob) Describe() string {
	return dbg.desc
}

func (dbg *debugBlob) Run(d *dcpu, args []string) {
	dbg.f(d, args)
}

var regNames = map[string]uint16{
	"A": ra,
	"a": ra,
	"B": rb,
	"b": rb,
	"C": rc,
	"c": rc,
	"X": rx,
	"x": rx,
	"Y": ry,
	"y": ry,
	"Z": rz,
	"z": rz,
	"I": ri,
	"i": ri,
	"J": rj,
	"j": rj,
}

func showReg(d *dcpu, name string, val uint16) {
	fmt.Printf("%2s  %04x (%d)\t[%s]  %04x (%d)\n", name, val, int16(val),
		name, d.mem[val], int16(d.mem[val]))
}

func cmdRegs(d *dcpu, args []string) {
	if len(args) > 1 {
		for _, r := range args[1:] {
			if reg, ok := regNames[r]; ok {
				showReg(d, strings.ToUpper(r), d.regs[reg])
			} else if r == "pc" || r == "PC" {
				showReg(d, "PC", d.pc)
			} else if r == "ex" || r == "EX" {
				showReg(d, "EX", d.ex)
			} else if r == "sp" || r == "SP" {
				showReg(d, "SP", d.sp)
			} else {
				fmt.Printf("%% Unknown register: %s\n", r)
			}
		}
	} else {
		showReg(d, "A", d.regs[ra])
		showReg(d, "B", d.regs[rb])
		showReg(d, "C", d.regs[rc])
		showReg(d, "X", d.regs[rx])
		showReg(d, "Y", d.regs[ry])
		showReg(d, "Z", d.regs[rz])
		showReg(d, "I", d.regs[ri])
		showReg(d, "J", d.regs[rj])
		showReg(d, "SP", d.sp)
		showReg(d, "EX", d.ex)
		showReg(d, "PC", d.pc)
	}
}

func singleHexArg(notSpecifiedMsg, parseErrorMsg string,
	cmd func(d *dcpu, arg uint16)) func(*dcpu, []string) {
	return func(d *dcpu, args []string) {
		if len(args) <= 1 {
			fmt.Println(notSpecifiedMsg)
			return
		}

		var x uint16
		_, err := fmt.Sscanf(args[1], "%x", &x)
		if err != nil {
			fmt.Printf(parseErrorMsg+": %v\n", err)
			return
		}

		cmd(d, x)
	}
}

func cmdBreak(d *dcpu, args []string) {
	if len(args) <= 1 {
		fmt.Printf("No breakpoint location specified (needs hex number)\n")
		return
	}

	var addr *uint16
	_, err := fmt.Sscanf(args[1], "%x", addr)
	if err != nil {
		fmt.Printf("Error parsing the location: %v\n", err)
		return
	}

	d.breakpoints = append(d.breakpoints, *addr)
	fmt.Printf("Breakpoint set at PC = %04x\n", *addr)
}

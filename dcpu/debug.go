package dcpu

import "strings"

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

var registers = []string{"A", "B", "C", "X", "Y", "Z", "I", "J", "SP", "EX",
	"IA", "PC"}

func (d *dcpu) Registers() []string {
	return registers
}

func (d *dcpu) RegByName(name string) (uint32, string, bool) {
	if r, ok := regNames[name]; ok {
		return uint32(d.regs[r]), strings.ToUpper(name), true
	}

	if name == "pc" || name == "PC" {
		return uint32(d.pc), "PC", true
	} else if name == "sp" || name == "SP" {
		return uint32(d.sp), "SP", true
	} else if name == "ex" || name == "EX" {
		return uint32(d.ex), "EX", true
	} else if name == "ia" || name == "IA" {
		return uint32(d.ia), "IA", true
	} else {
		return 0, "", false
	}
}

func (d *dcpu) RegisterWidth(name string) int {
	return 16
}

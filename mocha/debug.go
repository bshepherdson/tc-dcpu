package mocha

import "strings"

var regNames = map[string]uint16{
	"A": 0,
	"B": 1,
	"C": 2,
	"X": 3,
	"Y": 4,
	"Z": 5,
	"I": 6,
	"J": 7,
}

var registers = []string{"A", "B", "C", "X", "Y", "Z", "I", "J"}

func (c *m86k) Registers() []string {
	return registers
}

func (c *m86k) RegByName(name string) (uint32, string, bool) {
	uc := strings.ToUpper(name)
	if r, ok := regNames[uc]; ok {
		return c.regs[r], uc, true
	}

	if uc == "PC" {
		return c.pc, uc, true
	}
	if uc == "EX" {
		return c.ex, uc, true
	}
	if uc == "SP" {
		return c.sp, uc, true
	}
	if uc == "IA" {
		return uint32(c.pc), uc, true
	}

	return 0, "", false
}

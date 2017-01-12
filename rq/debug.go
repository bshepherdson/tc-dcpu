package rq

import "strings"

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

var registers = []string{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "pc", "sp", "lr", "cpsr"}

func (c *rq) Registers() []string {
	return registers
}

func (c *rq) RegByName(name string) (uint16, string, bool) {
	if r, ok := regNames[name]; ok {
		return c.regs[r], strings.ToLower(name), true
	}

	if name == "pc" || name == "PC" {
		return *c.pc, "pc", true
	} else if name == "sp" || name == "SP" {
		return *c.sp, "sp", true
	} else if name == "lr" || name == "LR" {
		return *c.lr, "lr", true
	} else if name == "cpsr" || name == "CPSR" {
		return c.cpsr, "cpsr", true
	} else {
		return 0, "", false
	}
}

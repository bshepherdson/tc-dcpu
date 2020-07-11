package mocha

import "fmt"

// Overall branch design deserves thinking about.
// We want to decouple the comparisons and the handling of the result.
// The comparsions can consume 1 or 2 arguments and emit 0 or 1.
// Then we can skip and branch accordingly.

type unaryBranch int

var handleUnaryBranch = unaryBranch(0)

func (unaryBranch) run(c *m86k, opcode uint16) {
	longwords := opcode&0x8000 != 0
	decrement := opcode&0x100 != 0

	delta := uint32(int32(int16(uint16(c.runMC([]mc{mcConsumePCWord})))))
	opMC := decodeOperand(opcode&0x3f, longwords)

	// Branch: read op, check condition, push delta, branch-if
	// Branch Dec: read op, dup, dec, write, check condition, push delta, branch-if
	// That decomposes into: read, maybe-dec, check condition, handle condition
	var decBlock microThread // Default to empty
	if decrement {
		decBlock = append([]mc{mcDup, mcLit(1), mcMinus}, opMC.write...)
	}

	conditionBlock := map[uint16]mc{
		0: unaryCond(func(x uint32) bool { return x == 0 }),
		1: unaryCond(func(x uint32) bool { return x != 0 }),
		2: unaryCond(func(x uint32) bool { return int32(x) > 0 }),
		3: unaryCond(func(x uint32) bool { return int32(x) < 0 }),
	}[(opcode>>6)&3]

	c.runMC(opMC.prepEA, opMC.read, decBlock, []mc{conditionBlock},
		[]mc{mcLit(delta), mcGetPC, mcPlus, mcPutPC})
	c.cycles++ // TODO Check speeds
}

func (unaryBranch) skip(c *m86k, opcode uint16) {
	c.pc++ // Skip the branch delta.
	c.skipOperand(opcode & 0x3f)
}

var unaryCondNames = []string{"BZR", "BNZ", "BPS", "BNG", "BZRD", "BNZD", "BPSD", "BNGD"}

func (unaryBranch) disassemble(d *disState, opcode uint16) {
	delta := d.consumeWord()
	operand := d.disOperand(opcode & 0x3f)
	sz := d.disL(opcode&0x10 != 0)
	cond := unaryCondNames[(opcode>>6)&7]
	d.emit(cond, sz, " ", operand, ", ", fmt.Sprintf("%d", delta))
}

func unaryCond(f func(x uint32) bool) mc {
	return func(c *m86k, s *mcState) {
		flag := f(s.pop())
		if flag {
			s.push(1)
		} else {
			s.push(0)
		}
	}
}

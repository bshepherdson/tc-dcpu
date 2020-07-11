package mocha

import "fmt"

type binaryOp int

var handleBinary = binaryOp(0)

func (binaryOp) run(c *m86k, opcode uint16) {
	dst := (opcode >> 6) & 0x3f
	src := opcode & 0x3f
	longwords := (opcode & 0x8000) != 0

	// Most of these are read src, read dst, operate, write dst.
	// But a few (SET, LEA, BTM) are read src, operate, write dst.
	// The skip and branch long forms are special too, so the approach here
	// is to determine the form and call a handler.

	srcMC := decodeOperand(src, longwords)
	dstMC := decodeOperand(dst, longwords)

	op := (opcode >> 11) & 7
	if op == 7 { // long form
		next := c.consumeWord()
		op = next & 0x1f
		binaryLongHandlers[op](c, dstMC, srcMC, next, longwords)
	} else {
		binaryShortHandlers[op](c, dstMC, srcMC, longwords)
	}
}

// Helper for wrapping a thread into a binary handler in dyadic (r, r, op, w) form.
func dyadic(t microThread) func(c *m86k, dstMC, srcMC *operandMC, longwords bool) {
	return func(c *m86k, dstMC, srcMC *operandMC, longwords bool) {
		c.runMC(srcMC.prepEA, srcMC.read, dstMC.prepEA, dstMC.read, t, dstMC.write)
	}
}

type longHandler func(c *m86k, dstMC, srcMC *operandMC, next uint16, longwords bool)

func dyadicLong(t microThread) longHandler {
	return func(c *m86k, dstMC, srcMC *operandMC, next uint16, longwords bool) {
		c.runMC(srcMC.prepEA, srcMC.read, dstMC.prepEA, dstMC.read, t, dstMC.write)
	}
}

func dyadicLongWidths(words, longs microThread) longHandler {
	return func(c *m86k, dstMC, srcMC *operandMC, next uint16, longwords bool) {
		t := words
		if longwords {
			t = longs
		}
		c.runMC(srcMC.prepEA, srcMC.read, dstMC.prepEA, dstMC.read, t, dstMC.write)
	}
}

var binaryShortHandlers = map[uint16]func(c *m86k, dst, src *operandMC, longwords bool){
	// SET
	1: func(c *m86k, dst, src *operandMC, longwords bool) {
		c.runMC(src.prepEA, src.read, dst.prepEA, dst.write)
		c.cycles++
	},

	// ADD
	2: func(c *m86k, dst, src *operandMC, longwords bool) {
		op := dyadic([]mc{mcLit(0),
			shortLong(longwords, opAddWord, opAddLongword), mcPutEX})
		op(c, dst, src, longwords)
	},

	// SUB
	3: func(c *m86k, dst, src *operandMC, longwords bool) {
		op := dyadic([]mc{mcLit(0),
			shortLong(longwords, opAddWord, opAddLongword), mcPutEX})
		op(c, dst, src, longwords)
	},

	// AND
	4: dyadic([]mc{mcAnd}),
	// BOR
	5: dyadic([]mc{mcBor}),
	// XOR
	6: dyadic([]mc{mcXor}),
}

var binaryLongHandlers = map[uint16]func(c *m86k, dst, src *operandMC, next uint16, longwords bool){
	// ADX
	0x00: func(c *m86k, dst, src *operandMC, next uint16, longwords bool) {
		op := dyadicLong([]mc{mcGetEX, mcCycles(1),
			shortLong(longwords, opAddWord, opAddLongword), mcPutEX})
		op(c, dst, src, next, longwords)
	},

	// SBX
	0x01: func(c *m86k, dst, src *operandMC, next uint16, longwords bool) {
		op := dyadicLong([]mc{mcGetEX, mcCycles(1),
			shortLong(longwords, opSubWord, opSubLongword), mcPutEX})
		op(c, dst, src, next, longwords)
	},

	// SHR
	0x02: dyadicLong([]mc{opShr}),
	// ASR
	0x03: dyadicLong([]mc{opAsr}),
	// SHL
	0x04: dyadicLong([]mc{opShl}),

	// MUL - Both widths are combined into one. It produces 32-bit result + EX on
	// the stack, and the word-size one drops EX and splits result.
	0x05: dyadicLongWidths(
		// Drop EX, split 32-bit result into lo hi.
		[]mc{opMul, mcDrop, mcSplitWords, mcPutEX},
		[]mc{opMul, mcPutEX, mcCycles(4)}), // 32-bit result + EX, extra 4 cycles

	// MLI - Similarly, both widths are combined for MLI.
	0x06: dyadicLongWidths(
		// Sign extend both args to 32-bit signed. MLI, clip to 16-bit result.
		// TODO Check the clip is right, that makes it unsigned again. That's
		// mostly a user problem I think.
		[]mc{mcSignExtend, mcSwap, mcSignExtend, mcSwap, opMli, mcClip16},
		[]mc{opMli, mcCycles(4)}), // 4 extra cycles for 32-bit.

	// DIV - Widths combined by opDiv.
	0x07: dyadicLongWidths(
		[]mc{opDiv, mcClip16, mcPutEX, mcClip16},
		[]mc{opDiv, mcPutEX, mcCycles(6)}),

	// DVI - Widths combined by opDvi
	0x08: dyadicLongWidths(
		[]mc{mcSignExtend, mcSwap, mcSignExtend, mcSwap, opDvi,
			mcClip16, mcPutEX, mcClip16},
		[]mc{opDvi, mcPutEX, mcCycles(4)}),

	// LEA
	0x09: func(c *m86k, dst, src *operandMC, next uint16, longwords bool) {
		c.runMC(src.prepEA, []mc{mcPushEA}, dst.prepEA, dst.write)
	},

	// BTX
	0x0a: dyadicLongWidths([]mc{mcMaskFor16, mcXor}, []mc{mcMaskFor32, mcXor}),
	// BTS
	0x0b: dyadicLongWidths([]mc{mcMaskFor16, mcBor}, []mc{mcMaskFor32, mcBor}),
	// BTC
	0x0c: dyadicLongWidths(
		[]mc{mcMaskFor16, mcNot, mcAnd, mcCycles(-1)},
		[]mc{mcMaskFor32, mcNot, mcAnd, mcCycles(-1)}),

	// BTM
	0x0d: func(c *m86k, dst, src *operandMC, next uint16, longwords bool) {
		c.runMC(src.prepEA, src.read,
			[]mc{shortLong(longwords, mcMaskFor16, mcMaskFor32)},
			dst.prepEA, dst.write)
	},

	// Branches
	// B
	0x10: binaryBranch(func(a, b uint16) bool { return a&b != 0 },
		func(a, b uint32) bool { return a&b != 0 }),
	// C
	0x11: binaryBranch(func(a, b uint16) bool { return a&b == 0 },
		func(a, b uint32) bool { return a&b == 0 }),
	// E
	0x12: binaryBranch(func(a, b uint16) bool { return a == b },
		func(a, b uint32) bool { return a == b }),
	// N
	0x13: binaryBranch(func(a, b uint16) bool { return a != b },
		func(a, b uint32) bool { return a != b }),
	// G
	0x14: binaryBranch(func(a, b uint16) bool { return a > b },
		func(a, b uint32) bool { return a > b }),
	// A
	0x15: binaryBranch(func(a, b uint16) bool { return int16(a) > int16(b) },
		func(a, b uint32) bool { return int32(a) > int32(b) }),
	// L
	0x16: binaryBranch(func(a, b uint16) bool { return a < b },
		func(a, b uint32) bool { return a < b }),
	// U
	0x17: binaryBranch(func(a, b uint16) bool { return int16(a) < int16(b) },
		func(a, b uint32) bool { return int32(a) < int32(b) }),
}

func binaryBranch(word func(a, b uint16) bool, long func(a, b uint32) bool) longHandler {
	return func(c *m86k, dst, src *operandMC, next uint16, longwords bool) {
		delta := int32(int16(next) >> 5)
		// It's relative to the current PC, just after the long word, before operands.
		newPC := c.pc + uint32(delta)

		// Actually evaluates our condition, (src dst -- cond?)
		var conditionBlock microThread
		if longwords {
			conditionBlock = []mc{mcCompare32(long)}
		} else {
			conditionBlock = []mc{mcCompare16(word)}
		}

		flag := c.runMC(src.prepEA, src.read, dst.prepEA, dst.read, conditionBlock)

		if delta == -1 { // Skipping mode
			if flag != 0 { // Condition succeeds
				c.skipping = false
				c.cycles += 2
			} else {
				c.skipping = true
				c.cycles += 3
			}
		} else { // Branching mode
			c.cycles += 2
			if flag != 0 { // Condition succeeds
				c.pc = newPC
			} else {
				c.cycles++
			}
		}
	}
}

// ( src dst ) on the stack, but we want a = dst, b = src
func mcCompare32(cmp func(a, b uint32) bool) mc {
	return func(c *m86k, s *mcState) {
		b := s.pop()
		a := s.pop()
		flag := cmp(a, b)
		if flag {
			s.push(1)
		} else {
			s.push(0)
		}
	}
}
func mcCompare16(cmp func(a, b uint16) bool) mc {
	return func(c *m86k, s *mcState) {
		b := s.pop()
		a := s.pop()
		flag := cmp(uint16(a), uint16(b))
		if flag {
			s.push(1)
		} else {
			s.push(0)
		}
	}
}

func (binaryOp) skip(c *m86k, opcode uint16) {
	if opcode&0x7000 == 0x7000 { // long form
		c.pc++
	}
	c.skipOperand(opcode & 0x3f)
	c.skipOperand((opcode >> 6) & 0x3f)
}

// ( bit-number -- bitmask )
func mcMaskFor32(c *m86k, s *mcState) {
	s.push(1 << (s.pop() & 0x1f))
}
func mcMaskFor16(c *m86k, s *mcState) {
	s.push(1 << (s.pop() & 0xf))
}

// ( src dst ex -- sum ex' )
func opAddWord(c *m86k, s *mcState) {
	sum := uint64(s.pop()) + uint64(s.pop()) + uint64(s.pop())
	s.push(uint32(sum & 0xffff))
	if sum > 0xffff {
		s.push(1)
	} else {
		s.push(0)
	}
	c.cycles++
}

// ( src dst ex -- sum ex' )
func opAddLongword(c *m86k, s *mcState) {
	sum := uint64(s.pop()) + uint64(s.pop()) + uint64(s.pop())
	s.push(uint32(sum))
	if sum > 0xffffffff {
		s.push(1)
	} else {
		s.push(0)
	}
	c.cycles++
}

// ( src dst ex -- sum ex' )
func opSubWord(c *m86k, s *mcState) {
	ex := uint64(s.pop())
	lhs := uint64(s.pop())
	rhs := uint64(s.pop())
	sum := lhs - rhs + ex
	s.push(uint32(sum & 0xffff))
	if lhs < rhs+ex {
		s.push(0xffffffff)
	} else {
		s.push(0)
	}
	c.cycles++
}

// ( src dst ex -- sum ex' )
func opSubLongword(c *m86k, s *mcState) {
	ex := uint64(s.pop())
	lhs := uint64(s.pop())
	rhs := uint64(s.pop())
	sum := lhs - rhs + ex
	s.push(uint32(sum))
	if lhs < rhs+ex {
		s.push(0xffffffff)
	} else {
		s.push(0)
	}
	c.cycles++
}

// ( src dst -- lo hi ) - Does a 64-bit unsigned multiply, putting the high
// longword on top of the stack.
func opMul(c *m86k, s *mcState) {
	res := uint64(s.pop()) * uint64(s.pop())
	s.push(uint32(res & 0xffffffff))
	s.push(uint32(res >> 32))
	c.cycles += 4 // 32-bit version adds 4 more in the thread.
}

// Does a signed 32-bit multiply, doesn't touch EX.
func opMli(c *m86k, s *mcState) {
	res := int32(s.pop()) * int32(s.pop())
	s.push(uint32(res))
	c.cycles += 4 // 32-bit version adds 4 more in the thread.
}

// Unsigned 32-bit division ( src dst -- quot rem )
func opDiv(c *m86k, s *mcState) {
	lhs := s.pop()
	rhs := s.pop()

	if rhs == 0 {
		s.push(0)
		s.push(0)
		return
	}

	s.push(lhs / rhs)
	s.push(lhs % rhs)
	c.cycles += 12 // 6 more for long words, handled above.
}

// Signed 32-bit division ( src dst -- quot rem )
func opDvi(c *m86k, s *mcState) {
	lhs := int32(s.pop())
	rhs := int32(s.pop())

	if rhs == 0 {
		s.push(0)
		s.push(0)
		return
	}

	s.push(uint32(lhs / rhs))
	s.push(uint32(lhs % rhs))
	// TODO Check the signed division is doing the right thing.
	c.cycles += 12 // 6 more for long words, handled above.
}

// ( src dst -- res ), updates EX accordingly.
// NB: src is always the shift amount, dst is shifted.
func opShr(c *m86k, s *mcState) {
	value := uint64(s.pop()) << 32
	value >>= s.pop()
	s.push(uint32(value >> 32))
	c.ex = uint32(value)
	c.cycles++
}

// ( src dst -- res ), updates EX accordingly.
// NB: src is always the shift amount, dst is shifted.
func opAsr(c *m86k, s *mcState) {
	value := int64(int32(s.pop())) << 32
	value >>= s.pop()
	s.push(uint32(value >> 32))
	c.ex = uint32(value)
	c.cycles++
}

// ( src dst -- res ), updates EX accordingly.
// NB: src is always the shift amount, dst is shifted.
func opShl(c *m86k, s *mcState) {
	value := uint64(s.pop())
	value <<= s.pop()
	s.push(uint32(value))
	c.ex = uint32(value >> 32)
	c.cycles++
}

func (binaryOp) disassemble(d *disState, opcode uint16) {
	op := binaryOpNames[(opcode>>12)&7]
	extra := ""
	if opcode&0x7000 == 0x7000 { // Long form
		next := d.consumeWord()
		op = binaryOpLongNames[next&0x1f]

		if next&0x10 != 0 { // Branch-type
			delta := int16(next) >> 5
			if delta == -1 { // IF-type
				op = "IF" + op[2:]
			} else { // Branch type
				extra = fmt.Sprintf(", %d", delta)
			}
		}
	}

	src := d.disOperand(opcode & 0x3f)
	dst := d.disOperand((opcode >> 6) & 0x3f)
	d.emit(op, d.disL(opcode&0x8000 != 0), " ", dst, ", ", src, extra)
}

var binaryOpNames = map[uint16]string{
	1: "SET",
	2: "ADD",
	3: "SUB",
	4: "AND",
	5: "BOR",
	6: "XOR",
}

var binaryOpLongNames = map[uint16]string{
	0x00: "ADX",
	0x01: "SBX",
	0x02: "SHR",
	0x03: "ASR",
	0x04: "SHL",
	0x05: "MUL",
	0x06: "MLI",
	0x07: "DIV",
	0x08: "DVI",
	0x09: "LEA",
	0x0a: "BTX",
	0x0b: "BTS",
	0x0c: "BTC",
	0x0d: "BTM",

	// Branches. These get replaced with IFx in the disassembly logic above.
	0x10: "BRB",
	0x11: "BRC",
	0x12: "BRE",
	0x13: "BRN",
	0x14: "BRG",
	0x15: "BRA",
	0x16: "BRL",
	0x17: "BRU",
}

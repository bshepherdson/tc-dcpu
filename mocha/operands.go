package mocha

// Microcode implementations for the various addressing modes.
// There's a few tricky things here. The most vital is that many addressing
// modes do expensive intermediate computations to determine their effective
// address. We don't want to duplicate that effort; nor can we easily, since PC
// has generally moved on it that case.
//
// - Branching opcodes are read-only, that's no problem.
// - Unary opcodes might read, write, or read and write.
// - Binary opcodes might just read, or might read then write.
//   In their read-write case, the other register's value is also involved.
//   We can't control the read order; in convoluted cases (eg. set [A]+, [A]+)
//   we need to guarantee the order of read right arg, read left arg, write left
//   arg.
//
// I think it suffices to have a side channel for the effective address in the
// mcState? Then the read-write style for something with an EA can be:
// - Consume any values from eg. [PC]+, and read any registers, needed to
//   compute the EA, putting it on the stack.
// - That performs any post-incrementing or other adjustment of values, once.
// - Call mcSetEA, popping that address into the side channel.
// - Read a word or longword from EA onto the stack.
// - ... other ops producing the result ...
// - Call mcGetEA, pushing the EA again.
// - Store our value at EA, without further side effects eg. postincrementing.
//
// I think that works for all cases? Branches have two operands, but they're
// read-only, so it suffices to read right, then read left, and done.
// Unary values might be read-write, but they work in the above design.
// Binary values work likewise, and the read and/or write of the other register
// is not confounded by extraneous values on the stack.
// Set has two operands, but they're read-only and write-only.
// I think that'll work.
//
// For skipping over operands, it usually suffices to prep their EA, since in
// all cases that gets read during prep.

type operandMC struct {
	prepEA microThread
	read   microThread
	write  microThread
}

var modeDecoders = map[uint16]func(regField uint16, longword bool) *operandMC{
	0: decodeRegDirect,
	1: decodeRegIndirect,
	2: decodeRegPostincrement,
	3: decodeRegPredecrement,
	4: decodeRegOffset,
	5: decodeRegIndexed,
	6: decodeSpecial1,
	7: decodeSpecial2,
}

var special1 = map[uint16]func(longword bool) *operandMC{
	0: operandPC,
	1: operandSP,
	2: operandEX,
	3: operandIA,
	4: operandPeek,
	5: operandPushPop,
	6: operandLit0,
	7: operandLit1,
}

var special2 = map[uint16]func(longword bool) *operandMC{
	0: operandAbsWord,
	1: operandAbsLongword,
	2: operandImmediateWord,
	3: operandImmediateLongword,
	4: operandPCIndirect,
	5: operandPCIndexed,
	6: operandPick,
	7: operandImmediateSignedWord,
}

func decodeOperand(op uint16, longword bool) *operandMC {
	mode := (op >> 3) & 7
	return modeDecoders[mode](op&7, longword)
}

var simpleWidths = map[uint16]uint32{
	0: 0, // A
	1: 0, // [A]
	2: 0, // [A]+
	3: 0, // -[A]
	4: 1, // [A + lit]
	5: 1, // [A, B]
	6: 0, // various, but all 0
}

var specialWidths = map[uint16]uint32{
	0: 1, // [lit_w]
	1: 2, // [lit_l]
	2: 1, // lit_w
	3: 2, // lit_l
	4: 1, // [PC + lit]
	5: 1, // [PC, A]
	6: 1, // [SP + lit]
	7: 1, // lit_sw
}

func operandWidth(op uint16) uint32 {
	mode := op >> 3
	if mode == 7 {
		return specialWidths[op&7]
	}
	return simpleWidths[mode]
}

func decodeSpecial1(regField uint16, longword bool) *operandMC {
	return special1[regField](longword)
}

func decodeSpecial2(regField uint16, longword bool) *operandMC {
	return special2[regField](longword)
}

func decodeRegDirect(regField uint16, longword bool) *operandMC {
	if longword {
		return &operandMC{
			read:  []mc{mcLit16(regField), mcReadReg32},
			write: []mc{mcLit16(regField), mcWriteReg32},
		}
	} else {
		return &operandMC{
			read:  []mc{mcLit16(regField), mcReadReg16},
			write: []mc{mcLit16(regField), mcWriteReg16},
		}
	}
}

func decodeRegIndirect(regField uint16, longword bool) *operandMC {
	thread := &operandMC{
		prepEA: []mc{mcLit16(regField), mcReadReg32, mcSetEA},
	}
	return readWriteEA(thread, longword)
}

// Expects prepEA to be defined, and adds basic read and write for the
// appropriate operand size.
func readWriteEA(op *operandMC, longword bool) *operandMC {
	if longword {
		op.read = []mc{mcPushEA, mcReadLongword}
		op.write = []mc{mcPushEA, mcWriteLongword}
	} else {
		op.read = []mc{mcPushEA, mcReadWord}
		op.write = []mc{mcPushEA, mcWriteWord}
	}
	return op
}

func decodeRegPostincrement(regField uint16, longword bool) *operandMC {
	var delta uint32 = 1
	if longword {
		delta = 2
	}

	thread := &operandMC{
		prepEA: []mc{
			mcLit16(regField), mcReadReg32,
			mcDup, mcLit(delta), mcPlus, // ( original incremented )
			mcLit16(regField), mcWriteReg32, // ( original )
			mcSetEA,
		},
	}
	return readWriteEA(thread, longword)
}

// Pre and post look overlapping, but they're not really.
// They do things in a different order.
func decodeRegPredecrement(regField uint16, longword bool) *operandMC {
	var delta uint32 = 1
	if longword {
		delta = 2
	}

	thread := &operandMC{
		prepEA: []mc{
			mcLit16(regField), mcReadReg32,
			mcLit(delta), mcMinus, // ( decremented )
			mcDup, mcLit16(regField), mcWriteReg32,
			mcSetEA,
		},
	}
	return readWriteEA(thread, longword)
}

func decodeRegOffset(regField uint16, longword bool) *operandMC {
	thread := &operandMC{
		prepEA: []mc{
			mcLit16(regField), mcReadReg32,
			mcConsumePCWord, mcSignExtend, mcPlus, mcSetEA,
		},
	}
	return readWriteEA(thread, longword)
}

func decodeRegIndexed(regField uint16, longword bool) *operandMC {
	thread := &operandMC{
		prepEA: []mc{
			mcLit16(regField), mcReadReg32,
			mcConsumePCWord, mcReadReg32, mcPlus, mcSetEA,
		},
	}
	return readWriteEA(thread, longword)
}

func specialReg(r mc, w mc, longword bool) *operandMC {
	if longword {
		return &operandMC{
			read:  []mc{r},
			write: []mc{w},
		}
	}
	return &operandMC{
		read:  []mc{r, mcClip16},
		write: []mc{mcClip16, w},
	}
}

func operandPC(longword bool) *operandMC {
	return specialReg(mcGetPC, mcPutPC, longword)
}

func operandSP(longword bool) *operandMC {
	return specialReg(mcGetSP, mcPutSP, longword)
}

func operandEX(longword bool) *operandMC {
	return specialReg(mcGetEX, mcPutEX, longword)
}

func operandIA(longword bool) *operandMC {
	return specialReg(mcGetIA, mcPutIA, longword)
}

func operandPeek(longword bool) *operandMC {
	return readWriteEA(&operandMC{
		prepEA: []mc{mcGetSP, mcSetEA},
	}, longword)
}

// This is awkward because it works both ways, push and pop.
// Only PEA and LEA use the effective address itself, and since they don't move
// it it's equivalent to PEEK.
// Otherwise, the read and write routines adjust SP properly.
func operandPushPop(longword bool) *operandMC {
	lit := shortLong(longword, mcLit(1), mcLit(2))
	return &operandMC{
		prepEA: []mc{mcGetSP, mcSetEA},
		read: []mc{mcGetSP, mcDup, lit, mcPlus, mcPutSP,
			shortLong(longword, mcReadWord, mcReadLongword)},
		write: []mc{mcGetSP, lit, mcMinus, mcDup, mcPutSP,
			shortLong(longword, mcWriteWord, mcWriteLongword)},
	}
}

func operandLit0(long bool) *operandMC {
	return &operandMC{
		read: []mc{mcLit(0)},
	}
}

func operandLit1(long bool) *operandMC {
	return &operandMC{
		read: []mc{mcLit(1)},
	}
}

func operandAbsWord(longword bool) *operandMC {
	return readWriteEA(&operandMC{
		prepEA: []mc{mcConsumePCWord, mcSetEA},
	}, longword)
}

func operandAbsLongword(longword bool) *operandMC {
	return readWriteEA(&operandMC{
		prepEA: []mc{mcConsumePCLongword, mcSetEA},
	}, longword)
}

func operandImmediateWord(longword bool) *operandMC {
	return &operandMC{
		read: []mc{mcConsumePCWord},
	}
}

func operandImmediateLongword(longword bool) *operandMC {
	return &operandMC{
		read: []mc{mcConsumePCLongword},
	}
}

func operandPCIndirect(longword bool) *operandMC {
	return readWriteEA(&operandMC{
		prepEA: []mc{mcGetPC, mcConsumePCWord, mcPlus, mcSetEA},
	}, longword)
}

// NB: We need to consume the word first, then fetch PC, since it's relative
// to that PC.
func operandPCIndexed(longword bool) *operandMC {
	return readWriteEA(&operandMC{
		prepEA: []mc{mcConsumePCWord, mcGetPC, mcReadReg32, mcPlus, mcSetEA},
	}, longword)
}

func operandPick(longword bool) *operandMC {
	return readWriteEA(&operandMC{
		prepEA: []mc{mcGetSP, mcConsumePCWord, mcPlus, mcSetEA},
	}, longword)
}

func operandImmediateSignedWord(longword bool) *operandMC {
	return &operandMC{
		read: []mc{mcConsumePCWord, mcSignExtend},
	}
}

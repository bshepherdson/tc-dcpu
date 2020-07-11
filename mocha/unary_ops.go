package mocha

import "fmt"

type unary int

var handleUnary = unary(0)

func (unary) run(c *m86k, opcode uint16) {
	// The rest have the size bit.
	longwords := opcode&0x8000 != 0
	opMC := decodeOperand(opcode&0x3f, longwords)
	unaryHandlers[(opcode>>6)&0x3f](c, opMC, longwords)
}

func (unary) skip(c *m86k, opcode uint16) {
	// Just skip the operand.
	c.skipOperand(opcode & 0x3f)
}

var unaryNames = []string{"", "SWP", "PEA", "NOT", "NEG", "JSR", "LOG", "LNK",
	"", "HWN", "HWQ", "HWI", "INT", "IAQ", "EXT"}

func (unary) disassemble(d *disState, opcode uint16) {
	d.emit(
		unaryNames[(opcode>>6)&0x3f],
		d.disL(opcode&0x8000 != 0),
		" ", d.disOperand(opcode&0x3f),
	)
}

var unaryHandlers = map[uint16]func(c *m86k, opMC *operandMC, longwords bool){
	1:  opSwp,
	2:  opPea,
	3:  opNot,
	4:  opNeg,
	5:  opJsr,
	6:  opLog,
	7:  opLnk,
	9:  opHwn,
	10: opHwq,
	11: opHwi,
	12: opInt,
	13: opIaq,
	14: opExt,
}

func opSwp(c *m86k, opMC *operandMC, longwords bool) {
	if longwords {
		c.runMC(opMC.prepEA, opMC.read,
			[]mc{mcSplitWords, mcSwap, mcJoinWords}, opMC.write)
	} else {
		// Need to read and write in case of side effects.
		c.runMC(opMC.prepEA, opMC.read, opMC.write)
	}
	c.cycles++
}

func opPea(c *m86k, opMC *operandMC, longwords bool) {
	c.runMC(opMC.prepEA,
		[]mc{mcPushEA, shortLong(longwords, mcPushWord, mcPushLongword)})
	c.cycles++
}

func opNot(c *m86k, opMC *operandMC, longwords bool) {
	var clip []mc
	if !longwords {
		clip = []mc{mcClip16}
	}

	c.runMC(opMC.prepEA, opMC.read,
		[]mc{func(c *m86k, s *mcState) {
			s.push(s.pop() & 0xffffffff)
		}},
		clip, opMC.write)
	c.cycles++
}

func opNeg(c *m86k, opMC *operandMC, longwords bool) {
	threads := []microThread{opMC.prepEA, opMC.read}

	if !longwords {
		threads = append(threads, []mc{mcClip16, mcSignExtend})
	}

	threads = append(threads, []mc{func(c *m86k, s *mcState) {
		s.push(uint32(-int32(s.pop())))
	}})

	if !longwords {
		threads = append(threads, []mc{mcClip16})
	}

	threads = append(threads, opMC.write)
	c.runMC(threads...)
	c.cycles++
}

func opJsr(c *m86k, opMC *operandMC, longwords bool) {
	readOnly(c, opMC, []mc{mcGetPC, mcPushLongword, mcPutPC})
	c.cycles++
}

func opLog(c *m86k, opMC *operandMC, longwords bool) {
	value := readOnly(c, opMC, []mc{})
	if longwords {
		fmt.Printf("Log: 0x%08x %d %c\n", value, int32(value), value)
	} else {
		fmt.Printf("Log: 0x%04x %d %c\n", value, int16(value), value)
	}
}

func opLnk(c *m86k, opMC *operandMC, longwords bool) {
	c.runMC(opMC.prepEA, opMC.read, []mc{
		mcLit16(7), mcReadReg32, mcPushLongword, // Save reg on stack.
		mcGetSP, mcLit16(7), mcWriteReg32, // Save new SP to reg.
		mcGetSP, mcPlus, mcPutSP, // And adjust SP by the operand.
	})
}

func opHwn(c *m86k, opMC *operandMC, longwords bool) {
	c.runMC(opMC.prepEA, []mc{mcLit(uint32(len(c.devices)))}, opMC.write)
	c.cycles += 2
}

func opHwq(c *m86k, opMC *operandMC, longwords bool) {
	devNum := readOnly(c, opMC, nil)
	dev := c.devices[devNum]
	devId, version, mfrId := dev.DeviceDetails()
	c.regs[0] = devId
	c.regs[2] = uint32(version)
	c.regs[3] = mfrId
	c.cycles += 4
}

func opHwi(c *m86k, opMC *operandMC, longwords bool) {
	dev := readOnly(c, opMC, nil)
	c.devices[dev].Interrupt(c)
	c.cycles += 4
}

func opInt(c *m86k, opMC *operandMC, longwords bool) {
	c.runMC(opMC.prepEA, opMC.read, []mc{mcClip16, mcQueueInt})
	c.cycles += 4
}

func opIaq(c *m86k, opMC *operandMC, longwords bool) {
	c.queueing = readOnly(c, opMC, nil) != 0
}

func opExt(c *m86k, opMC *operandMC, longwords bool) {
	c.runMC(opMC.prepEA, opMC.read, []mc{mcClip16, mcSignExtend}, opMC.write)
	c.cycles += 2
}

var unaryMainHandlers = map[uint16]func(c *m86k, opMC *operandMC, longwords bool){
	2: opNot,
	3: opNeg,
	4: opJsr,
	5: opIaq,
	6: opLog,
	7: opHwi,
}

func readOnly(c *m86k, opMC *operandMC, t microThread) uint32 {
	return c.runMC(opMC.prepEA, opMC.read, t)
}

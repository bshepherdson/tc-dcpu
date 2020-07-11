package mocha

type nullary int

func (nullary) run(c *m86k, opcode uint16) {
	if opcode == 0 { // NOP
		c.cycles++
	} else if opcode == 1 { // RFI
		c.runMC([]mc{
			mcPopLongword, mcLit16(0), mcWriteReg32, // Pop A
			mcPopLongword, mcPutPC, // Pop PC
		})
		c.queueing = false // and disable queueing.
		// No extra cycles on top of the 4 for those 2 longword reads.
	} else if opcode == 2 { // BRK
		c.debug = true
		c.cycles++
	} else if opcode == 3 { // HLT
		c.halted = true
	} else if opcode == 4 { // ULK
		c.runMC([]mc{
			mcLit16(7), mcReadReg32, mcPutSP, // Restore the reg to SP.
			mcPopLongword, mcLit16(7), mcWriteReg32, // Pop old reg value to reg.
		})
	}
}

func (nullary) skip(c *m86k, opcode uint16) {
	// No args, so nothing to do.
}

func (nullary) disassemble(d *disState, opcode uint16) {
	d.emit(map[uint16]string{
		0: "NOP",
		1: "RFI",
		2: "BRK",
		3: "HLT",
		4: "ULK",
	}[opcode&0x3f])
}

var handleNullary = nullary(0)

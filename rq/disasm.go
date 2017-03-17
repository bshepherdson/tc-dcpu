package rq

import (
	"fmt"
	"strings"
)

// Disassembler. Takes in the ROM or a piece of it, and dumps it to stdout.
// Format is:
// ADDR: WORD        disassembly...

// Risque-16 instructions are nearly always 1 word long, making this easier than
// the DCPU. (The exception is long-format branches.)

// Uses a similar format-based pattern to the interpreter.
var debugFormats = []func(c *rq, at, op uint16) uint16{
	disasmRI,
	disasmRI,
	disasmRI,
	disasmRI,
	disasmRRR,
	disasmBranch,
	disasmMemory,
	disasmMultiStore,
}

// Immediate format: 0oooodddXXXXXXXX
var riNames = []string{
	"",
	"mov",
	"neg",
	"cmp",
	"add",
	"sub",
	"mul",
	"lsl",
	"lsr",
	"asr",
	"and",
	"orr",
	"xor",
	"add",
	"add",
	"mvh",
}

func disasmRI(c *rq, at, op uint16) uint16 {
	opcode := (op >> 11) & 0xf
	reg := (op >> 8) & 7
	lit := op & 0xff
	if opcode == 0 {
		return disasmI(c, at, reg, lit)
	} else if opcode == 0xd {
		show(at, op, fmt.Sprintf("ADD r%d, PC, #%d", reg, lit))
	} else if opcode == 0xe {
		show(at, op, fmt.Sprintf("ADD r%d, SP, #%d", reg, lit))
	} else {
		show(at, op, fmt.Sprintf("%s r%d, #%d", riNames[opcode], reg, lit))
	}

	return 1
}

var iNames = []string{
	"add SP,",
	"sub SP,",
	"swi",
	"<illegal>",
	"<illegal>",
	"<illegal>",
	"<illegal>",
	"<illegal>",
}

func disasmI(c *rq, at, opcode, lit uint16) uint16 {
	show(at, (opcode<<8)|lit, fmt.Sprintf("%s #%d", iNames[opcode], lit))
	return 1
}

var rrrNames = []string{
	"",
	"add",
	"adc",
	"sub",
	"sbc",
	"mul",
	"lsl",
	"lsr",
	"asr",
	"and",
	"orr",
	"xor",
	"<illegal>",
	"<illegal>",
	"<illegal>",
	"<illegal>",
}

func disasmRRR(c *rq, at, op uint16) uint16 {
	opcode := (op >> 9) & 0xf
	if opcode == 0 {
		return disasmRR(c, at, op)
	}
	rd := op & 7
	rb := (op >> 6) & 7
	ra := (op >> 3) & 7
	show(at, op, fmt.Sprintf("%s r%d, r%d, r%d", rrrNames[opcode], rd, ra, rb))
	return 1
}

var rrNames = []string{"", "mov", "cmp", "cmn", "ror", "neg", "tst", "mvn"}

func disasmRR(c *rq, at, op uint16) uint16 {
	opcode := (op >> 6) & 7
	if opcode == 0 {
		return disasmR(c, at, op)
	}
	rd := op & 7
	ra := (op >> 3) & 7

	show(at, op, fmt.Sprintf("%s r%d, r%d", rrNames[opcode], rd, ra))
	return 1
}

var rNames = []string{"", "bx", "blx", "swi", "hwn", "hwq", "hwi", "xsr"}

func disasmR(c *rq, at, op uint16) uint16 {
	opcode := (op >> 3) & 7
	if opcode == 0 {
		return disasmVoid(c, at, op)
	}
	rd := op & 7
	show(at, op, fmt.Sprintf("%s r%d", rNames[opcode], rd))
	return 1
}

var voidNames = []string{"rfi", "ifs", "ifc", "ret", "popsp", "brk", "<illegal>", "<illegal>"}

func disasmVoid(c *rq, at, op uint16) uint16 {
	opcode := op & 7
	show(at, op, voidNames[opcode])
	return 1
}

var branchNames = []string{
	"b", "bl", "beq", "bne", "bcs", "bcc", "bmi", "bpl", "bvs", "bvc",
	"bhi", "bls", "bge", "blt", "bgt", "ble"}

func disasmBranch(c *rq, at, op uint16) uint16 {
	opcode := (op >> 9) & 0xf
	diff := op & 0x1ff
	if diff&0x100 != 0 {
		diff |= 0xfe00 // Sign-extend
	}
	target := at + 1 + diff
	length := uint16(1)
	if diff == 0xffff {
		target = c.mem[at+1] // Next word.
		length = 2
	}

	show(at, op, fmt.Sprintf("%s %04x", branchNames[opcode], target))
	return length
}

func disasmMemory(c *rq, at, op uint16) uint16 {
	opcode := (op >> 10) & 7
	rd := (op >> 7) & 7
	rb := (op >> 4) & 7
	lit := op & 0xf
	name := "ldr"
	if opcode&1 != 0 {
		name = "str"
	}

	switch opcode & 0x6 { // Ignore low bit
	case 0:
		show(at, op, fmt.Sprintf("%s r%d, [r%d], #%d", name, rd, rb, lit))
	case 2:
		show(at, op, fmt.Sprintf("%s r%d, [r%d, #%d]", name, rd, rb, lit))
	case 4:
		show(at, op, fmt.Sprintf("%s r%d, [r%d, r%d]", name, rd, rb, lit))
	case 6:
		show(at, op, fmt.Sprintf("%s r%d, [SP, #%d]", name, rd, lit))
	}

	return 1
}

func disasmMultiStore(c *rq, at, op uint16) uint16 {
	opcode := (op >> 11) & 3
	rb := (op >> 8) & 7
	regs := op & 0xff

	names := make([]string, 0, 8)
	for i := uint(0); i < 8; i++ {
		if regs&(1<<i) != 0 {
			names = append(names, fmt.Sprintf("r%d", i))
		}
	}

	if opcode <= 1 { // PUSH and POP
		var name string
		if opcode == 0 {
			name = "pop"
			if rb != 0 {
				names = append(names, "pc")
			}
		} else {
			name = "push"
			if rb != 0 {
				names = append(names, "lr")
			}
		}
		rlist := strings.Join(names, ", ")
		show(at, op, fmt.Sprintf("%s {%s}", name, rlist))
	} else { // LDMIA
		name := "ldmia"
		if opcode == 3 {
			name = "stmia"
		}
		rlist := strings.Join(names, ", ")
		show(at, op, fmt.Sprintf("%s r%d, {%s}", name, rb, rlist))
	}
	return 1
}

func show(at, op uint16, disasm string) {
	fmt.Printf("%04x: %04x %016b       %s\n", at, op, op, disasm)
}

func (c *rq) disasmROM() {
	// First find the highest nonzero word.
	var top int
	for i := 0; i <= 0xffff; i++ {
		if c.mem[i] != 0 {
			top = i
		}
	}

	for i := 0; i <= top; {
		i += int(c.disasmOp(uint16(i)))
	}
}

func (c *rq) disasmOp(at uint16) uint16 {
	op := c.mem[at]
	return debugFormats[bits(op, 13, 3)](c, at, op)
}

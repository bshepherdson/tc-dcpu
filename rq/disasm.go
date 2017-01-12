package rq

import (
	"fmt"
	"strings"
)

// Disassembler. Takes in the ROM or a piece of it, and dumps it to stdout.
// Format is:
// ADDR: WORD        disassembly...

// Risque-16 instructions are always 1 word long, making this easier than the
// DCPU.

// Uses a similar format-based pattern to the interpreter.

// No need to return a length, since they're all 1 each.
var debugFormats = []func(c *rq, at, op uint16) uint16{
	disasm1Or2,
	disasm1Or2,
	disasm3,
	disasm3,

	// 4
	disasm4Thru8,
	disasm9Or10,
	disasm11,
	disasm11,

	// 8
	disasm12,
	disasm13,
	disasm14,
	disasm15Or16,

	// c
	disasm17,
	disasm18Or19,
	disasm20,
	disasm21Or22,
}

var format1Names = []string{"lsl", "lsr", "asr"}

// Format 1: Shift by immediate.
// Format 2: Add/subtract three-arg form.
func disasm1Or2(c *rq, at, op uint16) uint16 {
	code := bits(op, 11, 2)
	if code == 3 {
		return disasm2(c, at, op)
	}

	show(at, op, fmt.Sprintf("%s r%d, r%d, #%d", format1Names[code],
		bits(op, 0, 3), bits(op, 3, 3), bits(op, 6, 5)))
	return 1
}

func disasm2(c *rq, at, op uint16) uint16 {
	name := "add"
	if flag(op, 9) {
		name = "sub"
	}

	var rhs string
	if flag(op, 10) { // Immediate
		rhs = fmt.Sprintf("#%d", bits(op, 6, 3))
	} else {
		rhs = fmt.Sprintf("r%d", bits(op, 6, 3))
	}

	show(at, op, fmt.Sprintf("%s r%d, r%d, %s", name, bits(op, 0, 3),
		bits(op, 3, 3), rhs))
	return 1
}

var format3Names = []string{"mov", "cmp", "add", "sub"}

// Format 3: Move/compare/add/subtract immediate
func disasm3(c *rq, at, op uint16) uint16 {
	show(at, op, fmt.Sprintf("%s r%d, #%d", format3Names[bits(op, 11, 2)],
		bits(op, 8, 3), bits(op, 0, 8)))
	return 1
}

func disasm4Thru8(c *rq, at, op uint16) uint16 {
	if bits(op, 10, 2) == 0 {
		return disasm4(c, at, op)
	} else if bits(op, 7, 5) == 0x1d {
		return disasm6(c, at, op)
	} else if bits(op, 10, 3) == 3 {
		return disasm5(c, at, op)
	} else if bits(op, 8, 4) == 7 {
		return disasm7(c, at, op)
	} else {
		return disasm8(c, at, op)
	}
}

var format4Names = []string{"and", "eor", "lsl", "lsr", "asr", "adc", "sbc",
	"ror", "tst", "neg", "cmp", "cmn", "orr", "mul", "bic", "mvn"}

// Format 4: 2-register ALU operations
func disasm4(c *rq, at, op uint16) uint16 {
	show(at, op, fmt.Sprintf("%s r%d, r%d", format4Names[bits(op, 6, 4)],
		bits(op, 0, 3), bits(op, 3, 3)))
	return 1
}

// Format 5: BX and BLX
func disasm5(c *rq, at, op uint16) uint16 {
	name := "bx"
	if flag(op, 6) {
		name = "blx"
	}
	show(at, op, fmt.Sprintf("%s r%d", name, bits(op, 0, 3)))
	return 1
}

var format6Names = []string{"hwn", "hwq", "hwi"}

// Format 6: Hardware
func disasm6(c *rq, at, op uint16) uint16 {
	show(at, op, fmt.Sprintf("%s r%d", format6Names[bits(op, 5, 2)],
		bits(op, 0, 3)))
	return 1
}

var format7Names = []string{"rfi", "rsi", "ifs", "ifc", "mrs", "msr"}

// Format 7: Manipulating CPSR
func disasm7(c *rq, at, op uint16) uint16 {
	code := bits(op, 5, 3)
	if code >= 4 {
		show(at, op, fmt.Sprintf("%s r%d", format7Names[code], bits(op, 0, 3)))
	} else {
		show(at, op, format7Names[code])
	}
	return 1
}

// Format 8: PC-relative load
func disasm8(c *rq, at, op uint16) uint16 {
	show(at, op, fmt.Sprintf("ldr r%d, [PC, #%d]", bits(op, 8, 3), bits(op, 0, 8)))
	return 1
}

func disasm9Or10(c *rq, at, op uint16) uint16 {
	if flag(op, 9) {
		return disasm10(c, at, op)
	} else {
		return disasm9(c, at, op)
	}
}

// Format 9: Load/store with register offset
func disasm9(c *rq, at, op uint16) uint16 {
	rd := bits(op, 0, 3)
	rb := bits(op, 3, 3)
	ra := bits(op, 6, 3)
	name := "str"
	if flag(op, 11) {
		name = "ldr"
	}

	f := "%s r%d, [r%d, r%d]"
	if flag(op, 10) { // Post-indexing
		f = "%s r%d, [r%d], r%d"
	}
	show(at, op, fmt.Sprintf(f, name, rd, rb, ra))
	return 1
}

// Format 10: Long literal load
func disasm10(c *rq, at, op uint16) uint16 {
	rd := bits(op, 0, 3)
	next := c.mem[at+1]
	show(at, op, fmt.Sprintf("ldr r%d, =0x%04x", rd, next))
	return 2
}

// Format 11: Load/store with immediate offset
func disasm11(c *rq, at, op uint16) uint16 {
	rd := bits(op, 0, 3)
	rb := bits(op, 3, 3)
	offset := bits(op, 6, 5)
	name := "str"
	if flag(op, 12) {
		name = "ldr"
	}

	f := "%s r%d, [r%d, #%d]"
	if flag(op, 11) { // Post-indexing
		f = "%s r%d, [r%d], #%d"
	}
	show(at, op, fmt.Sprintf(f, name, rd, rb, offset))
	return 1
}

// Format 12: Unused
func disasm12(c *rq, at, op uint16) uint16 {
	fmt.Printf("Illegal operation: format 12\n")
	return 1
}

// Format 13: SP-relative load/store
func disasm13(c *rq, at, op uint16) uint16 {
	name := "str"
	if flag(op, 11) {
		name = "ldr"
	}

	show(at, op, fmt.Sprintf("%s r%d, [SP, #%d]", name, bits(op, 8, 3),
		bits(op, 0, 8)))
	return 1
}

// Format 14: Load address
func disasm14(c *rq, at, op uint16) uint16 {
	reg := "PC"
	if flag(op, 11) {
		reg = "SP"
	}
	show(at, op, fmt.Sprintf("add r%d, %s, #%d", bits(op, 8, 3), reg,
		bits(op, 0, 8)))
	return 1
}

func disasm15Or16(c *rq, at, op uint16) uint16 {
	if bits(op, 9, 2) == 2 { // Format 16
		return disasm16(c, at, op)
	} else {
		return disasm15(c, at, op)
	}
}

// Format 15: Adjust stack pointer
func disasm15(c *rq, at, op uint16) uint16 {
	name := "add"
	if flag(op, 8) {
		name = "sub"
	}

	show(at, op, fmt.Sprintf("%s SP, #%d", name, bits(op, 0, 7)))
	return 1
}

var registerListing = []string{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"}

func buildRegList(op uint16) []string {
	s := make([]string, 0, 9)
	for i := uint16(0); i < 8; i++ {
		if flag(op, i) {
			s = append(s, registerListing[i])
		}
	}
	return s
}

// Format 16: Push/pop registers
func disasm16(c *rq, at, op uint16) uint16 {
	name := "push"
	if flag(op, 11) {
		name = "pop"
	}

	s := buildRegList(op)
	if flag(op, 8) {
		if flag(op, 11) { // Popping
			s = append(s, "pc")
		} else { // Pushing
			s = append(s, "lr")
		}
	}

	show(at, op, fmt.Sprintf("%s { %s }", name, strings.Join(s, ", ")))
	return 1
}

// Format 17: Multiple load/store
func disasm17(c *rq, at, op uint16) uint16 {
	name := "stmia"
	if flag(op, 11) {
		name = "ldmia"
	}

	s := buildRegList(op)
	show(at, op, fmt.Sprintf("%s r%d!, { %s }", name, bits(op, 8, 3),
		strings.Join(s, ", ")))
	return 1
}

var format18Names = []string{"beq", "bne", "bcs", "bcc", "bmi", "bpl",
	"bvs", "bvc", "bhi", "bls", "bge", "blt", "bgt", "ble", "ILLEGAL", "swi"}

// Format 18: Conditional branches
// Format 19: SWI
func disasm18Or19(c *rq, at, op uint16) uint16 {
	if bits(op, 8, 4) == 15 { // SWI
		show(at, op, fmt.Sprintf("%s %04x", format18Names[bits(op, 8, 4)],
			bits(op, 0, 8)))
	} else {
		signed := int16(bits(op, 0, 8))
		if signed > 127 {
			signed = 256 - signed
		}
		show(at, op, fmt.Sprintf("%s %d -> %04x", format18Names[bits(op, 8, 4)],
			signed, at+1+uint16(signed)))
	}

	return 1
}

// Format 20: Unconditional branch
func disasm20(c *rq, at, op uint16) uint16 {
	offset := signExtend(11, bits(op, 0, 11))
	show(at, op, fmt.Sprintf("b %04x (%d)", at+uint16(offset), offset))
	return 1
}

// Format 21: Immediate branch and link
// Format 22: Long branch and link
func disasm21Or22(c *rq, at, op uint16) uint16 {
	if flag(op, 11) { // Immediate
		show(at, op, fmt.Sprintf("bl %04x", bits(op, 0, 11)))
	} else {
		pos := "lo"
		if flag(op, 10) { // High
			pos = "hi"
		}
		show(at, op, fmt.Sprintf("bl_%s %02x", pos, bits(op, 0, 8)))
	}
	return 1
}

func show(at, op uint16, disasm string) {
	fmt.Printf("%04x: %04x %016b       %s\n", at, op, op, disasm)
}

func (c *rq) disasmROM() {
	for i := 0; i <= 0xffff; {
		i += int(c.disasmOp(uint16(i)))
	}
}

func (c *rq) disasmOp(at uint16) uint16 {
	op := c.mem[at]
	return debugFormats[bits(op, 12, 4)](c, at, op)
}

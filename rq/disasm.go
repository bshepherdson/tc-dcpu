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
var debugFormats = []func(c *rq, at, op uint16){
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

var format1Names = []string{"LSL", "LSR", "ASR"}

// Format 1: Shift by immediate.
// Format 2: Add/subtract three-arg form.
func disasm1Or2(c *rq, at, op uint16) {
	code := bits(op, 11, 2)
	if code == 3 {
		disasm2(c, at, op)
		return
	}

	show(at, op, fmt.Sprintf("%s r%d, r%d, #%d", format1Names[code],
		bits(op, 0, 3), bits(op, 3, 3), bits(op, 6, 5)))
}

func disasm2(c *rq, at, op uint16) {
	name := "ADD"
	if flag(op, 9) {
		name = "SUB"
	}

	var rhs string
	if flag(op, 10) { // Immediate
		rhs = fmt.Sprintf("#%d", bits(op, 6, 3))
	} else {
		rhs = fmt.Sprintf("r%d", bits(op, 6, 3))
	}

	show(at, op, fmt.Sprintf("%s r%d, r%d, %s", name, bits(op, 0, 3),
		bits(op, 3, 3), rhs))
}

var format3Names = []string{"MOV", "CMP", "ADD", "SUB"}

// Format 3: Move/compare/add/subtract immediate
func disasm3(c *rq, at, op uint16) {
	show(at, op, fmt.Sprintf("%s r%d, #%d", format3Names[bits(op, 11, 2)],
		bits(op, 8, 3), bits(op, 0, 8)))
}

func disasm4Thru8(c *rq, at, op uint16) {
	if bits(op, 10, 2) == 0 {
		disasm4(c, at, op)
	} else if bits(op, 7, 5) == 0x1d {
		disasm6(c, at, op)
	} else if bits(op, 10, 3) == 3 {
		disasm5(c, at, op)
	} else if bits(op, 8, 4) == 7 {
		disasm7(c, at, op)
	} else {
		disasm8(c, at, op)
	}
}

var format4Names = []string{"AND", "EOR", "LSL", "LSR", "ASR", "ADC", "SBC",
	"ROR", "TST", "NEG", "CMP", "CMN", "ORR", "MUL", "BIC", "MVN"}

// Format 4: 2-register ALU operations
func disasm4(c *rq, at, op uint16) {
	show(at, op, fmt.Sprintf("%s r%d, r%d", format4Names[bits(op, 6, 4)],
		bits(op, 0, 3), bits(op, 3, 3)))
}

// Format 5: BX and BLX
func disasm5(c *rq, at, op uint16) {
	name := "BX"
	if flag(op, 6) {
		name = "BLX"
	}
	show(at, op, fmt.Sprintf("%s r%d", name, bits(op, 0, 3)))
}

var format6Names = []string{"HWN", "HWQ", "HWI"}

// Format 6: Hardware
func disasm6(c *rq, at, op uint16) {
	show(at, op, fmt.Sprintf("%s r%d", format6Names[bits(op, 5, 2)],
		bits(op, 0, 3)))
}

var format7Names = []string{"RFI", "RSI", "IFS", "IFC", "MRS", "MSR"}

// Format 7: Manipulating CPSR
func disasm7(c *rq, at, op uint16) {
	code := bits(op, 5, 3)
	if code >= 4 {
		show(at, op, fmt.Sprintf("%s r%d", format7Names[code], bits(op, 0, 3)))
	} else {
		show(at, op, format7Names[code])
	}
}

// Format 8: PC-relative load
func disasm8(c *rq, at, op uint16) {
	show(at, op, fmt.Sprintf("LDR r%d, [PC, #%d]", bits(op, 8, 3), bits(op, 0, 8)))
}

// Format 9: Load/store with register offset
// Format 10: Unused
func disasm9Or10(c *rq, at, op uint16) {
	rd := bits(op, 0, 3)
	rb := bits(op, 3, 3)
	ra := bits(op, 6, 3)
	name := "STR"
	if flag(op, 11) {
		name = "LDR"
	}

	f := "%s r%d, [r%d, r%d]"
	if flag(op, 10) { // Post-indexing
		f = "%s r%d, [r%d], r%d"
	}
	show(at, op, fmt.Sprintf(f, name, rd, rb, ra))
}

// Format 11: Load/store with immediate offset
func disasm11(c *rq, at, op uint16) {
	rd := bits(op, 0, 3)
	rb := bits(op, 3, 3)
	offset := bits(op, 6, 5)
	name := "STR"
	if flag(op, 12) {
		name = "LDR"
	}

	f := "%s r%d, [r%d, #%d]"
	if flag(op, 11) { // Post-indexing
		f = "%s r%d, [r%d], #%d"
	}
	show(at, op, fmt.Sprintf(f, name, rd, rb, offset))
}

// Format 12: Unused
func disasm12(c *rq, at, op uint16) {
	fmt.Printf("Illegal operation: format 12\n")
}

// Format 13: SP-relative load/store
func disasm13(c *rq, at, op uint16) {
	name := "STR"
	if flag(op, 11) {
		name = "LDR"
	}

	show(at, op, fmt.Sprintf("%s r%d, [SP, #%d]", name, bits(op, 8, 3),
		bits(op, 0, 8)))
}

// Format 14: Load address
func disasm14(c *rq, at, op uint16) {
	reg := "PC"
	if flag(op, 11) {
		reg = "SP"
	}
	show(at, op, fmt.Sprintf("ADD r%d, %s, #%d", bits(op, 8, 3), reg,
		bits(op, 0, 8)))
}

func disasm15Or16(c *rq, at, op uint16) {
	if bits(op, 9, 2) == 2 { // Format 16
		disasm16(c, at, op)
	} else {
		disasm15(c, at, op)
	}
}

// Format 15: Adjust stack pointer
func disasm15(c *rq, at, op uint16) {
	name := "ADD"
	if flag(op, 8) {
		name = "SUB"
	}

	show(at, op, fmt.Sprintf("%s SP, #%d", name, bits(op, 0, 7)))
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
func disasm16(c *rq, at, op uint16) {
	name := "PUSH"
	if flag(op, 11) {
		name = "POP"
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
}

// Format 17: Multiple load/store
func disasm17(c *rq, at, op uint16) {
	name := "STMIA"
	if flag(op, 11) {
		name = "LDMIA"
	}

	s := buildRegList(op)
	show(at, op, fmt.Sprintf("%s r%d!, { %s }", name, bits(op, 8, 3),
		strings.Join(s, ", ")))
}

var format18Names = []string{"BEQ", "BNE", "BCS", "BCC", "BMI", "BPL",
	"BVS", "BVC", "BHI", "BLS", "BGE", "BLT", "BGT", "BLE", "ILLEGAL", "SWI"}

// Format 18: Conditional branches
// Format 19: SWI
func disasm18Or19(c *rq, at, op uint16) {
	show(at, op, fmt.Sprintf("%s %04x", format18Names[bits(op, 8, 4)],
		bits(op, 0, 8)))
}

// Format 20: Unconditional branch
func disasm20(c *rq, at, op uint16) {
	offset := signExtend(11, bits(op, 0, 11))
	show(at, op, fmt.Sprintf("B %04x (%d)", at+uint16(offset), offset))
}

// Format 21: Immediate branch and link
// Format 22: Long branch and link
func disasm21Or22(c *rq, at, op uint16) {
	if flag(op, 11) { // Immediate
		show(at, op, fmt.Sprintf("BL %04x", bits(op, 0, 11)))
	} else {
		pos := "LO"
		if flag(op, 10) { // High
			pos = "HI"
		}
		show(at, op, fmt.Sprintf("BL_%s %02x", pos, bits(op, 0, 8)))
	}
}

func show(at, op uint16, disasm string) {
	fmt.Printf("%04x: %04x %016b       %s\n", at, op, op, disasm)
}

func (c *rq) disasmROM() {
	for i := 0; i <= 0xffff; i++ {
		c.disasmOp(uint16(i))
	}
}

func (c *rq) disasmOp(at uint16) uint16 {
	op := c.mem[at]
	debugFormats[bits(op, 12, 4)](c, at, op)
	return 1
}

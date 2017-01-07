package dcpu

import "fmt"

// Disassembler. Takes in the ROM file, and dumps it out to stdout.
// The format is:
// ADDR: WORD (WORD (WORD))             disassembly...

var mainOps = map[uint16]string{
	0x01: "SET",
	0x02: "ADD",
	0x03: "SUB",
	0x04: "MUL",
	0x05: "MLI",
	0x06: "DIV",
	0x07: "DVI",
	0x08: "MOD",
	0x09: "MDI",
	0x0a: "AND",
	0x0b: "BOR",
	0x0c: "XOR",
	0x0d: "SHR",
	0x0e: "ASR",
	0x0f: "SHL",

	0x10: "IFB",
	0x11: "IFC",
	0x12: "IFE",
	0x13: "IFN",
	0x14: "IFG",
	0x15: "IFA",
	0x16: "IFL",
	0x17: "IFU",

	0x1a: "ADX",
	0x1b: "SBX",
	0x1e: "STI",
	0x1f: "STD",
}

var specOps = map[uint16]string{
	0x01: "JSR",
	0x08: "INT",
	0x09: "IAG",
	0x0a: "IAS",
	0x0b: "RFI",
	0x0c: "IAQ",
	0x10: "HWN",
	0x11: "HWQ",
	0x12: "HWI",
	0x13: "LOG",
	0x14: "BRK",
	0x15: "HLT",
}

var simpleArgs = map[uint16]string{
	0x00: "A",
	0x01: "B",
	0x02: "C",
	0x03: "X",
	0x04: "Y",
	0x05: "Z",
	0x06: "I",
	0x07: "J",
	0x08: "[A]",
	0x09: "[B]",
	0x0a: "[C]",
	0x0b: "[X]",
	0x0c: "[Y]",
	0x0d: "[Z]",
	0x0e: "[I]",
	0x0f: "[J]",
	0x18: "PUSH/POP", // TODO: Improve this.
	0x19: "PEEK",
	0x1b: "SP",
	0x1c: "PC",
	0x1d: "EX",
}

// Format strings with one uint16 argument.
var argFormats = map[uint16]string{
	0x10: "[A + $%x]",
	0x11: "[B + $%x]",
	0x12: "[C + $%x]",
	0x13: "[X + $%x]",
	0x14: "[Y + $%x]",
	0x15: "[Z + $%x]",
	0x16: "[I + $%x]",
	0x17: "[J + $%x]",
	0x1a: "PICK $%x",
	0x1e: "[$%x]",
	0x1f: "$%x",
}

func isWideOp(arg uint16) bool {
	return (0x10 <= arg && arg < 0x17) || arg == 0x1a || arg == 0x1e || arg == 0x1f
}

func printArg(arg, extra uint16) string {
	if arg >= 0x20 { // Immediate literal.
		return fmt.Sprintf("$%x", arg-0x21)
	}

	if s, ok := simpleArgs[arg]; ok {
		return s
	}

	return fmt.Sprintf(argFormats[arg], extra)
}

// Emits a single instruction. Returns the number of words it uses.
func disasmOp(mem []uint16, pc, opcode uint16) int {
	op := opcode & 0x1f
	b := (opcode >> 5) & 0x1f
	a := (opcode >> 10) & 0x3f

	var mnemonic string
	if op == 0 {
		mnemonic = specOps[b]
	} else {
		mnemonic = mainOps[op]
	}

	var aExtra, bExtra uint16
	words := fmt.Sprintf("%04x ", opcode)
	width := 1
	if isWideOp(a) {
		aExtra = mem[int(pc)+width]
		words += fmt.Sprintf("%04x ", aExtra)
		width++
	}
	if op != 0 && isWideOp(b) {
		bExtra = mem[int(pc)+width]
		words += fmt.Sprintf("%04x ", bExtra)
		width++
	}

	astr := printArg(a, aExtra)
	if op == 0 {
		fmt.Printf("%04x: %-20s  %s %s\n", pc, words, mnemonic, astr)
	} else {
		bstr := printArg(b, bExtra)
		fmt.Printf("%04x: %-20s  %s %s, %s\n", pc, words, mnemonic, bstr, astr)
	}

	return width
}

func disasmROM(mem []uint16) {
	pc := 0
	for pc < len(mem) {
		pc += disasmOp(mem, uint16(pc), mem[pc])
	}
}

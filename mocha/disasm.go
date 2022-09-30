package mocha

import (
	"fmt"
	"strings"
)

func (c *m86k) DisassembleOp(at uint32) uint16 {
	d := &disState{pc: at, cpu: c}
	opcode := d.consumeWord()
	//fmt.Printf("Dis: $%08x: $%04x\n", at, opcode)
	handlerFor(opcode).disassemble(d, opcode)

	var strs []string
	for _, w := range d.words {
		strs = append(strs, fmt.Sprintf("%04x", w))
	}
	wordsStr := strings.Join(strs, " ")

	fmt.Printf("%08x: %-40s    %s\n", at, wordsStr, strings.Join(d.chunks, ""))
	return uint16(len(d.words))
}

type disState struct {
	words  []uint16
	chunks []string
	pc     uint32
	cpu    *m86k
}

func (d *disState) emit(s ...string) {
	d.chunks = append(d.chunks, s...)
}

func (d *disState) word(x uint16) {
	d.words = append(d.words, x)
}

func (d *disState) consumeWord() uint16 {
	word := d.cpu.mem[d.pc]
	d.pc++
	d.word(word)
	return word
}

func (d *disState) consumeLongword() uint32 {
	lo := d.consumeWord()
	hi := d.consumeWord()
	return (uint32(hi) << 16) | uint32(lo)
}

// Updates disassembly state, including appending the operand's human-readable
// representation.
func (d *disState) disOperand(operand uint16) string {
	mode := (operand >> 3) & 7
	regField := operand & 7

	if mode == 0 { // A
		return d.reg(regField)
	} else if mode == 1 { // [A]
		return fmt.Sprintf("[%s]", d.reg(regField))
	} else if mode == 2 { // [A]+
		return fmt.Sprintf("[%s]+", d.reg(regField))
	} else if mode == 3 { // -[A]
		return fmt.Sprintf("-[%s]", d.reg(regField))
	} else if mode == 4 { // [A+lit]
		lit := d.consumeWord()
		return fmt.Sprintf("[%s + $%04x]", d.reg(regField), lit)
	} else if mode == 5 { // [A, B]
		reg := d.consumeWord()
		return fmt.Sprintf("[%s, %s]", d.reg(regField), d.reg(reg&7))
	} else if mode == 6 && regField == 0 { // PC
		return "PC"
	} else if mode == 6 && regField == 1 { // SP
		return "SP"
	} else if mode == 6 && regField == 2 { // EX
		return "EX"
	} else if mode == 6 && regField == 3 { // IA
		return "IA"
	} else if mode == 6 && regField == 4 { // [SP] / PEEK
		return "[SP]"
	} else if mode == 6 && regField == 5 { // push/pop
		return "PUSH/POP"
	} else if mode == 6 && regField == 6 { // 0
		return "0"
	} else if mode == 6 && regField == 7 { // 1
		return "1"
	} else if mode == 7 && regField == 0 { // [lit_w]
		lit := d.consumeWord()
		return fmt.Sprintf("[$%04x]", lit)
	} else if mode == 7 && regField == 1 { // [lit_l]
		lit := d.consumeLongword()
		return fmt.Sprintf("[$%08x]", lit)
	} else if mode == 7 && regField == 2 { // lit_w
		lit := d.consumeWord()
		return fmt.Sprintf("$%04x", lit)
	} else if mode == 7 && regField == 3 { // lit_l
		lit := d.consumeLongword()
		return fmt.Sprintf("$%08x", lit)
	} else if mode == 7 && regField == 4 { // [PC+lit]
		lit := d.consumeWord()
		return fmt.Sprintf("[PC + $%04x]", lit)
	} else if mode == 7 && regField == 5 { // [PC, A]
		reg := d.consumeWord()
		return fmt.Sprintf("[PC, %s]", d.reg(reg))
	} else if mode == 7 && regField == 6 { // [SP+lit]
		lit := d.consumeWord()
		return fmt.Sprintf("[SP + $%04x]", lit)
	} else { // lit_signed_w
		lit := d.consumeWord()
		return fmt.Sprintf("(sw %04x %d)", lit, lit)
	}
}

func (d *disState) reg(reg uint16) string {
	if reg < uint16(len(registers)) {
		return registers[reg]
	}
	return "(illegal)"
}

func (d *disState) disReg(reg uint16) {
	d.emit(d.reg(reg))
}

func (d *disState) disL(longwords bool) string {
	if longwords {
		return "L "
	} else {
		return "W "
	}
}

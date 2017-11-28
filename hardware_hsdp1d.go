package main

import (
	"fmt"
	"tc-dcpu/common"
)

type HSDP1D struct {
	mode    uint16
	buffer  []*bufEntry
	counter uint
}

type bufEntry struct {
	from uint16
	data []uint16
}

var modeSizes = []uint16{80, 16, 8}

const (
	modeText    = 0
	modeRawHex  = 1
	modeHexDump = 2
)

// DCPU ticks at 100kHZ. 2 lines per second is 50k cycles per line.
const ticksPerLine = 50000

func (p *HSDP1D) DeviceDetails() (uint32, uint16, uint32) {
	return 0xcff2a11d, 1, 0xf6976d00
}

func (p *HSDP1D) Interrupt(d common.CPU) {
	switch d.ReadReg(0) {
	case 0x0000: // SET_MODE
		p.mode = d.ReadReg(1) & 3
	case 0x0001: // GET_MODE
		d.WriteReg(0, p.mode)
	case 0x0002: // CUT_PAGE
		fmt.Printf("P: (page cut)\n\n\n")
	case 0x0003: // PRINT_SINGLE_LINE
		p.queueLine(d, d.ReadReg(1), 1*modeSizes[p.mode])
	case 0x0004: // PRINT_MULTIPLE_LINES
		p.queueLine(d, d.ReadReg(1), d.ReadReg(2)*modeSizes[p.mode])
	case 0x0005: // FULL_DUMP
		p.queueLine(d, 0, 0xffff)
	case 0x0006: // BUFFER_STATUS
		total := 0
		for _, b := range p.buffer {
			total += len(b.data)
		}
		d.WriteReg(0, uint16(total))
	case 0xffff: // RESET
		p.buffer = nil
		p.mode = 0
	}
}

func (p *HSDP1D) Tick(d common.CPU) {
	p.counter--
	if p.counter == 0 {
		p.printLine()
		p.counter = ticksPerLine
		if Turbo {
			p.counter = 1
		}
	}
}

func (p *HSDP1D) Cleanup() {}

func NewHSDP1D() common.Device {
	p := new(HSDP1D)
	p.counter = ticksPerLine
	return p
}

func (p *HSDP1D) queueLine(d common.CPU, start, count uint16) {
	buf := make([]uint16, count, count)
	copy(buf, d.Memory()[start:start+count])
	p.buffer = append(p.buffer, &bufEntry{start, buf})
}

func (b *bufEntry) pop(count uint16) ([]uint16, bool) {
	if len(b.data) <= int(count) {
		return b.data, false
	}

	ret := b.data[0:count]
	b.data = b.data[count:]
	b.from += count
	return ret, true
}

// Reads an entire line of data from the buffer and prints it.
func (p *HSDP1D) printLine() {
	// If the mode is bad, we silently print nothing.
	if len(p.buffer) == 0 || int(p.mode) >= len(modeSizes) {
		return
	}

	from := p.buffer[0].from
	toWrite, more := p.buffer[0].pop(modeSizes[p.mode])
	if !more {
		p.buffer = p.buffer[1:]
	}

	// Render our line, which might be shorter than intended.
	switch p.mode {
	case modeText:
		bytes := make([]byte, 80)
		i := 0
		for _, word := range toWrite {
			b := word & 0x7f
			if b >= 32 {
				bytes[i] = byte(b)
				i++
			}
		}
		fmt.Printf("P: %s\n", string(bytes[0:i]))

	case modeRawHex:
		fmt.Printf("P:")
		for _, word := range toWrite {
			fmt.Printf(" %04x", word)
		}
		fmt.Printf("\n")

	case modeHexDump:
		for len(toWrite) < int(modeSizes[modeHexDump]) {
			toWrite = append(toWrite, 0)
		}

		bytes := make([]byte, 8)
		for i, word := range toWrite {
			b := word & 0x7f
			if b >= 32 {
				bytes[i] = byte(b)
			} else {
				bytes[i] = '.'
			}
		}

		fmt.Printf("%04x:  %04x %04x %04x %04x  %04x %04x %04x %04x    %s\n", from,
			toWrite[0], toWrite[1], toWrite[2], toWrite[3], toWrite[4], toWrite[5],
			toWrite[6], toWrite[7], string(bytes))
	}
}

package main

// This is an implementation of the Techcompliant KaiComm Synchronous Serial
// Interface or SSI. This is a full-duplex, synchronous, wired communication
// channel. It is implemented here as a bridge to the host machine's terminal,
// which can be handy for automated testing and other applications.

// TODO: Accurately simulate the speeds given by the spec; this version runs as
// fast as the host machine cares to go.

import (
	"bufio"
	"fmt"
	"os"

	"github.com/bshepherdson/tc-dcpu/common"
)

type Serial struct {
	recvBuffer []byte
	baud       uint16
	blockSize  uint16
	recvInt    uint16
	xmitInt    uint16
	input      <-chan string
}

func (s *Serial) DeviceDetails() (uint32, uint16, uint32) {
	return 0xe57d9027, 0x0103, 0xa87c900e
}

func (s *Serial) Interrupt(d common.CPU) {
	switch d.ReadReg(0) {
	case 0x0000: // Query Status
		stat := uint16(0)
		if len(s.recvBuffer) > 0 {
			stat |= 4
		}
		if s.recvInt != 0 {
			stat |= 0x4000
		}
		if s.xmitInt != 0 {
			stat |= 0x8000
		}
		d.WriteReg(0, stat)

	case 0x0001: // Configure port
		// B holds the block size minus 1, C the baud in the low 8 bits.
		// The transfer rate is (C+1) * 3125, giving a range of 3125 - 800,000 bits
		// per second.
		s.blockSize = (d.ReadReg(1) & 3) + 1
		s.baud = 3125 * ((d.ReadReg(2) & 0xff) + 1)

	case 0x0002: // Receive
		// Receives one block into B:A, with the MSB having been received first.
		if len(s.recvBuffer) < int(s.blockSize) {
			d.WriteReg(0, 0)
			d.WriteReg(1, 0)
			d.WriteReg(2, 3) // No data error.
			return
		}

		raw := uint32(0)
		i := 0
		for ; i < int(s.blockSize); i++ {
			raw = (raw << 8) | uint32(s.recvBuffer[i])
		}
		s.recvBuffer = s.recvBuffer[i:]
		d.WriteReg(1, uint16(raw>>16))
		d.WriteReg(0, uint16(raw))
		d.WriteReg(2, 0) // No error.

	case 0x0003: // Transmit
		// C:B contains the data, which is sent MSB first.
		hi := d.ReadReg(2)
		lo := d.ReadReg(1)
		if s.blockSize >= 4 {
			s.emit(byte(hi >> 8))
		}
		if s.blockSize >= 3 {
			s.emit(byte(hi))
		}
		if s.blockSize >= 2 {
			s.emit(byte(lo >> 8))
		}
		s.emit(byte(lo))
		d.WriteReg(2, 0) // No error.
		os.Stdout.Sync()

	case 0x0004: // Configure interrupts
		// B[0] is receive interrupt, B[1] is transmit.
		// C is receive message, X is transmit message.
		b := d.ReadReg(1)
		if b&1 != 0 {
			s.recvInt = d.ReadReg(2)
		}
		if b&2 != 0 {
			s.xmitInt = d.ReadReg(3)
		}

	case 0xffff: // Reset
		s.recvBuffer = []byte{}
		s.baud = 3125
		s.blockSize = 1
		s.recvInt = 0
		s.xmitInt = 0
	}
}

func (s *Serial) emit(b byte) {
	if b == 0x11 {
		b = '\n'
	}
	os.Stdout.Write([]byte{b})
}

func (s *Serial) Tick(d common.CPU) {
	select {
	case stdin, ok := <-s.input:
		if !ok {
			return
		}
		wasEmpty := len(s.recvBuffer) == 0
		for _, b := range []byte(stdin) {
			toSend := b
			if b == '\n' {
				toSend = 0x11
			}
			s.recvBuffer = append(s.recvBuffer, toSend)
		}

		if wasEmpty && len(s.recvBuffer) > 0 {
			if s.recvInt > 0 {
				d.AddInterrupt(s.recvInt)
			}
		}
	default:
	}
}

func (s *Serial) Cleanup() {}

func NewSerial() *Serial {
	s := new(Serial)
	ch := make(chan string)
	s.input = ch

	go func(c chan<- string) {
		reader := bufio.NewReader(os.Stdin)
		for {
			str, err := reader.ReadString('\n')
			if err != nil {
				fmt.Printf("Got an error, shutting down the channel: %v\n", err)
				close(ch)
				return
			}
			ch <- str
		}
	}(ch)

	return s
}

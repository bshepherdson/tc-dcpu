package main

import (
	"emulator/common"
	"fmt"
	"io"
	"os"
)

const sectorSize = 1024
const cyclesPerSector = 1666 // 200 RPM, 18 sectors/track = 1666 cycles/sector
const cyclesPerTrack = 2400  // 2.4ms per track.

const (
	floppyStateNoMedia uint16 = 0
	floppyStateReady          = 1
	floppyStateReadyWP        = 2
	floppyStateBusy           = 3
)

const (
	floppyErrorNone      uint16 = 0
	floppyErrorBusy             = 1
	floppyErrorNoMedia          = 2
	floppyErrorProtected        = 3
	floppyErrorEject            = 4
	floppyErrorBadSector        = 5
	floppyErrorBroken           = 0xffff
)

type M35FD struct {
	file           *os.File
	size           int64
	state          uint16
	lastError      uint16
	intMessage     uint16
	busy           bool
	writeProtected bool
	writing        bool

	sector    uint16
	addr      uint16
	countdown uint16
}

var diskNumber int

/*
30,700 words/second
each track has 18*512 = 9216 per track = 3.33116 circuits per second
200 RPM

Note that the disk image is stored big-endian. That is, the first byte of the
file is the high portion of the first word, and the second byte is the low part.
*/
func NewM35FD(cpu common.CPU) common.Device {
	if diskNumber >= len(diskFileNames) {
		panic("not enough filenames for number of disks")
	}
	name := diskFileNames[diskNumber]
	diskNumber++

	disk := new(M35FD)
	disk.Open(cpu, name)
	return disk
}

func (fd *M35FD) Open(cpu common.CPU, name string) bool {
	// If there's a previous one, eject first.
	// That makes sure we really send the no-media interrupt.
	if fd.file != nil {
		fd.Eject(cpu)
	}

	file, err := os.Open(name)
	if err != nil {
		fmt.Printf("ERROR: could not find file '%s': %v", name, err)
		fd.Eject(cpu)
		return false
	}
	fd.file = file
	info, err := file.Stat()
	if err != nil {
		fmt.Printf("ERROR: could not stat disk file: %v", err)
		fd.Eject(cpu)
		return false
	}
	fd.size = info.Size()
	fd.state = floppyStateReady
	fd.maybeInterrupt(cpu)
	return true
}

func (fd *M35FD) Eject(cpu common.CPU) {
	if fd.file != nil {
		fd.file.Close()
	}
	fd.file = nil
	fd.size = 0
	fd.state = floppyStateNoMedia
	fd.maybeInterrupt(cpu)
}

func (fd *M35FD) DeviceDetails() (uint32, uint16, uint32) {
	return 0x4fd524c5, 0x000b, 0x1eb37e91
}

func (fd *M35FD) Interrupt(c common.CPU) {
	switch c.ReadReg(0) {
	case 0: // Poll device
		c.WriteReg(1, fd.state)
		c.WriteReg(2, fd.lastError)
		fd.lastError = 0

	case 1: // Set interrupt
		fd.intMessage = c.ReadReg(3)
		fd.maybeInterrupt(c)

	case 2: // Read sector
		if fd.state == floppyStateNoMedia {
			fd.lastError = floppyErrorNoMedia
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		} else if fd.state == floppyStateBusy {
			fd.lastError = floppyErrorBusy
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		} else if sectorSize*int64(c.ReadReg(3)) < fd.size {
			fd.state = floppyStateBusy
			fd.writing = false
			fd.busy = true
			oldSector := fd.sector
			fd.sector = c.ReadReg(3)
			fd.addr = c.ReadReg(4)
			c.WriteReg(1, 1)
			fd.countdown = seekTime(oldSector, fd.sector)
			fd.maybeInterrupt(c)
		} else {
			fd.lastError = floppyErrorBadSector
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		}

	case 3: // Write sector
		if fd.state == floppyStateNoMedia {
			fd.lastError = floppyErrorNoMedia
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		} else if fd.state == floppyStateBusy {
			fd.lastError = floppyErrorBusy
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		} else if fd.state == floppyStateReadyWP {
			fd.lastError = floppyErrorProtected
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		} else if sectorSize*int64(c.ReadReg(3)) < fd.size {
			fd.writing = true
			fd.state = floppyStateBusy
			fd.busy = true
			oldSector := fd.sector
			fd.sector = c.ReadReg(3)
			fd.addr = c.ReadReg(4)
			c.WriteReg(1, 1)
			fd.maybeInterrupt(c)
			fd.countdown = seekTime(oldSector, fd.sector)
		} else {
			fd.lastError = floppyErrorBadSector
			fd.maybeInterrupt(c)
			c.WriteReg(1, 0)
		}
	}
}

func seekTime(old, nu uint16) uint16 {
	oldTrack := old / 18
	oldPos := old % 18
	nuTrack := nu / 18
	nuPos := nu % 18

	tracks := oldTrack - nuTrack
	if tracks < 0 {
		tracks = -tracks
	}
	sectors := oldPos - nuPos
	if sectors < 0 {
		sectors = -sectors
	}

	return tracks*cyclesPerTrack + sectors*cyclesPerSector
}

func (fd *M35FD) maybeInterrupt(c common.CPU) {
	if fd.intMessage != 0 {
		c.AddInterrupt(fd.intMessage)
	}
}

func (fd *M35FD) Tick(c common.CPU) {
	if !fd.busy {
		return
	}

	fd.countdown--
	if fd.countdown > 0 {
		return
	}

	// Otherwise our operation just finished.
	fd.busy = false
	fd.state = floppyStateReady
	if fd.writeProtected {
		fd.state = floppyStateReadyWP
	}
	fd.lastError = floppyErrorNone

	// Actually perform the read or write!
	if fd.writing {
		mem := c.Memory()[fd.addr : fd.addr+512]
		bytes := make([]byte, 1024)
		for i := 0; i < 512; i++ {
			bytes[2*i] = byte(mem[i] >> 8)
			bytes[2*i+1] = byte(mem[i] & 0xff)
		}
		_, err := fd.file.WriteAt(bytes, int64(fd.sector)*1024)
		if err != nil {
			fd.lastError = floppyErrorBroken
		}
	} else {
		mem := c.Memory()[fd.addr : fd.addr+512]
		bytes := make([]byte, 1024)
		n, err := fd.file.ReadAt(bytes, int64(fd.sector)*1024)
		if err != nil && err != io.EOF {
			fd.lastError = floppyErrorBroken
			return
		}

		// In case of EOF, we read 0s.
		for i := 0; i < 512; i++ {
			if 2*i < n {
				mem[i] = (uint16(bytes[2*i]) << 8) | uint16(bytes[2*i+1])
			} else {
				mem[i] = 0
			}
		}
	}
	fd.maybeInterrupt(c)
}

func (fd *M35FD) Cleanup() {
	fd.file.Close()
}

package main

import (
	"fmt"
	"time"

	"github.com/veandco/go-sdl2/sdl"
)

type Keyboard struct {
	rawMode    bool
	intMessage uint16
	lastPoll   time.Time
	head       int
	tail       int
	buffer     [256]uint16
	keysDown   [256]bool
}

const inputInterval time.Duration = time.Millisecond * 50

// New puts it into the right state: empty buffer, smart text mode.
func (k *Keyboard) DeviceDetails() (uint32, uint16, uint32) {
	return 0x30c17406, 1, 0x1c6c8b36
}

// Nothing to do per-cycle.
func (k *Keyboard) Tick(d *dcpu) {
	if time.Since(k.lastPoll) < inputInterval {
		return
	}

	k.lastPoll = time.Now()

	for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
		switch t := event.(type) {
		case *sdl.QuitEvent:
			d.kill()
		case *sdl.KeyDownEvent:
			fmt.Printf("keydown: %d %c\n", t.Keysym.Sym, rune(t.Keysym.Sym))
		case *sdl.KeyUpEvent:
			fmt.Printf("keyup: %d %c\n", t.Keysym.Sym, rune(t.Keysym.Sym))
		}
	}
}

func (k *Keyboard) Interrupt(d *dcpu) {
	switch d.regs[0] {
	case 0: // CLEAR_BUFFER
		k.head = 0
		k.tail = 0

	case 1: // GET_NEXT
		if k.head == k.tail {
			d.regs[2] = 0
		} else {
			d.regs[2] = k.buffer[k.head]
			k.head++
			if k.head >= 256 {
				k.head -= 256
			}
		}

	case 2: // CHECK_KEY
		down := uint16(0)
		if k.keysDown[d.regs[1]] {
			down = 1
		}
		d.regs[2] = down

	case 3: // SET_INT
		k.intMessage = d.regs[1]

	case 4: // SET_MODE
		k.rawMode = d.regs[1] == 1
		// Buffer is cleared on mode switch.
		k.head = 0
		k.tail = 0
	}
}

func (k *Keyboard) Cleanup() {}

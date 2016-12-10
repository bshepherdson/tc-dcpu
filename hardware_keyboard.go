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

const inputInterval time.Duration = time.Millisecond * 20

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
			key, index := k.readKey(t.Keysym)
			fmt.Printf("keydown: %d %c\n", index, key)

			if key == 0 {
				if index != 0 {
					k.keysDown[index] = true
				}
				return
			}

			k.keysDown[index] = true
			k.buffer[k.tail] = key
			k.tail++
			if k.tail >= 256 {
				k.tail -= 256
			}
			fmt.Printf("Buffered: %d %c\n", key, rune(key))

		case *sdl.KeyUpEvent:
			key, index := k.readKey(t.Keysym)
			fmt.Printf("keyup: %d %c\n", index, key)

			k.keysDown[index] = false
		}
	}
}

var keyCodes = map[sdl.Keycode]uint16{
	sdl.K_LSHIFT:    0x90,
	sdl.K_RSHIFT:    0x90,
	sdl.K_LCTRL:     0x91,
	sdl.K_RCTRL:     0x91,
	sdl.K_LALT:      0x92,
	sdl.K_RALT:      0x92,
	sdl.K_BACKSPACE: 0x10,
	sdl.K_RETURN:    0x11,
	sdl.K_INSERT:    0x12,
	sdl.K_DELETE:    0x13,
	sdl.K_UP:        0x80,
	sdl.K_DOWN:      0x81,
	sdl.K_LEFT:      0x82,
	sdl.K_RIGHT:     0x83,
}

var shiftedKeys = map[sdl.Keycode]uint16{
	sdl.K_0:            41,  // )
	sdl.K_1:            33,  // !
	sdl.K_2:            64,  // @
	sdl.K_3:            35,  // #
	sdl.K_4:            36,  // $
	sdl.K_5:            37,  // %
	sdl.K_6:            94,  // ^
	sdl.K_7:            38,  // &
	sdl.K_8:            42,  // *
	sdl.K_9:            40,  // (
	sdl.K_MINUS:        95,  // _
	sdl.K_EQUALS:       43,  // +
	sdl.K_LEFTBRACKET:  123, // {
	sdl.K_RIGHTBRACKET: 125, // }
	sdl.K_BACKSLASH:    124, // |
	sdl.K_BACKQUOTE:    126, // ~
	sdl.K_COMMA:        60,  // <
	sdl.K_PERIOD:       62,  // >
	sdl.K_SLASH:        63,  // ?
	sdl.K_SEMICOLON:    58,  // :
	sdl.K_QUOTE:        34,  // "
}

func (k *Keyboard) readKey(sym sdl.Keysym) (uint16, uint32) {
	if code, ok := keyCodes[sym.Sym]; ok {
		if !k.rawMode && 0x90 <= code && code <= 0x92 { // Cooked, and mod key.
			return 0, uint32(code)
		}
		return code, uint32(code)
	} else if 0x20 <= uint32(sym.Sym) && uint32(sym.Sym) < 0x80 { // Printable ASCII
		// If we're in raw mode, done. If in cooked mode, mix in shift state.
		if !k.rawMode {
			if (sym.Mod & sdl.KMOD_SHIFT) != 0 {
				// Letters get anded with ~32
				var code uint16
				if sdl.K_a <= sym.Sym && sym.Sym <= sdl.K_z {
					code = uint16(sym.Sym) &^ 0x20
				} else if c, ok := shiftedKeys[sym.Sym]; ok {
					code = c
				} else {
					code = uint16(sym.Sym) // No shift for this key.
				}
				return code, uint32(code)
			}
		}
		return uint16(sym.Sym), uint32(sym.Sym)
	}

	// Unrecognized key. Return 0.
	fmt.Printf("Unrecognized keycode: %d\n", sym.Sym)
	return 0, 0
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

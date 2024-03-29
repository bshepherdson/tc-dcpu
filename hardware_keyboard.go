package main

import (
	"time"

	"github.com/bshepherdson/tc-dcpu/common"

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
	fKeys      []int
}

const inputInterval time.Duration = time.Millisecond * 20

// New puts it into the right state: empty buffer, smart text mode.
func (k *Keyboard) DeviceDetails() (uint32, uint16, uint32) {
	return 0x30c17406, 1, 0x1c6c8b36
}

// Nothing to do per-cycle.
func (k *Keyboard) Tick(c common.CPU) {
	if time.Since(k.lastPoll) < inputInterval {
		return
	}

	k.lastPoll = time.Now()

	for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
		switch t := event.(type) {
		case *sdl.QuitEvent:
			c.Exit()
		case *sdl.KeyboardEvent:
			if t.Type == sdl.KEYDOWN {
				key, index := k.readKey(t.Keysym, true)

				if key == 0 {
					if index != 0 {
						k.keysDown[index] = true
					}
					return
				}

				k.keysDown[index] = true
				k.Enqueue(key)
			} else {
				_, index := k.readKey(t.Keysym, false)

				k.keysDown[index] = false
			}
		}
	}

	// Drain the fKeys buffer to the CPU.
	for _, k := range k.fKeys {
		fKey(c, k)
	}
	k.fKeys = k.fKeys[0:0]
}

func (k *Keyboard) Enqueue(key uint16) {
	k.buffer[k.tail] = key
	k.tail++
	if k.tail >= 256 {
		k.tail -= 256
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

var fKeys = map[sdl.Keycode]int{
	sdl.K_F1:  1,
	sdl.K_F2:  2,
	sdl.K_F3:  3,
	sdl.K_F4:  4,
	sdl.K_F5:  5,
	sdl.K_F6:  6,
	sdl.K_F7:  7,
	sdl.K_F8:  8,
	sdl.K_F9:  9,
	sdl.K_F10: 10,
	sdl.K_F11: 11,
	sdl.K_F12: 12,
}

func (k *Keyboard) readKey(sym sdl.Keysym, down bool) (uint16, uint32) {
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
	} else if index, ok := fKeys[sym.Sym]; ok && down {
		k.fKeys = append(k.fKeys, index)
	}

	// Unrecognized key. Return 0.
	//fmt.Printf("Unrecognized keycode: %d\n", sym.Sym)
	return 0, 0
}

func (k *Keyboard) Interrupt(c common.CPU) {
	switch c.ReadReg(0) {
	case 0: // CLEAR_BUFFER
		k.head = 0
		k.tail = 0

	case 1: // GET_NEXT
		if k.head == k.tail {
			c.WriteReg(2, 0)
		} else {
			c.WriteReg(2, k.buffer[k.head])
			k.head++
			if k.head >= 256 {
				k.head -= 256
			}
		}

	case 2: // CHECK_KEY
		down := uint16(0)
		if k.keysDown[c.ReadReg(1)] {
			down = 1
		}
		c.WriteReg(2, down)

	case 3: // SET_INT
		k.intMessage = c.ReadReg(1)

	case 4: // SET_MODE
		k.rawMode = c.ReadReg(1) == 1
		// Buffer is cleared on mode switch.
		k.head = 0
		k.tail = 0
	}
}

func (k *Keyboard) Cleanup() {}

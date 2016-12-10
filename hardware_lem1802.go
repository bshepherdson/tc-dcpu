package main

import (
	"fmt"
	"runtime"
	"time"
	"unsafe"

	"github.com/veandco/go-sdl2/sdl"
)

const (
	scaleFactor  int = 4
	widthPixels      = 128
	heightPixels     = 96
	widthChars       = 32
	heightChars      = 12
	fontWidth        = 4
	fontHeight       = 8
)

type LEM1802 struct {
	vram    uint16
	fontram uint16
	palram  uint16
	//border  uint16 TODO: Implement borders

	window   *sdl.Window
	renderer *sdl.Renderer
	texture  *sdl.Texture
	pixels   unsafe.Pointer

	lastFrame time.Time
}

func (lem *LEM1802) DeviceDetails() (uint32, uint16, uint32) {
	return 0x734df615, 0x1802, 0x1c6c8b36
}

func (lem *LEM1802) Interrupt(d *dcpu) {
	fmt.Printf("LEM interrupt: A = %04x B = %04x\n", d.regs[ra], d.regs[rb])
	switch d.regs[0] {
	case 0: // MAP_SCREEN
		lem.vram = d.regs[1]
	case 1: // MAP_FONT
		lem.fontram = d.regs[1]
	case 2: // MAP_PALETTE
		lem.palram = d.regs[1]
	}
}

func (lem *LEM1802) Tick(d *dcpu) {
	// TODO: When the display gets disabled, paint it black.
	if lem.vram != 0 && time.Since(lem.lastFrame) > 50*time.Millisecond {
		fmt.Printf("Painting\n")
		var pitch int
		err := lem.texture.Lock(nil, &lem.pixels, &pitch)
		if pitch != widthPixels*4 {
			panic(fmt.Errorf("unexpected pitch: %d", pitch))
		}
		if err != nil {
			panic(fmt.Errorf("error locking texture: %v", err))
		}

		var palette []uint16 = defaultPalette[:]
		var font []uint16 = defaultFont[:]
		if lem.palram != 0 {
			palette = d.mem[lem.palram : lem.palram+16]
		}
		if lem.fontram != 0 {
			font = d.mem[lem.fontram : lem.fontram+256]
		}
		vram := d.mem[lem.vram : lem.vram+widthChars*heightChars]

		for i := 0; i < heightChars; i++ {
			for j := 0; j < widthChars; j++ {
				lem.writeChar(&palette, &font, vram[i*widthChars+j], i, j)
			}
		}

		// Fully painted, now flip the texture onto the display.
		lem.texture.Unlock()
		err = lem.renderer.Clear()
		if err != nil {
			panic(fmt.Errorf("failed to clear renderer: %v", err))
		}
		err = lem.renderer.Copy(lem.texture, &sdl.Rect{0, 0, int32(widthPixels), int32(heightPixels)},
			&sdl.Rect{0, 0, int32(widthPixels * scaleFactor), int32(heightPixels * scaleFactor)})
		if err != nil {
			panic(fmt.Errorf("failed to copy texture: %v", err))
		}

		lem.renderer.Present()
		lem.lastFrame = time.Now()
	}
}

func (lem *LEM1802) writeChar(palette, font *[]uint16, char uint16, row, col int) {
	c := char & 0x7f // Just the actual character value.
	fontLo := (*font)[c*2]
	fontHi := (*font)[c*2+1]

	// Now write the pixels, where the LSB in each half-word is the topmost pixel
	// in that column.
	y := row * fontHeight
	x := col * fontWidth
	fg := (*palette)[(char>>12)&0xf]
	bg := (*palette)[(char>>8)&0xf]

	lem.writeColumn(fg, bg, x, y, uint8(fontLo>>8))
	lem.writeColumn(fg, bg, x+1, y, uint8(fontLo))
	lem.writeColumn(fg, bg, x+2, y, uint8(fontHi>>8))
	lem.writeColumn(fg, bg, x+3, y, uint8(fontHi))
}

func (lem *LEM1802) writeColumn(fg, bg uint16, x, y int, pixels uint8) {
	for i := uint(0); i < 8; i++ {
		b := (pixels >> i) & 1
		c := fg
		if b == 0 {
			c = bg
		}
		lem.writePixel(c, x, y+int(i))
	}
}

// Turns a DCPU colour value and writes it into the texture.
// DCPU format is 0000rrrrggggbbbb, texture's format is ARGB.
func (lem *LEM1802) writePixel(c uint16, x, y int) {
	offset := widthPixels*4*uintptr(y) + 4*uintptr(x)
	if offset < 0 || offset > (widthPixels*heightPixels*4) {
		panic(fmt.Errorf("drawing outside legal region: (%d, %d) = %x\n", x, y, offset))
	}
	p := unsafe.Pointer(uintptr(lem.pixels) + offset)

	r := (uint32(c) >> 8) & 0xf
	r = r | (r << 4)
	g := (uint32(c) >> 4) & 0xf
	g = g | (g << 4)
	b := uint32(c) & 0xf
	b = b | (b << 4)

	converted := (0xff000000) | (r << 16) | (g << 8) | b
	*((*uint32)(p)) = converted
}

func (lem *LEM1802) Cleanup() {}

func NewLEM1802() device {
	lem := new(LEM1802)

	lem.lastFrame = time.Now()

	runtime.LockOSThread() // Latch this goroutine to the same thread for SDL.
	sdl.Init(sdl.INIT_VIDEO)
	window, err := sdl.CreateWindow("LEM 1802", sdl.WINDOWPOS_UNDEFINED,
		sdl.WINDOWPOS_UNDEFINED, widthPixels*scaleFactor, heightPixels*scaleFactor, sdl.WINDOW_SHOWN)
	if err != nil {
		panic(fmt.Errorf("failed to create window: %v", err))
	}

	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)
	if err != nil {
		panic(fmt.Errorf("failed to create renderer: %v", err))
	}

	texture, err := renderer.CreateTexture(sdl.PIXELFORMAT_ARGB8888,
		sdl.TEXTUREACCESS_STREAMING, widthPixels, heightPixels)
	if err != nil {
		panic(fmt.Errorf("failed to create texture: %v", err))
	}

	lem.window = window
	lem.renderer = renderer
	lem.texture = texture
	return lem
}

var defaultPalette [16]uint16 = [16]uint16{
	0x0000, 0x000a, 0x00a0, 0x00aa,
	0x0a00, 0x0a0a, 0x0aa0, 0x0aaa,
	0x0555, 0x055f, 0x05f5, 0x05ff,
	0x0f55, 0x0f5f, 0x0ff5, 0x0fff,
}

var defaultFont [256]uint16 = [256]uint16{
	0xb79e, 0x388e, // 0x00 - blob thingy 1
	0x722c, 0x75f4, // 0x01 - blob thingy 2
	0x19bb, 0x7f8f, // 0x02 - blob thingy 3
	0x85f9, 0xb158, // 0x03 - blob thingy 4
	0x242e, 0x2400, // 0x04 - plus/minus
	0x082a, 0x0800, // 0x05 - division
	0x0008, 0x0000, // 0x06 - centered dot
	0x0808, 0x0808, // 0x07 - centered horizontal line
	0x00ff, 0x0000, // 0x08 - centered vertical line
	0x00f8, 0x0808, // 0x09 - outline SE quarter
	0x08f8, 0x0000, // 0x0a - outline SW quarter
	0x080f, 0x0000, // 0x0b - outline NW quarter
	0x000f, 0x0808, // 0x0c - outline NE quarter
	0x00ff, 0x0808, // 0x0d - vertical bar with E leg
	0x08f8, 0x0808, // 0x0e - horizontal bar with S leg
	0x08ff, 0x0000, // 0x0f - vertical bar with W leg

	0x080f, 0x0808, // 0x10 - horizontal bar with N leg
	0x08ff, 0x0808, // 0x11 - cross
	0x6633, 0x99cc, // 0x12 - cross-diagonal lines
	0x9933, 0x66cc, // 0x13 - main-diagonal lines
	0xfef8, 0xe080, // 0x14 - diagonal SW half
	0x7f1f, 0x0301, // 0x15 - diagonal NW half
	0x0107, 0x1f7f, // 0x16 - diagonal NE half
	0x80e0, 0xf8fe, // 0x17 - diagonal SE half
	0x5500, 0xaa00, // 0x18 - dotted lines
	0x55aa, 0x55aa, // 0x19 - checkerboard
	0xffaa, 0xff55, // 0x1a - negative space dotted lines
	0x0f0f, 0x0f0f, // 0x1b - N half
	0xf0f0, 0xf0f0, // 0x1c - S half
	0x0000, 0xffff, // 0x1d - E half
	0xffff, 0x0000, // 0x1e - W half
	0xffff, 0xffff, // 0x1f - wholly filled

	0x0000, 0x0000, // 0x20 - space (wholly empty)
	0x005f, 0x0000, // 0x21 - !
	0x0300, 0x0300, // 0x22 - "
	0x3e14, 0x3e00, // 0x23 - #
	0x266b, 0x3200, // 0x24 - $
	0x611c, 0x4300, // 0x25 - %
	0x3629, 0x7650, // 0x26 - &
	0x0002, 0x0100, // 0x27 - '
	0x1c22, 0x4100, // 0x28 - (
	0x4122, 0x1c00, // 0x29 - )
	0x1408, 0x1400, // 0x2a - *
	0x081c, 0x0800, // 0x2b - +
	0x4020, 0x0000, // 0x2c - ,
	0x0808, 0x0800, // 0x2d - -
	0x0040, 0x0000, // 0x2e - .
	0x601c, 0x0300, // 0x2f - /

	0x3e49, 0x3e00, // 0x30 - 0
	0x427f, 0x4000, // 0x31 - 1
	0x6259, 0x4600, // 0x32 - 2
	0x2249, 0x3600, // 0x33 - 3
	0x0f08, 0x7f00, // 0x34 - 4
	0x2745, 0x3900, // 0x35 - 5
	0x3e49, 0x3200, // 0x36 - 6
	0x6119, 0x0700, // 0x37 - 7
	0x3649, 0x3600, // 0x38 - 8
	0x2649, 0x3e00, // 0x39 - 9
	0x0024, 0x0000, // 0x3a - :
	0x4024, 0x0000, // 0x3b - ;
	0x0814, 0x2200, // 0x3c - <
	0x1414, 0x1400, // 0x3d - =
	0x2214, 0x0800, // 0x3e - >
	0x0259, 0x0600, // 0x3f - ?

	0x3e59, 0x5e00, // 0x40 - @
	0x7e09, 0x7e00, // 0x41 - A
	0x7f49, 0x3600, // 0x42 - B
	0x3e41, 0x2200, // 0x43 - C
	0x7f41, 0x3e00, // 0x44 - D
	0x7f49, 0x4100, // 0x45 - E
	0x7f09, 0x0100, // 0x46 - F
	0x3e41, 0x7a00, // 0x47 - G
	0x7f08, 0x7f00, // 0x48 - H
	0x417f, 0x4100, // 0x49 - I
	0x2040, 0x3f00, // 0x4a - J
	0x7f08, 0x7700, // 0x4b - K
	0x7f40, 0x4000, // 0x4c - L
	0x7f06, 0x7f00, // 0x4d - M
	0x7f01, 0x7e00, // 0x4e - N
	0x3e41, 0x3e00, // 0x4f - O

	0x7f09, 0x0600, // 0x50 - P
	0x3e41, 0xbe00, // 0x51 - Q
	0x7f09, 0x7600, // 0x52 - R
	0x2649, 0x3200, // 0x53 - S
	0x017f, 0x0100, // 0x54 - T
	0x3f40, 0x7f00, // 0x55 - U
	0x1f60, 0x1f00, // 0x56 - V
	0x7f30, 0x7f00, // 0x57 - W
	0x7708, 0x7700, // 0x58 - X
	0x0778, 0x0700, // 0x59 - Y
	0x7149, 0x4700, // 0x5a - Z
	0x007f, 0x4100, // 0x5b - [
	0x031c, 0x6000, // 0x5c - \
	0x417f, 0x0000, // 0x5d - ]
	0x0201, 0x0200, // 0x5e - ^
	0x8080, 0x8000, // 0x5f - _

	0x0001, 0x0200, // 0x60 - `
	0x2454, 0x7800, // 0x61 - a
	0x7f44, 0x3800, // 0x62 - b
	0x3844, 0x2800, // 0x63 - c
	0x3844, 0x7f00, // 0x64 - d
	0x3854, 0x5800, // 0x65 - e
	0x087e, 0x0900, // 0x66 - f
	0x4854, 0x3c00, // 0x67 - g
	0x7f04, 0x7800, // 0x68 - h
	0x047d, 0x0000, // 0x69 - i
	0x2040, 0x3d00, // 0x6a - j
	0x7f10, 0x6c00, // 0x6b - k
	0x017f, 0x0000, // 0x6c - l
	0x7c18, 0x7c00, // 0x6d - m
	0x7c04, 0x7800, // 0x6e - n
	0x3844, 0x3800, // 0x6f - o

	0x7c14, 0x0800, // 0x70 - p
	0x0814, 0x7c00, // 0x71 - q
	0x7c04, 0x0800, // 0x72 - r
	0x4854, 0x2400, // 0x73 - s
	0x043e, 0x4400, // 0x74 - t
	0x3c40, 0x7c00, // 0x75 - u
	0x1c60, 0x1c00, // 0x76 - v
	0x7c30, 0x7c00, // 0x77 - w
	0x6c10, 0x6c00, // 0x78 - x
	0x4c50, 0x3c00, // 0x79 - y
	0x6454, 0x4c00, // 0x7a - z
	0x0836, 0x4100, // 0x7b - {
	0x0077, 0x0000, // 0x7c - |
	0x4136, 0x0800, // 0x7d - }
	0x0201, 0x0201, // 0x7e - ~
	0x0205, 0x0200, // 0x7f - degrees
}

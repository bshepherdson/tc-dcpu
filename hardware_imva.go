package main

import (
	"fmt"
	"runtime"
	"tc-dcpu/common"
	"time"
	"unsafe"

	"github.com/veandco/go-sdl2/sdl"
)

const (
	imvaScaleFactor  int = 3
	imvaWidthPixels      = 320
	imvaHeightPixels     = 200
	memMapSize           = 4000 // Each word gives an 8x2 region: 40 wide, 100 tall.
	columns              = 40
	rows                 = 200
	metaRows             = 100
	columnWidth          = 8
)

type IMVA struct {
	vram          uint16
	colour        uint16
	overlay       uint16
	overlayOffset uint16
	overlayEffect uint16 // TODO: Support the overlay and its effects.

	window   *sdl.Window
	renderer *sdl.Renderer
	texture  *sdl.Texture
	pixels   unsafe.Pointer

	lastFrame time.Time
}

func (imva *IMVA) DeviceDetails() (uint32, uint16, uint32) {
	return 0x75f6a113, 0x0538, 0x59ea5742
}

func (imva *IMVA) Interrupt(c common.CPU) {
	switch c.ReadReg(0) {
	case 0: // Activate and map the display.
		imva.vram = c.ReadReg(1)
	case 1: // Configure the overlay.
		imva.overlay = c.ReadReg(1)
		imva.overlayOffset = c.ReadReg(2)
	case 2: // Configure effects.
		imva.colour = c.ReadReg(1)
		imva.overlayEffect = c.ReadReg(2)
	}
}

func (imva *IMVA) Tick(c common.CPU) {
	// TODO: When the display gets disabled, paint it black.
	if imva.vram != 0 && time.Since(imva.lastFrame) > 50*time.Millisecond {
		pixels, pitch, err := imva.texture.Lock(nil) //, &imva.pixels, &pitch)
		if pitch != imvaWidthPixels*4 {
			panic(fmt.Errorf("unexpected pitch: %d", pitch))
		}
		if err != nil {
			panic(fmt.Errorf("error locking texture: %v", err))
		}

		vram := c.Memory()[imva.vram : imva.vram+memMapSize]
		imva.paint(pixels, vram)

		// Fully painted, now flip the texture onto the display.
		imva.texture.Unlock()
		err = imva.renderer.Clear()
		if err != nil {
			panic(fmt.Errorf("failed to clear renderer: %v", err))
		}
		err = imva.renderer.Copy(imva.texture, &sdl.Rect{0, 0, int32(imvaWidthPixels), int32(imvaHeightPixels)},
			&sdl.Rect{0, 0, int32(imvaWidthPixels * imvaScaleFactor), int32(imvaHeightPixels * imvaScaleFactor)})
		if err != nil {
			panic(fmt.Errorf("failed to copy texture: %v", err))
		}

		imva.renderer.Present()
		imva.lastFrame = time.Now()
	}
}

// Paints the entire display into the provided pixel buffer.
func (imva *IMVA) paint(pixels []byte, vram []uint16) {
	// We work our way through the pixel map, a word at a time.
	// 40 rows per line, 2 lines per word = 100 words.
	for row := 0; row < metaRows; row++ {
		for col := 0; col < columns; col++ {
			// Read the word for this 8x2 region.
			raster := vram[row*40+col]
			for x := 0; x < 8; x++ {
				mask := uint16(1 << (7 - uint(x)))
				imva.writePixel(pixels, raster&mask > 0, col*8+x, 2*row)
				imva.writePixel(pixels, raster&(mask<<8) > 0, col*8+x, 2*row+1)
			}
		}
	}
}

// Turns a DCPU colour value and writes it into the texture.
// DCPU format is 0000rrrrggggbbbb, texture's format is ARGB.
func (imva *IMVA) writePixel(pixels []byte, on bool, x, y int) {
	offset := imvaWidthPixels*4*uintptr(y) + 4*uintptr(x)
	if offset < 0 || offset > (imvaWidthPixels*imvaHeightPixels*4) {
		panic(fmt.Errorf("drawing outside legal region: (%d, %d) = %x\n", x, y, offset))
	}

	var r, g, b byte // Defaults to 0, black, for the background.
	if on {
		r = byte(imva.colour & 0xf)
		r = r | (r << 4)
		g = byte((imva.colour >> 4) & 0xf)
		g = g | (g << 4)
		b = byte((imva.colour >> 8) & 0xf)
		b = b | (b << 4)
	}

	// TODO: Handle the opacity parameter.
	pixels[offset+3] = 0xff // Alpha
	pixels[offset+2] = r
	pixels[offset+1] = g
	pixels[offset] = b
}

func (imva *IMVA) Cleanup() {
	imva.texture.Destroy()
	imva.renderer.Destroy()
	imva.window.Destroy()
}

func NewIMVA() common.Device {
	imva := new(IMVA)

	imva.lastFrame = time.Now()
	imva.colour = 0x0fff // Highest-contrast white.

	runtime.LockOSThread() // Latch this goroutine to the same thread for SDL.
	sdl.Init(sdl.INIT_VIDEO)
	window, err := sdl.CreateWindow("IMVA", sdl.WINDOWPOS_UNDEFINED,
		sdl.WINDOWPOS_UNDEFINED, imvaWidthPixels*imvaScaleFactor, imvaHeightPixels*imvaScaleFactor, sdl.WINDOW_SHOWN)
	if err != nil {
		panic(fmt.Errorf("failed to create window: %v", err))
	}

	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)
	if err != nil {
		panic(fmt.Errorf("failed to create renderer: %v", err))
	}

	texture, err := renderer.CreateTexture(sdl.PIXELFORMAT_ARGB8888,
		sdl.TEXTUREACCESS_STREAMING, imvaWidthPixels, imvaHeightPixels)
	if err != nil {
		panic(fmt.Errorf("failed to create texture: %v", err))
	}

	imva.window = window
	imva.renderer = renderer
	imva.texture = texture
	return imva
}

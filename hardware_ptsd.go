package main

import (
	"fmt"
	"runtime"
	"tc-dcpu/common"
	"time"

	"github.com/veandco/go-sdl2/sdl"
)

const (
	ptsdScaleFactor  int = 4
	ptsdWidthPixels  int = 128
	ptsdHeightPixels int = 96
)

type tileSpec struct {
	x, y    int
	tileNum uint16
}

type PTSD struct {
	tileRAM    uint16
	bgMapRAM   uint16
	paletteRAM uint16
	spriteRAM  uint16

	smallBackground bool
	wideSprites     bool
	tallSprites     bool
	colorDepth      int
	tileWidth       int
	tileHeight      int

	interrupt uint16
	bgPalette uint16
	bgScrollX uint16
	bgScrollY uint16

	window   *sdl.Window
	renderer *sdl.Renderer
	texture  *sdl.Texture

	lastFrame time.Time
}

func (p *PTSD) DeviceDetails() (uint32, uint16, uint32) {
	return 0x73f747c2, 2, 0x8b43a166
}

func (p *PTSD) Interrupt(c common.CPU) {
	switch c.ReadReg(0) {
	case 0: // SET_CONFIG
		b := c.ReadReg(1)
		p.smallBackground = b&0x1000 != 0
		p.wideSprites = b&0x0200 != 0
		p.tallSprites = b&0x0100 != 0

		p.colorDepth = 1 << uint(((b>>4)&3)-1)
		p.tileWidth = 4
		p.tileHeight = 4
		if b&2 != 0 {
			p.tileWidth = 8
		}
		if b&1 != 0 {
			p.tileHeight = 8
		}
		if p.tileWidth == 8 && p.tileHeight == 4 {
			// Illegal state: 8x4 tiles. Treats them as 4x4 and display corrupted.
			p.tileWidth = 4
		}

	case 1: // SET_INTERRUPT
		p.interrupt = c.ReadReg(1)
	case 2: // SET_BG_PALETTE
		p.bgPalette = c.ReadReg(1)
	case 3: // SET_SCROLL
		p.bgScrollX = c.ReadReg(3) & 0xff
		p.bgScrollY = c.ReadReg(4) & 0xff
	case 0x10: // MAP_TILE_DATA
		p.tileRAM = c.ReadReg(1)
	case 0x11: // MAP_PALETTES
		p.paletteRAM = c.ReadReg(1)
	case 0x12: // MAP_BACKGROUND
		p.bgMapRAM = c.ReadReg(1)
	case 0x13: // MAP_SPRITES
		p.spriteRAM = c.ReadReg(1)
	case 0xffff: // RESET
		p.tileRAM = 0
		p.bgMapRAM = 0
		p.paletteRAM = 0
		p.spriteRAM = 0

		p.smallBackground = false
		p.tallSprites = false
		p.wideSprites = false
		p.colorDepth = 4
		p.tileWidth = 4
		p.tileHeight = 4
		p.interrupt = 0

		p.bgPalette = 0
		p.bgScrollX = 0
		p.bgScrollY = 0
	}
}

// Sizing and specs, dynamic based on the master configuration.
func (p *PTSD) tileSize() uint16 {
	return uint16(p.tileWidth * p.tileHeight * p.colorDepth / 16)
}

func (p *PTSD) tileRAMSize() uint16 {
	return p.tileSize() * 256
}

func (p *PTSD) bgMapSize() uint16 {
	base := 256
	if p.smallBackground {
		base = 128
	}
	return uint16((base/p.tileWidth)*(base/p.tileHeight)) / 2
}

func (p *PTSD) paletteSize() uint16 {
	return uint16(1 << uint(p.colorDepth))
}

func (p *PTSD) paletteRAMSize() uint16 {
	return 16 * p.paletteSize()
}

func (p *PTSD) Tick(c common.CPU) {
	// TODO: When the display gets disabled, paint it black.
	if p.tileRAM != 0 && p.paletteRAM != 0 && time.Since(p.lastFrame) > 50*time.Millisecond {
		pixels, pitch, err := p.texture.Lock(nil)
		if pitch != ptsdWidthPixels*4 {
			panic(fmt.Errorf("unexpected pitch: %d", pitch))
		}
		if err != nil {
			panic(fmt.Errorf("error locking texture: %v", err))
		}

		tiles := c.Memory()[p.tileRAM : p.tileRAM+p.tileRAMSize()]
		palettes := c.Memory()[p.paletteRAM : p.paletteRAM+p.paletteRAMSize()]

		// Bottommost layer: BG colour 0 (even if the BG isn't enabled).
		bgp := palettes[p.bgPalette*p.paletteSize() : (p.bgPalette+1)*p.paletteSize()]
		if len(bgp) <= 0 {
			fmt.Printf("bgp %v, palette size %d, bgPalette %d\n", bgp, p.paletteSize(), p.bgPalette)
		}

		for i := 0; i < ptsdWidthPixels; i++ {
			for j := 0; j < ptsdHeightPixels; j++ {
				p.paint(pixels, i, j, bgp[0])
			}
		}

		// Next layer: sprites under the background, if enabled.
		sprites := []uint16{}
		if p.spriteRAM != 0 {
			sprites = c.Memory()[p.spriteRAM : p.spriteRAM+256]
			p.paintSprites(pixels, tiles, palettes, sprites, true /* under */)
		}

		// Next layer: background if enabled.
		if p.bgMapRAM != 0 {
			bg := c.Memory()[p.bgMapRAM : p.bgMapRAM+p.bgMapSize()]
			p.paintBackground(pixels, tiles, bgp, bg)
		}

		// Last layer: sprites above the background, if enabled.
		if p.spriteRAM != 0 {
			p.paintSprites(pixels, tiles, palettes, sprites, false /* under */)
		}

		// Fully painted, now flip the texture onto the display.
		p.texture.Unlock()
		err = p.renderer.Clear()
		if err != nil {
			panic(fmt.Errorf("failed to clear renderer: %v", err))
		}
		err = p.renderer.Copy(p.texture, &sdl.Rect{0, 0, int32(ptsdWidthPixels), int32(ptsdHeightPixels)},
			&sdl.Rect{0, 0, int32(ptsdWidthPixels * ptsdScaleFactor), int32(ptsdHeightPixels * ptsdScaleFactor)})
		if err != nil {
			panic(fmt.Errorf("failed to copy texture: %v", err))
		}

		p.renderer.Present()
		p.lastFrame = time.Now()
		if p.interrupt != 0 {
			c.AddInterrupt(p.interrupt)
		}
	}
}

func (p *PTSD) paintSprites(pixels []byte, tiles, palettes, sprites []uint16, under bool) {
	// We iterate through the sprites highest to lowest, painting those that are
	// visible and over/under the background.
	for i := 127; i >= 0; i-- {
		s1 := sprites[2*i]
		s2 := sprites[2*i+1]
		u := s1&0x4000 != 0
		large := s1&0x8000 != 0
		if u != under {
			continue
		}

		// Still here, so paint it.
		vFlip := s1&0x2000 != 0
		hFlip := s1&0x1000 != 0
		palNum := (s1 & 0x0f00) >> 8
		tileNum := s1 & 0xff

		baseY := int((s2 >> 8) & 0xff)
		baseX := int(s2 & 0xff)

		tileSpecs := []*tileSpec{&tileSpec{baseX, baseY, tileNum}}

		if large {
			if p.wideSprites {
				tileNum++
				tileSpecs = append(tileSpecs, &tileSpec{baseX + p.tileWidth, baseY, tileNum})
			}

			if p.tallSprites {
				tileNum++
				tileSpecs = append(tileSpecs, &tileSpec{baseX, baseY + p.tileHeight, tileNum})

				if p.wideSprites {
					tileSpecs = append(tileSpecs, &tileSpec{baseX + p.tileWidth, baseY + p.tileHeight, tileNum + 1})
				}
			}

			// Now handle the flips.
			if vFlip && p.tallSprites && p.wideSprites {
				tileSpecs[0].tileNum, tileSpecs[2].tileNum = tileSpecs[2].tileNum, tileSpecs[0].tileNum
				tileSpecs[1].tileNum, tileSpecs[3].tileNum = tileSpecs[3].tileNum, tileSpecs[1].tileNum
			} else if vFlip && p.tallSprites {
				tileSpecs[0].tileNum, tileSpecs[1].tileNum = tileSpecs[1].tileNum, tileSpecs[0].tileNum
			}

			if hFlip && p.wideSprites {
				tileSpecs[0].tileNum, tileSpecs[1].tileNum = tileSpecs[1].tileNum, tileSpecs[0].tileNum
				fmt.Printf("flipping %v %v\n", tileSpecs[0], tileSpecs[1])
				if p.tallSprites {
					tileSpecs[2].tileNum, tileSpecs[3].tileNum = tileSpecs[3].tileNum, tileSpecs[2].tileNum
				}
			}
		}

		for _, spec := range tileSpecs {
			p.paintSpriteTile(pixels, tiles, palettes, palNum, spec.tileNum, vFlip, hFlip, spec.x, spec.y)
		}
	}
}

func (p *PTSD) paintSpriteTile(pixels []byte, tiles, palettes []uint16, palNum, tileNum uint16, vFlip, hFlip bool, baseX, baseY int) {
	for x := 0; x < p.tileWidth; x++ {
		for y := 0; y < p.tileHeight; y++ {
			realX := (baseX + x) & 0xff
			realY := (baseY + y) & 0xff
			if realX < 0 || realY < 0 || realX >= ptsdWidthPixels || realY >= ptsdHeightPixels {
				continue
			}

			tx := x
			ty := y
			if vFlip {
				ty = p.tileHeight - ty - 1
			}
			if hFlip {
				tx = p.tileWidth - tx - 1
			}

			c := p.tileAt(tiles, tileNum, tx, ty) // Color number at this point.
			if c == 0 {
				continue // Transparent, so skip it.
			}
			p.paint(pixels, realX, realY, palettes[palNum*p.paletteSize()+c])
		}
	}
}

func (p *PTSD) paintBackground(pixels []byte, tiles, palette, bg []uint16) {
	tilesAcross := 256 / p.tileWidth
	for y := 0; y < ptsdHeightPixels; y++ {
		for x := 0; x < ptsdWidthPixels; x++ {
			// Adjust for scrolling to find the background-relative coordinates.
			bgX := (x + int(p.bgScrollX)) & 0xff
			bgY := (y + int(p.bgScrollY)) & 0xff

			mapOffset := (bgX / p.tileWidth) + ((bgY / p.tileHeight) * tilesAcross)
			tile := bg[mapOffset>>1] // Two tiles per word.
			if tile&1 != 0 {
				tile = tile & 0xff
			} else {
				tile = tile >> 8
			}

			c := p.tileAt(tiles, tile, bgX&(p.tileWidth-1), bgY&(p.tileHeight-1))
			if c == 0 {
				continue
			}
			p.paint(pixels, x, y, palette[c])
		}
	}
}

// Returns the colour number at the given position in the tile.
func (p *PTSD) tileAt(tiles []uint16, tileNumber uint16, x, y int) uint16 {
	size := p.tileSize()
	t := tiles[tileNumber*size : (tileNumber+1)*size]
	// The region we want to read is colorDepth bits wide, and begins at bit
	// (y * tileWidth + x) * colorDepth.
	bit := p.colorDepth * (y*p.tileWidth + x)
	index := bit >> 4
	shift := uint(16 - (bit & 0xf) - p.colorDepth)
	mask := p.paletteSize() - 1
	return (t[index] >> shift) & mask
}

// Takes a full colour value and writes it into the texture.
// The format is 0000rrrrggggbbbb, texture's format is ARGB8888.
func (p *PTSD) paint(pixels []byte, x, y int, c uint16) {
	offset := uintptr(ptsdWidthPixels)*4*uintptr(y) + 4*uintptr(x)
	if offset < 0 || offset > uintptr(ptsdWidthPixels*ptsdHeightPixels*4) {
		panic(fmt.Errorf("drawing outside legal region: (%d, %d) = %x\n", x, y, offset))
	}

	r := (c >> 8) & 0xf
	r = r | (r << 4)
	g := (c >> 4) & 0xf
	g = g | (g << 4)
	b := c & 0xf
	b = b | (b << 4)

	pixels[offset+3] = 0xff
	pixels[offset+2] = byte(r)
	pixels[offset+1] = byte(g)
	pixels[offset] = byte(b)
}

// TODO
func (p *PTSD) Cleanup() {}

func NewPTSD() common.Device {
	p := new(PTSD)

	p.lastFrame = time.Now()

	runtime.LockOSThread() // Latch this goroutine to the same thread for SDL.
	sdl.Init(sdl.INIT_VIDEO)
	window, err := sdl.CreateWindow("PTSD", sdl.WINDOWPOS_UNDEFINED,
		sdl.WINDOWPOS_UNDEFINED, int32(ptsdWidthPixels*ptsdScaleFactor),
		int32(ptsdHeightPixels*ptsdScaleFactor), sdl.WINDOW_SHOWN)
	if err != nil {
		panic(fmt.Errorf("failed to create window: %v", err))
	}

	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)
	if err != nil {
		panic(fmt.Errorf("failed to create renderer: %v", err))
	}

	texture, err := renderer.CreateTexture(sdl.PIXELFORMAT_ARGB8888,
		sdl.TEXTUREACCESS_STREAMING, int32(ptsdWidthPixels), int32(ptsdHeightPixels))
	if err != nil {
		panic(fmt.Errorf("failed to create texture: %v", err))
	}

	p.window = window
	p.renderer = renderer
	p.texture = texture
	return p
}

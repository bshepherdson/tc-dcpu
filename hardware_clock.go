package main

import "time"

type Clock struct {
	tickInterval time.Duration
	lastTick     time.Time
	ticking      bool
	ticksSince   uint16
	intMessage   uint16
	startupTime  time.Time
}

/*
type device interface {
	// Returns the device ID, version and manufacturer.
	DeviceDetails() (uint32, uint16, uint32)
	Interrupt(*dcpu)
	Tick(*dcpu)
	Cleanup()
}
*/

func (c *Clock) DeviceDetails() (uint32, uint16, uint32) {
	return 0x12d1b402, 2, 0x1c6c8b36
}

func (c *Clock) Interrupt(d *dcpu) {
	switch d.regs[ra] {
	case 0x0000: // SET_SPEED
		if d.regs[rb] == 0 {
			c.ticking = false
		} else {
			c.ticking = true
			c.tickInterval = time.Second * time.Duration(d.regs[rb]) / 60
			c.lastTick = time.Now()
		}
	case 0x0001: // GET_TICKS
		d.regs[rc] = c.ticksSince
		c.ticksSince = 0
	case 0x0002: // SET_INT
		c.intMessage = d.regs[rb]
	case 0x0010: // REAL_TIME
		t := time.Now()
		d.regs[rb] = uint16(t.Year())
		d.regs[rc] = (uint16(t.Month()) << 8) | uint16(t.Day())
		d.regs[rx] = (uint16(t.Hour()) << 8) | uint16(t.Minute())
		d.regs[ry] = uint16(t.Second())
		d.regs[rz] = uint16(t.Nanosecond() / 1000000)
	case 0x0011: // RUN_TIME
		diff := time.Since(c.startupTime)
		d.regs[rb] = 0
		d.regs[rc] = 0
		d.regs[rx] = (uint16(diff.Hours()) << 8) | uint16(diff.Minutes())
		d.regs[ry] = uint16(diff.Seconds())
		d.regs[rz] = uint16(diff.Nanoseconds() / 1000000)
	case 0x0012: // SET_REAL_TIME
		// Do nothing.
	case 0xffff: // RESET
		c.reset()
	}
}

func (c *Clock) Tick(d *dcpu) {
	c.ticksSince++
	if c.ticking && c.intMessage != 0 && time.Since(c.lastTick) >= c.tickInterval {
		c.lastTick = time.Now()
		d.addInterrupt(c.intMessage)
	}
}

func (c *Clock) Cleanup() {}

func (c *Clock) reset() {
	c.ticking = false
	c.ticksSince = 0
	c.intMessage = 0
	c.startupTime = time.Now()
}

func NewClock() device {
	c := new(Clock)
	c.reset()
	return c
}

package main

import (
	"emulator/common"
	"time"
)

type Clock struct {
	tickInterval time.Duration
	lastTick     time.Time
	ticking      bool
	ticksSince   uint16
	intMessage   uint16
	startupTime  time.Time
}

func (c *Clock) DeviceDetails() (uint32, uint16, uint32) {
	return 0x12d1b402, 2, 0x1c6c8b36
}

func (c *Clock) Interrupt(d common.CPU) {
	switch d.ReadReg(0) {
	case 0x0000: // SET_SPEED
		if d.ReadReg(1) == 0 {
			c.ticking = false
		} else {
			c.ticking = true
			c.tickInterval = time.Second * time.Duration(d.ReadReg(1)) / 60
			c.lastTick = time.Now()
		}
	case 0x0001: // GET_TICKS
		d.WriteReg(2, c.ticksSince)
		c.ticksSince = 0
	case 0x0002: // SET_INT
		c.intMessage = d.ReadReg(1)
	case 0x0010: // REAL_TIME
		t := time.Now()
		d.WriteReg(1, uint16(t.Year()))
		d.WriteReg(2, (uint16(t.Month())<<8)|uint16(t.Day()))
		d.WriteReg(3, (uint16(t.Hour())<<8)|uint16(t.Minute()))
		d.WriteReg(4, uint16(t.Second()))
		d.WriteReg(5, uint16(t.Nanosecond()/1000000))
	case 0x0011: // RUN_TIME
		diff := time.Since(c.startupTime)
		d.WriteReg(1, 0)
		d.WriteReg(2, 0)
		d.WriteReg(3, (uint16(diff.Hours())<<8)|uint16(diff.Minutes()))
		d.WriteReg(4, uint16(diff.Seconds()))
		d.WriteReg(5, uint16(diff.Nanoseconds()/1000000))
	case 0x0012: // SET_REAL_TIME
		// Do nothing.
	case 0xffff: // RESET
		c.reset()
	}
}

func (c *Clock) Tick(d common.CPU) {
	c.ticksSince++
	if c.ticking && c.intMessage != 0 && time.Since(c.lastTick) >= c.tickInterval {
		c.lastTick = time.Now()
		d.AddInterrupt(c.intMessage)
	}
}

func (c *Clock) Cleanup() {}

func (c *Clock) reset() {
	c.ticking = false
	c.ticksSince = 0
	c.intMessage = 0
	c.startupTime = time.Now()
}

func NewClock() common.Device {
	c := new(Clock)
	c.reset()
	return c
}

package main

import (
	"math/rand"
	"time"

	"github.com/shepheb/tc-dcpu/common"
)

type RNG struct {
	r *rand.Rand
}

func (rng *RNG) DeviceDetails() (uint32, uint16, uint32) {
	return 0x13e2dc12, 1, 0
}

func (rng *RNG) Interrupt(c common.CPU) {
	switch c.ReadReg(0) {
	case 0x0000: // SET_SEED
		seed := int64(c.ReadReg(1))
		if seed == 0 {
			seed = time.Now().UnixNano()
		}
		rng.r.Seed(seed)
	case 0x0001: // GET_RANDOM
		c.WriteReg(2, uint16(rng.r.Uint32()))
	case 0xffff: // RESET
		rng.reset()
	}
}

func (rng *RNG) Tick(d common.CPU) {}
func (rng *RNG) Cleanup()          {}

func (rng *RNG) reset() {
	rng.r = rand.New(rand.NewSource(time.Now().UnixNano()))
}

func NewRNG() common.Device {
	rng := new(RNG)
	rng.reset()
	return rng
}

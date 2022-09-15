package main

import "github.com/bshepherdson/tc-dcpu/common"

var deviceTypes = map[string]func(cpu common.CPU) common.Device{
	"keyboard": func(cpu common.CPU) common.Device { return new(Keyboard) },
	"rng":      func(cpu common.CPU) common.Device { return NewRNG() },
	"lem1802":  func(cpu common.CPU) common.Device { return NewLEM1802() },
	"ptsd":     func(cpu common.CPU) common.Device { return NewPTSD() },
	"clock":    func(cpu common.CPU) common.Device { return NewClock() },
	"m35fd":    func(cpu common.CPU) common.Device { return NewM35FD(cpu) },
	"hsdp-1d":  func(cpu common.CPU) common.Device { return NewHSDP1D() },
	"imva":     func(cpu common.CPU) common.Device { return NewIMVA() },
	"serial":   func(cpu common.CPU) common.Device { return NewSerial() },
}

var deviceDescriptions = map[string]string{
	"lem1802":  "Nya Elektriska LEM1802 display",
	"ptsd":     "Parallax Tiled Sprite Display",
	"m35fd":    "Mackapar Media 3.5\" floppy drive",
	"keyboard": "Generic keyboard",
	"clock":    "Generic clock",
	"rng":      "Generic random number generator",
	"hsdp-1d":  "Chartronics High Speed Data Printer",
	"imva":     "Interlaced Monochrome Videographic Adaptor",
	"serial":   "KaiComm SSI serial link",
}

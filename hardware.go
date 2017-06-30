package main

import "emulator/common"

var deviceTypes = map[string]func(cpu common.CPU) common.Device{
	"keyboard": func(cpu common.CPU) common.Device { return new(Keyboard) },
	"lem1802":  func(cpu common.CPU) common.Device { return NewLEM1802() },
	"clock":    func(cpu common.CPU) common.Device { return NewClock() },
	"m35fd":    func(cpu common.CPU) common.Device { return NewM35FD(cpu) },
	"hsdp-1d":  func(cpu common.CPU) common.Device { return NewHSDP1D() },
}

var deviceDescriptions = map[string]string{
	"lem1802":  "Nya Elektriska LEM1802 display",
	"m35fd":    "Mackapar Media 3.5\" floppy drive",
	"keyboard": "Generic keyboard",
	"clock":    "Generic clock",
	"hsdp-1d":  "Chartronics High Speed Data Printer",
}

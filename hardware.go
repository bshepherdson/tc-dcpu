package main

import "emulator/common"

var deviceTypes = map[string]func() common.Device{
	"keyboard": func() common.Device { return new(Keyboard) },
	"lem1802":  func() common.Device { return NewLEM1802() },
	"clock":    func() common.Device { return NewClock() },
	"m35fd":    func() common.Device { return NewM35FD() },
}

var deviceDescriptions = map[string]string{
	"lem1802":  "Nya Elektriska LEM1802 display",
	"m35fd":    "Mackapar Media 3.5\" floppy drive",
	"keyboard": "Generic keyboard",
	"clock":    "Generic clock",
}

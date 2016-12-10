package main

var deviceTypes = map[string]func() device{
	"keyboard": func() device { return new(Keyboard) },
	"lem1802":  func() device { return NewLEM1802() },
	"clock":    func() device { return NewClock() },
	"m35fd":    func() device { return NewM35FD() },
}

var deviceDescriptions = map[string]string{
	"lem1802":  "Nya Elektriska LEM1802 display",
	"m35fd":    "Mackapar Media 3.5\" floppy drive",
	"keyboard": "Generic keyboard",
	"clock":    "Generic clock",
}

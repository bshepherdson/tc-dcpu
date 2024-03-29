package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/bshepherdson/tc-dcpu/common"
	"github.com/bshepherdson/tc-dcpu/dcpu"
	"github.com/bshepherdson/tc-dcpu/mocha"
	"github.com/bshepherdson/tc-dcpu/rq"
)

func usage() {
	flag.PrintDefaults()
}

func dumpDeviceList() {
	for name, desc := range deviceDescriptions {
		fmt.Printf("%-20s %s\n", name, desc)
	}
}

var diskFileNames []string
var Turbo bool = false

func main() {
	// Turbo mode for now, not working on the timing.
	deviceList := flag.String("hw", "keyboard,lem1802,m35fd,clock,rng,hsdp-1d,serial",
		"List of hardware devices. See -dump-hw for a list of devices.")
	dumpDevices := flag.Bool("dump-hw", false,
		"Dump a list of hardware devices and exit.")
	arch := flag.String("arch", "dcpu", "CPU architecture. 'dcpu', 'rq' and 'mocha' are supported")
	disks := flag.String("disk", "", "Filenames of the disk to load (comma-separated.")
	disassemble := flag.Bool("disassemble", false, "Disassemble the ROM to stdout")
	turboFlag := flag.Bool("turbo", false, "True to start in turbo (unlimited speed) mode. Default: false, native speed (DCPU: 100kHz)")
	script := flag.String("script", "", "Script file to run.")

	flag.Parse()
	if !flag.Parsed() {
		fmt.Printf("Usage: %s [options] <ROM file>\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	inputReader = bufio.NewReader(os.Stdin)

	if *dumpDevices {
		dumpDeviceList()
		return
	}

	romFile := flag.Arg(0)
	if romFile == "" {
		fmt.Printf("Missing required ROM file name!\n")
		fmt.Printf("Usage: %s [options] <ROM file>\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	// Copy the ROM into memory.
	rom, err := ioutil.ReadFile(romFile)
	if err != nil {
		panic("failed to open ROM file")
	}

	// Set up the CPU and populate the devices.
	var cpu common.CPU
	switch *arch {
	case "dcpu":
		cpu = dcpu.NewDCPU()
	case "rq":
		cpu = rq.NewRQ()
	case "mocha":
		cpu = mocha.NewMocha86k()
	default:
		fmt.Printf("Unknown CPU architecture: %s\n", *arch)
	}

	// Copy the ROM into memory.
	mem := cpu.Memory()
	for i := 0; i < len(rom); i += 2 {
		mem[i>>1] = (uint16(rom[i]) << 8) | uint16(rom[i+1])
	}

	if *disassemble {
		cpu.Disassemble()
		return
	}

	common.InputReader = bufio.NewReader(os.Stdin)

	deviceNames := strings.Split(*deviceList, ",")
	diskFileNames = strings.Split(*disks, ",")
	for _, d := range deviceNames {
		if dt, ok := deviceTypes[d]; ok {
			fmt.Printf("Loading device: %s\n", d)
			cpu.AddDevice(dt(cpu))
		} else {
			fmt.Printf("Unknown device: %s\n", d)
			dumpDeviceList()
			return
		}
	}

	Turbo = *turboFlag

	if *script != "" {
		RunScript(cpu, *script)
	}

	run(cpu)
}

var inputReader *bufio.Reader

func debugConsole(c common.CPU) {
	// Print the prompt and handle the input.
	c.DebugPrompt()
	in, err := inputReader.ReadString('\n')
	if err != nil {
		fmt.Printf("error while reading input: %v\n", err)
		return
	}

	// Try to parse in. First split on spaces.
	args := strings.Split(strings.TrimSpace(in), " ")
	if cmd, ok := common.DebugCommands[args[0]]; ok {
		cmd.Run(c, args)
	} else {
		fmt.Printf("Unknown command '%s'\n", args[0])
		fmt.Printf("Commands:\n")
		for key, dbg := range common.DebugCommands {
			fmt.Printf("%s\t%s\n", key, dbg.Describe())
		}
	}
}

func fKey(c common.CPU, key int) {
	switch key {
	case 1: // F1 - help
		fmt.Println("=== Emulator commands ===")
		fmt.Println("F1\tShow this help")
		fmt.Println("F2\tStart debugging")
		fmt.Println("F3\tResume running")
		fmt.Println("F4\tTurbo speed toggle")
		fmt.Println("F6\tEject/insert disk")

	case 2: // F2 - start debugging
		*c.Debugging() = true

	case 3: // F3 - stop debugging
		*c.Debugging() = false

	case 4: // F4 - toggle turbo
		Turbo = !Turbo
		if Turbo {
			fmt.Println("Turbo enabled: speed unlimited")
		} else {
			fmt.Println("Turbo disabled: running at nominal speed")
		}

	case 6: // F6 - eject/insert disk
		var drive *M35FD
		for _, d := range c.Devices() {
			switch d2 := d.(type) {
			case *M35FD:
				drive = d2
			}
		}
		if drive == nil {
			fmt.Println("No disk device found.")
			return
		}

		if drive.file != nil {
			drive.Eject(c)
			fmt.Println("Disk ejected. F6 again to insert.")
		} else {
			fmt.Println("Enter the new disk location.")
			var newLocation string
			fmt.Scanln(&newLocation)
			if drive.Open(c, newLocation) {
				fmt.Println("Disk loaded successfully")
			}
		}
	}
}

func speedLogger(cycles *int) {
	lastCount := 0
	ticker := time.Tick(5 * time.Second)
	for {
		<-ticker
		newCount := *cycles
		fmt.Printf("Ran %d cycles in 5 seconds: %.2f kHz\n", newCount-lastCount,
			float64(newCount-lastCount)/5000)
		lastCount = newCount
	}
}

func run(c common.CPU) {
	// Ticks at 100Hz, so I run c.Speed()/100 cycles per tick, for about the right
	// net speed over time.
	ticker := time.Tick(10 * time.Millisecond)
	cpuChunkSize := c.Speed() / 100
	cycles := 0
	combinedCycles := 0
	go speedLogger(&combinedCycles)

	// Repeatedly try to run the CPU operation, stopping on a debug to show the
	// console.
	for {
		for !*c.Debugging() {
			cycles++
			combinedCycles++
			if !Turbo && cycles >= cpuChunkSize {
				_ = <-ticker
				cycles = 0
			}

			c.RunOp()
		}

		debugConsole(c)
	}
}

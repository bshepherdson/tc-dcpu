package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"emulator/common"
	"emulator/dcpu"
)

func usage() {
	flag.PrintDefaults()
}

func dumpDeviceList() {
	for name, desc := range deviceDescriptions {
		fmt.Printf("%s\t%s\n", name, desc)
	}
}

var diskFileNames []string

func main() {
	// Turbo mode for now, not working on the timing.
	deviceList := flag.String("hw", "keyboard,lem1802,m35fd,clock",
		"List of hardware devices. See -dump-hw for a list of devices.")
	dumpDevices := flag.Bool("dump-hw", false,
		"Dump a list of hardware devices and exit.")
	arch := flag.String("arch", "dcpu", "CPU architecture. 'dcpu' and 'rq' are supported")
	disks := flag.String("disk", "", "Filenames of the disk to load (comma-separated.")
	disassemble := flag.Bool("disassemble", false, "Disassemble the ROM to stdout")

	flag.Parse()
	if !flag.Parsed() {
		fmt.Printf("Usage: %s [options] <ROM file>\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	romFile := flag.Arg(0)
	if romFile == "" {
		fmt.Printf("Missing required ROM file name!\n")
		fmt.Printf("Usage: %s [options] <ROM file>\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(1)
	}

	if *dumpDevices {
		dumpDeviceList()
		return
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
	//case "rq":
	//	cpu = rq.NewRQ()
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
			cpu.AddDevice(dt())
		} else {
			fmt.Printf("Unknown device: %s\n", d)
			dumpDeviceList()
			return
		}
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

func run(c common.CPU) {
	// Repeatedly try to run the CPU operation, stopping on a debug to show the
	// console.
	for {
		for !*c.Debugging() {
			c.RunOp()
		}

		debugConsole(c)
	}
}

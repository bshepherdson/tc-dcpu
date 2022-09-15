package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"

	"github.com/bshepherdson/tc-dcpu/common"
)

type command func(c common.CPU, args []string)

var cmds = map[string]command{
	"eject": cmdEject,
	"mount": cmdMount,
	"type":  cmdTypeChar,
	"send":  cmdSendString,
	"run":   cmdRun,
	"quit":  cmdQuit,
}

func cmdEject(c common.CPU, args []string) {
	for _, d := range c.Devices() {
		if d2, ok := d.(*M35FD); ok {
			d2.Eject(c)
			return
		}
	}
}

func cmdMount(c common.CPU, args []string) {
	if len(args) < 1 {
		panic("'mount' requires a filename as an argment.")
	}
	for _, d := range c.Devices() {
		if d2, ok := d.(*M35FD); ok {
			d2.Open(c, args[0])
			return
		}
	}
}

func cmdTypeChar(c common.CPU, args []string) {
	if len(args) < 1 {
		panic("'type' requires a single character as an argument")
	}
	for _, d := range c.Devices() {
		if d2, ok := d.(*Keyboard); ok {
			d2.Enqueue(uint16(args[0][0]))
			return
		}
	}
}

func cmdSendString(c common.CPU, args []string) {
	if len(args) < 1 {
		panic("'send' requires 1 or more arguments to type")
	}
	for _, d := range c.Devices() {
		if d2, ok := d.(*Keyboard); ok {
			for _, s := range args {
				for _, ch := range s {
					d2.Enqueue(uint16(ch))
				}
				d2.Enqueue(uint16(' '))
			}
			d2.Enqueue(uint16('\x11'))
			return
		}
	}
}

func cmdQuit(c common.CPU, args []string) {
	os.Exit(0)
}

func cmdRun(c common.CPU, args []string) {
	if len(args) < 1 {
		panic("'run' requires an argument giving the cycle count")
	}

	cycles, err := strconv.ParseUint(args[0], 10, 64)
	if err != nil {
		panic("'run' requires a positive integer argument")
	}

	for i := uint64(0); i < cycles; i++ {
		c.RunOp()
	}
}

func RunScript(c common.CPU, file string) {
	contents, err := ioutil.ReadFile(file)
	if err != nil {
		panic(err)
	}

	lines := strings.Split(string(contents), "\n")
	for _, line := range lines {
		if len(line) == 0 {
			continue
		}

		args := strings.Split(line, " ")
		if cmd, ok := cmds[args[0]]; ok {
			cmd(c, args[1:])
		} else {
			panic(fmt.Errorf("Unknown command '%s'", args[0]))
		}
	}
}

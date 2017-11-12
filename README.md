# TC-DCPU: An Emulator

This is an emulator, written with Go and SDL2, for the
[Techcompliant](https://github.com/techcompliant/TC-Specs) flavour of the
DCPU-16.

## Processors

### DCPU-16

The DCPU-16 core is compatible with the final 1.7 version of Notch's spec,
augmented with TC's
[extensions](https://github.com/techcompliant/TC-Specs/blob/master/CPU/clock.md)
to add the `LOG`, `BRK` and `HLT` opcodes.


### Risque-16

The [Risque-16](https://github.com/shepheb/risque16) is a RISC-style chip
inspired by the ARM and Thumb architectures.

It supports the same hardware as the DCPU-16. Instructions generally do less per
cycle than the DCPU, but the simpler core executes faster, yielding similar
performance.

Pass the `-cpu rq16` flag to switch to Risque-16 mode.

## Getting Started

Install Go, and then `go get github.com/shepheb/tc-dcpu`.

## Hardware

TC-DCPU supports the following devices, with links to the Techcompliant
[specs](https://github.com/techcompliant/TC-Specs):

- [LEM1802](https://github.com/techcompliant/TC-Specs/blob/master/Displays/LEM1802.txt) colour character display
- [IMVA](https://github.com/techcompliant/TC-Specs/blob/master/Displays/IMVA.md)
  monochrome pixel display
- [Generic keyboard](https://github.com/techcompliant/TC-Specs/blob/master/Input/keyboard.md) (TC's enhanced, compatible version with "raw mode")
- [Generic
  clock](https://github.com/techcompliant/TC-Specs/blob/master/CPU/clock.md)
  with TC's extensions to add time-since-startup and absolute time.
- [M35fd](https://github.com/techcompliant/TC-Specs/blob/master/Storage/m35fd.txt)
  floppy drive
- [HSDP-1D](https://github.com/techcompliant/TC-Specs/blob/master/Simple%20Outputs/HSDP-1D.md)
  high-speed printer.

All devices publish the Techcompliant new IDs, not the old Notch ones.

## Usage

`tc-dcpu [options] <ROM file>`

### Command-line Options

- `-arch cpu`: Pass `dcpu` or `rq` to select the DCPU-16 or Risque-16
  (default: `dcpu`)
- `-disassemble`: Dump a human-readable disassembly of the ROM, instead of
  executing it. (`-arch` is honoured; other options are ignored.)
- `-disk disk_file`: Specifies the filename to be loaded into the M35fd disk
  drive.
- `-dump-hw`: Pass this to exit immediately with a list of the available
  hardware devices, and their names as understood by `-hw`.
- `-hw device1,device2,...`: Pass a comma-separated list of device names as
  given by `-dump-hw`; devices will be created in that order.
    - Default: `keyboard,lem1802,m35fd,clock,hsdp-1d`
- `script script_file`: Run the script in this file (see below for the scripting
  language). This allows for automated testing and so on.
- `turbo`: Pass this to start in "turbo" mode, where the speed is unlimited.
  Default: false, 100kHz for DCPU-16 and 200kHz for Risque-16.


### Runtime Keybindings

Pressing F-keys engages special functions in the emulator.

- **F1**: Show a help guide.
- **F2**: Start the debugger
- **F3**: Resume running, when debugging
- **F4**: Toggle turbo speed
- **F6**: Eject/insert disk (assumes a single disk drive)


### Debugging

On a `LOG` opcode, the emulator emits the value in hex, decimal and as a
character.

Encountering a `BRK` opcode triggers the debugger. The debugger has built-in
help.


### Scripting

A rudimentary scripting language is included, to help automate testing and
bootstrapped applications. It expects one command per line, and has basically
no syntax. (No comments, no variables, etc.)

- `eject`: Ejects the disk. Assumes a single disk drive.
- `mount disk_file`: Mounts the named file as the disk. Must be ejected already.
  Likewise assumes a single disk drive.
- `type char`: Types a single character.
- `send string...`: Sends the words typed, separated by spaces and ended by a
  newline.
- `run cycles`: Runs that many cycles before continuing the script.
- `quit`: Quits the emulator.

The scripting language is processor-agnostic.

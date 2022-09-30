package mocha

import (
	"fmt"
	"reflect"
	"runtime"
	"strings"
)

// Interpreting the complicated addressing modes and other values is usually
// done in hardware with microcoding, and that approach seems sensible here too.
// The microcode routines are written as operations for a Forth-like stack
// machine.
//
// This file defines the common, shared microcode routines used by all the
// different implementation files.
type mcState struct {
	stack [256]uint32
	sp    int
	ea    uint32
}

func (st *mcState) push(x uint32) {
	st.sp++
	st.stack[st.sp] = x
}

func (st *mcState) pop() uint32 {
	val := st.stack[st.sp]
	st.sp--
	return val
}

func (st *mcState) peek() uint32 {
	return st.stack[st.sp]
}

func (st *mcState) tos() *uint32 {
	return &st.stack[st.sp]
}

func (st *mcState) printStack() {
	var strs []string
	for i := 0; i <= st.sp; i++ {
		strs = append(strs, fmt.Sprintf("%08x", st.stack[i]))
	}
	fmt.Println(strings.Join(strs, " "))
}

func newMcState() *mcState {
	return &mcState{sp: -1}
}

const debug = false

// Runs any number of threads over the same state.
// Returns TOS, or 0 if empty.
func (c *m86k) runMC(threads ...microThread) uint32 {
	s := newMcState()
	if debug {
		fmt.Printf("\n\n============\n")
	}
	for _, thread := range threads {
		for _, op := range thread {
			if debug {
				fmt.Println(runtime.FuncForPC(reflect.ValueOf(op).Pointer()).Name())
			}
			op(c, s)
			if debug {
				s.printStack()
			}
		}
	}
	if debug {
		fmt.Printf("============\n")
	}
	if s.sp >= 0 {
		return s.pop()
	}
	return 0
}

type mc func(c *m86k, st *mcState)

type microThread []mc

// Can be used to join a series of microcode operations together, as commonly
// used when combining
//func joinThreads(threads ...microThread) microThread {
//	var ret []mc
//	for _, thread := range threads {
//		for _, op := range thread {
//			ret = append(ret, op)
//		}
//	}
//	return ret
//}

// ( reg -- value )
func mcReadReg32(c *m86k, s *mcState) {
	reg := s.pop()
	if reg < 0 || reg > 7 {
		panic(fmt.Sprintf("Tried to read bad register number (PC $%08x, reg $%04x)", c.pc, reg))
	}
	s.push(c.regs[reg])
}

func mcReadReg16(c *m86k, s *mcState) {
	mcReadReg32(c, s)
	mcClip16(c, s)
}

// ( u32 -- u16 )
func mcClip16(c *m86k, s *mcState) {
	*s.tos() &= 0xffff
}

// ( u32 -- u16lo u16hi )
func mcSplitWords(c *m86k, s *mcState) {
	w := s.pop()
	s.push(w & 0xffff)
	s.push(w >> 16)
}

// ( u16lo u16hi -- u32 )
func mcJoinWords(c *m86k, s *mcState) {
	s.push((s.pop() << 16) | (s.pop() & 0xffff))
}

// ( u32 reg -- )
func mcWriteReg32(c *m86k, s *mcState) {
	reg := s.pop()
	if reg < 0 || reg > 7 {
		panic("Tried to read bad register number")
	}

	c.regs[reg] = s.pop()
}

// ( u16 reg -- )
func mcWriteReg16(c *m86k, s *mcState) {
	reg := s.pop()
	if reg < 0 || reg > 7 {
		panic("Tried to read bad register number")
	}

	c.regs[reg] = writeLow(c.regs[reg], uint16(s.pop()))
}

// ( addr32 -- u16 )
func mcReadWord(c *m86k, s *mcState) {
	addr := s.pop()
	s.push(uint32(c.readWord(addr)))
	c.cycles++
}

// ( addr32 -- u32 )
func mcReadLongword(c *m86k, s *mcState) {
	addr := s.pop()
	lo := c.readWord(addr)
	hi := c.readWord(addr + 1)
	//fmt.Printf("mcReadLongword: $%08x = $%04x + $%04x = $%08x\n", addr, lo, hi, (uint32(hi)<<16)|uint32(lo))
	s.push((uint32(hi) << 16) | uint32(lo))
	c.cycles += 2
}

// ( u16 addr32 -- )
func mcWriteWord(c *m86k, s *mcState) {
	addr := s.pop()
	c.writeWord(addr, uint16(s.pop()))
	c.cycles += 1
}

// ( u32 addr32 -- )
func mcWriteLongword(c *m86k, s *mcState) {
	addr := s.pop()
	value := s.pop()
	c.writeWord(addr, uint16(value))
	c.writeWord(addr+1, uint16(value>>16))
	c.cycles += 2
}

// ( u16 -- s32 )
func mcSignExtend(c *m86k, s *mcState) {
	u16 := s.pop()
	s.push(uint32(int32(int16(u16))))
}

// ( u16 -- )
func mcQueueInt(c *m86k, s *mcState) {
	c.AddInterrupt(uint16(s.pop()))
}

// ( -- pc )
func mcGetPC(c *m86k, s *mcState) {
	s.push(c.pc)
}

// ( -- sp )
func mcGetSP(c *m86k, s *mcState) {
	s.push(c.sp)
}

// ( -- ex )
func mcGetEX(c *m86k, s *mcState) {
	s.push(c.ex)
}

// ( -- ia )
func mcGetIA(c *m86k, s *mcState) {
	s.push(uint32(c.ia))
}

// ( pc -- )
func mcPutPC(c *m86k, s *mcState) {
	c.pc = s.pop()
}

// ( sp -- )
func mcPutSP(c *m86k, s *mcState) {
	c.sp = s.pop()
}

// ( ex -- )
func mcPutEX(c *m86k, s *mcState) {
	c.ex = s.pop()
}

// ( ia -- )
func mcPutIA(c *m86k, s *mcState) {
	c.ia = uint16(s.pop())
}

// ( -- u16 ), bumps PC
func mcConsumePCWord(c *m86k, s *mcState) {
	s.push(c.pc)
	mcReadWord(c, s)
	c.pc++
}

// ( -- u32 ), bumps PCx2
func mcConsumePCLongword(c *m86k, s *mcState) {
	s.push(c.pc)
	mcReadLongword(c, s)
	c.pc += 2
}

func mcPushWord(c *m86k, s *mcState) {
	c.sp--
	s.push(c.sp)
	mcWriteWord(c, s)
}

func mcPushLongword(c *m86k, s *mcState) {
	c.sp -= 2
	s.push(c.sp)
	mcWriteLongword(c, s)
}

func mcPopWord(c *m86k, s *mcState) {
	s.push(c.sp)
	c.sp++
	mcReadWord(c, s)
}
func mcPopLongword(c *m86k, s *mcState) {
	s.push(c.sp)
	c.sp += 2
	mcReadLongword(c, s)
}

// Meta: Given a literal value, returns an mc that pushes it.
func mcLit(value uint32) mc {
	return func(c *m86k, s *mcState) {
		s.push(value)
	}
}
func mcLit16(value uint16) mc {
	return mcLit(uint32(value))
}

func mcCycles(value int) mc {
	return func(c *m86k, s *mcState) {
		c.cycles += value
	}
}

func mcSetEA(c *m86k, s *mcState) {
	s.ea = s.pop()
}

func mcPushEA(c *m86k, s *mcState) {
	s.push(s.ea)
}

// Stack ops
func mcDup(c *m86k, s *mcState) {
	s.push(s.peek())
}
func mcDrop(c *m86k, s *mcState) {
	s.pop()
}
func mcSwap(c *m86k, s *mcState) {
	top := s.pop()
	next := s.pop()
	s.push(top)
	s.push(next)
}

// Arithmetic - these are not full operations, just stack shuffling.
func mcPlus(c *m86k, s *mcState) {
	s.push(s.pop() + s.pop())
}

func mcMinus(c *m86k, s *mcState) {
	top := s.pop()
	s.push(s.pop() - top)
}

// These are fit to be called as operations; they account for cycles.
func mcAnd(c *m86k, s *mcState) {
	s.push(s.pop() & s.pop())
	c.cycles++
}
func mcBor(c *m86k, s *mcState) {
	s.push(s.pop() | s.pop())
	c.cycles++
}
func mcXor(c *m86k, s *mcState) {
	s.push(s.pop() ^ s.pop())
	c.cycles++
}

func mcNot(c *m86k, s *mcState) {
	s.push(s.pop() ^ 0xffffffff)
	c.cycles++
}

func shortLong(longwords bool, short, long mc) mc {
	if longwords {
		return long
	}
	return short
}

// Does nothing, useful for shortLong when there's nothing to do in one case.
func mcNop(c *m86k, s *mcState) {}

// ( cond? -- ) Sets the skipping field if the condition *fails*
// Therefore eg. IFE should set nonzero if the args are equal, then this will
// *not* skip. Double negatives are hard.
func mcSetSkip(c *m86k, s *mcState) {
	c.skipping = s.pop() == 0
}

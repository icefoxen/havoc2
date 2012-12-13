#ifndef _DEBUG_H
#define _DEBUG_H

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "hw.h"

extern vm* debugVM;

// Okay... this function halts the VM
// and enters a REPL that gives you direct
// access to its state.  Things it should do:
// Dump registers
// Dump memory
// Set registers
// Set memory
// Fill memory area
// Resume execution
void debug();

void printRegs(vm* c);

void printMem(vm* c, uint64_t start, uint64_t length);

void setMem(vm* c, uint64_t start, uint64_t length, uint64_t val);

void printHelp(void);

void setupSignalHandlers();

#endif // _DEBUG_H

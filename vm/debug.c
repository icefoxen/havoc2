// Grumble mutter fucking POSIX versions...  see man feature_test_macros
#define _POSIX_C_SOURCE 200112L
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include "debug.h"
#include "cpu.h"

void mHelp(void) {
   printf("m address [length]\n");
   printf("\tDump memory\n");
}

void rHelp(void) {
   printf("r\n");
   printf("\tPrint registers\n");
}

void sHelp(void) {
   printf("s reg value\n");
   printf("\tSet general-purpose register\n");
}

void iHelp(void) {
   printf("i reg value\n");
   printf("\tSet system register\n");
}

void nHelp(void) {
   printf("n address [length] value\n");
   printf("\tSet memory (octas)\n");
}

void qHelp(void) {
   printf("q\n");
   printf("\tResume execution\n");
}

void xHelp(void) {
   printf("x\n");
   printf("\tExit interpreter\n");
}

// Bad shit can happen anywhere, and it's often useful to get into
// the debugger even if we don't have a reference to the vm handy.
vm* debugVM = NULL;
// So, we use a global.  This is set in main.c
// right before setupSignalHandlers is called.
// These should happen as close together as possible,
// so the segfault handler doesn't try dumping registers
// from debugVM when it doesn't point anywhere yet!


static const int strLen = 80;
void debug() {
   printf("Debugger entered.\n");
   char buf[strLen];
   vm* c = debugVM;
   if(c == NULL) {
      printf("Oh snap!  debugVM not set, can't enter debugger!\n");
      exit(1);
   }
   while(1) {
      printf("h> ");
      fgets((char*) &buf, strLen, stdin);
      switch(buf[0]) {
	 case 'h':
	 case '?':
	    printHelp();
	    break;
	 case 'r':
	    printRegs(c);
	    break;
	 case 's': {
	    // Set reg;
	    int reg;
	    uint64_t val;
	    int ret = sscanf((const char*) &buf, "s %X %lX", &reg, &val);
	    if(ret == 2) {
	       setreg(c, reg, val);
	    } else {
	       sHelp();
	    }
	    break;
	 }
	 case 'i': {
	    // Set system register
	    int reg;
	    uint64_t val;
	    int ret = sscanf((const char*) &buf, "i %X %lX", &reg, &val);
	    if(ret == 2) {
	       setregS(c, reg, val);
	    } else {
	       sHelp();
	    }
	    break;
	 }
	 case 'm': {
	    uint64_t from, len;
	    int ret = sscanf((const char*) &buf, "m %lX %lX", &from, &len);
	    switch(ret) {
	       case 2:
		  printMem(c, from, len);
		  break;
	       case 1:
		  printMem(c, from, 0x100);
		  break;
	       default:
		  mHelp();
		  break;
	    }
	    break;
	 }
	 case 'n': {
	    uint64_t from, len, val;
	    int ret = sscanf((const char*) &buf, "n %lX %lX %lX", &from, &len, &val);
	    switch(ret) {
	       case 3:
		  setMem(c, from, len, val);
		  break;
	       case 2:
		  val = len;
		  setMem(c, from, 1, val);
		  break;
	       default:
		  nHelp();
	    }
	    setMem(c, 0, 0, 0);
	    break;
	 }
	 case 'q':
	    // Resume execution.
	    return;
	 case 'x':
	    // Exit program
	    exit(0);
	    break;
	 default:
	    printf("Unknown command: %c.  Type h or ? for help.\n", buf[0]);
	    break;
      }
      fflush(NULL);
   }
}

void printRegs(vm* c) {
   for(int i = 0; i < NUMREGS; i += 4) {
      printf("R%02d: 0x%016lX R%02d: 0x%016lX R%02d: 0x%016lX R%02d: 0x%016lX\n",
	     i, getreg(c, i), i+1, getreg(c, i+1),
	     i+2, getreg(c, i+2), i+3, getreg(c, i+3));
   }
   printf(" IP: 0x%016lX FLG: 0x%016lX CPU: 0x%016lX IDT: 0x%016lX\n",
	  SIP(c), SFLAGS(c), SCPU(c), SIDT(c));
   fflush(NULL);
}

void printMem(vm* c, uint64_t start, uint64_t length) {
   for(uint64_t i = start; i  < (start+length); i += 16) {
      printf("0x%016lX: 0x%08X 0x%08X 0x%08X 0x%08X\n",
	     i, getmemT(c, i), getmemT(c, i+4), 
	     getmemT(c, i+8), getmemT(c, i+12));
   }
   fflush(NULL);
}

void setMem(vm* c, uint64_t start, uint64_t length, uint64_t val) {
   for(uint64_t i = start; i < (start+length); i += 8) {
      setmemO(c, i, val);
   }
   fflush(NULL);
}

void printHelp(void) {
   printf("All numbers are in hex whether or not you add a leading 0x\n");
   printf("This includes the numbers used to specify registers; sorry.\n");
   rHelp();
   mHelp();
   sHelp();
   iHelp();
   nHelp();
   qHelp();
   xHelp();
   fflush(NULL);
}


// We add some signal handlers, too...
void segfaultHandler(int i) {
   printf("Segfault occured!\n");
   printRegs(debugVM);
   fflush(NULL);
   exit(-1);
}


void setupSignalHandlers() {
   struct sigaction sa;
   sa.sa_handler = segfaultHandler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = 0;
   sigaction(SIGSEGV, &sa, NULL);
}

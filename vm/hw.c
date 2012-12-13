#define _POSIX_C_SOURCE 200112L
#include <errno.h>
#include <poll.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <time.h>

#include <malloc.h>
#include "cpu.h"
#include "hw.h"
#include "debug.h"
#include "error.h"

// XXX: Consider the various errors that can arise from mmap();
// read-write errors can result in SIGBUS.
// mmap() may not be the best solution.  pread() and pwrite(), dawg!
void initDrive(drive* d, char* file) {
   if(file != NULL) { // Try to open and mmap file
      int fd = open(file, O_RDWR);
      if(fd == -1) {
	 generalError("File specified for long-term storage exist'th not!");
      }
      off_t s = lseek(fd, 0, SEEK_END);
      if(s == -1) {
	 generalError("lseek: Something horrible happened trying to create storage drive.");
      }
      d->fd = fd;
      d->mapLength = s;
      d->map = (uint8_t*) mmap(
	 NULL, d->mapLength, PROT_READ | PROT_WRITE, MAP_SHARED,
	 d->fd, 0);
      d->status = ST_READY;
   } else { // No file, so we just do nothing.
      d->fd = -1;
      d->map = NULL;
      d->mapLength = 0;
      d->status = ST_UNATTACHED;
   }
   d->memLocation = 0;
   d->driveLocation = 0;
   d->opLength = 0;
   d->interruptWhenDone = true;
}

void initConsole(console* c, int infd, int outfd) {
   c->infd = infd;
   c->outfd = outfd;
   c->readspace = 0;
   c->writespace = CONSOLE_BUFSIZE;
}

// XXX: Holy fuck we don't init any of the condition variables...  eeek.
vm* newVM(uint64_t memory, char **file, const int filecount) {
   // Allocate structure and memory
   vm* c = (vm*) malloc(sizeof(vm));
   if(!c) {
      generalError("Could not allocate memory for VM structure.  Aiee!");
   }
   memset((void*) c, 0, sizeof(vm));
   c->mem = (uint8_t*) calloc(memory, 1);
   c->memLength = memory;
   if(!c->mem) {
      generalError("Could not allocate memory region; is it too big?");
   }

   // Set up hardware communication pipe
   int pipefds[2];
   // XXX: Check errors
   pipe(pipefds);
   c->hwOut = pipefds[1];
   c->hwIn = pipefds[0];
   pthread_mutex_init(&(c->hwMutex), NULL);
   
   // Fire up console
   initConsole(&(c->console), STDIN_FILENO, STDOUT_FILENO);

   // Fire up the long-term storage, if any.
   for(int i = 0; i < filecount; i++) {
      initDrive(&(c->drives[i]), file[i]);
   }
   pthread_mutex_init(&(c->driveMutex), NULL);
   pthread_cond_init(&(c->driveCond), NULL);

   // Init timer
   // XXX: Set for debugging purposes;
   // should both be set to 0 upon real boot.
   c->intervalTimerInterval = 1000;
   c->intervalTimerRepeat = 0;
   //pthread_mutex_init(&(c->periodicTimerMutex), NULL);

   // Initialize interrupts
   c->interruptFlag = false;
   for(int i = 0; i < NUMINTERRUPTS; i++) {
      c->interrupt[i] = false;
   }
   pthread_mutex_init(&(c->interruptMutex), NULL);

   c->instructionsRun = 0;
   return c;
}



void finalizeDrive(drive* d) {
   //printf("Map: %ld fd: %d\n", (long int) c->map[0], c->fd[0]);
   fflush(NULL);
   if(d->map != NULL) {
      munmap(d->map, d->mapLength);
   }
   if(d->fd != -1) {
      close(d->fd);
   }
}

void finalizeConsole(console* c) {
   // XXX: Closing stdin and stdout might be a bad idea???
   close(c->infd);
   close(c->outfd);
}

// XXX: Make sure this properly kills all threads and frees
// all resources.
// XXX: Killing threads??!!?!?!?!?!?!?!?!
void freeVM(vm* c) {
/*
   pthread_exit(&(c->interruptThread));
   close(c->interruptInput);
   close(c->interruptOutput);
*/
   // XXX: Check errors
   close(c->hwIn);
   close(c->hwOut);
   pthread_mutex_destroy(&(c->hwMutex));
   pthread_mutex_destroy(&(c->interruptMutex));
   pthread_mutex_destroy(&(c->driveMutex));
   pthread_cond_destroy(&(c->driveCond));
   for(int i = 0; i < NUMDRIVES; i++) {
      finalizeDrive(c->drives + i);
   }
   finalizeConsole(&(c->console));
   free(c->mem);
   free(c);
}


uint64_t hwLoad(vm* c, uint64_t addr) {
   switch(addr) {
      // Built-in properties
      case MEM_SIZE:
	 return c->memLength;
	 break;

      case START_DEBUGGER:
	 break;
	 
      case SHUTDOWN:
	 // Why yes, we ARE running, thank you for asking!
	 return 1;

	 // Console device
/*
      case CON_READREG:
	 break;
      case CON_WRITEREG:
	 break;
      case CON_READSIZE:
	 break;
      case CON_WRITESIZE:
	 break;
      case CON_READBUF:
	 break;
      case CON_WRITEBUF:
	 break;
*/

	 // Timer device
      case TIM_INTERVAL:
	 return c->intervalTimerInterval;
	 break;
      case TIM_REPEAT:
	 return c->intervalTimerRepeat;
	 break;

	 // XXX: There's a better way of doing this,
	 // probably involving vtables...
      case DRV_STAT(0):
	 return c->drives[0].status;
	 break;
      case DRV_MEM(0):
	 return c->drives[0].memLocation;
	 break;
      case DRV_STORE(0):
	 return c->drives[0].driveLocation;
	 break;
      case DRV_OPSIZE(0):
	 return c->drives[0].opLength;
	 break;
      case DRV_LENGTH(0):
	 return c->drives[0].mapLength;
	 break;

      case DRV_STAT(1):
	 return c->drives[1].status;
	 break;
      case DRV_MEM(1):
	 return c->drives[1].memLocation;
	 break;
      case DRV_STORE(1):
	 return c->drives[1].driveLocation;
	 break;
      case DRV_OPSIZE(1):
	 return c->drives[1].opLength;
	 break;
      case DRV_LENGTH(1):
	 return c->drives[1].mapLength;
	 break;

      case DRV_STAT(2):
	 return c->drives[2].status;
	 break;
      case DRV_MEM(2):
	 return c->drives[2].memLocation;
	 break;
      case DRV_STORE(2):
	 return c->drives[2].driveLocation;
	 break;
      case DRV_OPSIZE(2):
	 return c->drives[2].opLength;
	 break;
      case DRV_LENGTH(2):
	 return c->drives[2].mapLength;
	 break;

      case DRV_STAT(3):
	 return c->drives[3].status;
	 break;
      case DRV_MEM(3):
	 return c->drives[3].memLocation;
	 break;
      case DRV_STORE(3):
	 return c->drives[3].driveLocation;
	 break;
      case DRV_OPSIZE(3):
	 return c->drives[3].opLength;
	 break;
      case DRV_LENGTH(3):
	 return c->drives[3].mapLength;
	 break;

      case DRV_STAT(4):
	 return c->drives[4].status;
	 break;
      case DRV_MEM(4):
	 return c->drives[4].memLocation;
	 break;
      case DRV_STORE(4):
	 return c->drives[4].driveLocation;
	 break;
      case DRV_OPSIZE(4):
	 return c->drives[4].opLength;
	 break;
      case DRV_LENGTH(4):
	 return c->drives[4].mapLength;
	 break;

      case DRV_STAT(5):
	 return c->drives[5].status;
	 break;
      case DRV_MEM(5):
	 return c->drives[5].memLocation;
	 break;
      case DRV_STORE(5):
	 return c->drives[5].driveLocation;
	 break;
      case DRV_OPSIZE(5):
	 return c->drives[5].opLength;
	 break;
      case DRV_LENGTH(5):
	 return c->drives[5].mapLength;
	 break;

      case DRV_STAT(6):
	 return c->drives[6].status;
	 break;
      case DRV_MEM(6):
	 return c->drives[6].memLocation;
	 break;
      case DRV_STORE(6):
	 return c->drives[6].driveLocation;
	 break;
      case DRV_OPSIZE(6):
	 return c->drives[6].opLength;
	 break;
      case DRV_LENGTH(6):
	 return c->drives[6].mapLength;
	 break;

      case DRV_STAT(7):
	 return c->drives[7].status;
	 break;
      case DRV_MEM(7):
	 return c->drives[7].memLocation;
	 break;
      case DRV_STORE(7):
	 return c->drives[7].driveLocation;
	 break;
      case DRV_OPSIZE(7):
	 return c->drives[7].opLength;
	 break;
      case DRV_LENGTH(7):
	 return c->drives[7].mapLength;
	 break;

      default:
	 break;
   }
   return 0;
}

void hwStore(vm* c, uint64_t addr, uint64_t val) {
   switch(addr) {
      // XXX: On an invalid operation, is it better to do nothing,
      // or scream and die?
      case MEM_SIZE:
	 break;
      case START_DEBUGGER:
	 debug(c);
	 break;
      case SHUTDOWN:
	 // XXX: Is this graceful enough?
	 printf("Instructions run: %ld\n", c->instructionsRun);
	 freeVM(c);
	 exit(0);
	 break;

	 // Console device
/*
      case CON_READREG:
	 break;
      case CON_WRITEREG:
	 break;
      case CON_READSIZE:
	 break;
      case CON_WRITESIZE:
	 break;
      case CON_READBUF:
	 break;
      case CON_WRITEBUF:
	 break;
*/


      case TIM_INTERVAL:
	 // We signal the hardware to make it recompute the
	 // interval time to the next interrupt.
	 pthread_mutex_lock(&(c->hwMutex));
	 c->intervalTimerInterval = val;
	 pthread_mutex_unlock(&(c->hwMutex));
	 signalHardware(c, 0);
	 break;
      case TIM_REPEAT:
	 pthread_mutex_lock(&(c->hwMutex));
	 c->intervalTimerRepeat = val;
	 pthread_mutex_unlock(&(c->hwMutex));
	 signalHardware(c, 0);
	 break;

	 // XXX: Mutexes for these?  Shit...
      case DRV_STAT(0):
	 c->drives[0].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(0):
	 c->drives[0].memLocation = val;
	 break;
      case DRV_STORE(0):
	 c->drives[0].driveLocation = val;
	 break;
      case DRV_OPSIZE(0):
	 c->drives[0].opLength = val;
	 break;
      case DRV_LENGTH(0):
	 break;

      case DRV_STAT(1):
	 c->drives[1].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(1):
	 c->drives[1].memLocation = val;
	 break;
      case DRV_STORE(1):
	 c->drives[1].driveLocation = val;
	 break;
      case DRV_OPSIZE(1):
	 c->drives[1].opLength = val;
	 break;
      case DRV_LENGTH(1):
	 break;

      case DRV_STAT(2):
	 c->drives[2].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(2):
	 c->drives[2].memLocation = val;
	 break;
      case DRV_STORE(2):
	 c->drives[2].driveLocation = val;
	 break;
      case DRV_OPSIZE(2):
	 c->drives[2].opLength = val;
	 break;
      case DRV_LENGTH(2):
	 break;

      case DRV_STAT(3):
	 c->drives[3].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(3):
	 c->drives[3].memLocation = val;
	 break;
      case DRV_STORE(3):
	 c->drives[3].driveLocation = val;
	 break;
      case DRV_OPSIZE(3):
	 c->drives[3].opLength = val;
	 break;
      case DRV_LENGTH(3):
	 break;

      case DRV_STAT(4):
	 c->drives[4].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(4):
	 c->drives[4].memLocation = val;
	 break;
      case DRV_STORE(4):
	 c->drives[4].driveLocation = val;
	 break;
      case DRV_OPSIZE(4):
	 c->drives[4].opLength = val;
	 break;
      case DRV_LENGTH(4):
	 break;

      case DRV_STAT(5):
	 c->drives[5].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(5):
	 c->drives[5].memLocation = val;
	 break;
      case DRV_STORE(5):
	 c->drives[5].driveLocation = val;
	 break;
      case DRV_OPSIZE(5):
	 c->drives[5].opLength = val;
	 break;
      case DRV_LENGTH(5):
	 break;

      case DRV_STAT(6):
	 c->drives[6].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(6):
	 c->drives[6].memLocation = val;
	 break;
      case DRV_STORE(6):
	 c->drives[6].driveLocation = val;
	 break;
      case DRV_OPSIZE(6):
	 c->drives[6].opLength = val;
	 break;
      case DRV_LENGTH(6):
	 break;

      case DRV_STAT(7):	 
	 c->drives[7].status = val;
	 pthread_cond_signal(&(c->driveCond));
	 break;
      case DRV_MEM(7):
	 c->drives[7].memLocation = val;
	 break;
      case DRV_STORE(7):
	 c->drives[7].driveLocation = val;
	 break;
      case DRV_OPSIZE(7):
	 c->drives[7].opLength = val;
	 break;
      case DRV_LENGTH(7):
	 break;

      default:
	 break;
   }
}

void sendIntToCPU(vm* c, int i) {
   pthread_mutex_lock(&(c->interruptMutex));
   c->interrupt[i] = true;
   // This variable is read in run() without being mutex'ed,
   // but that's okay.
   c->interruptFlag = true;
   pthread_mutex_unlock(&(c->interruptMutex));
}

// XXX: This is REAL expensive!
// Also, it does not at all do masks or...
// well, actually handle interrupts.
// Id est, change the CPU state and jump to an ISR.
void handleInterrupts(vm* c) {
   pthread_mutex_lock(&(c->interruptMutex));
   for(int i = 0; i < NUMINTERRUPTS; i++) {
      if(c->interrupt[i]) {
	 printf("Interrupt happened: %d\n", i);
      c->interrupt[i] = false;
      }
   }
   c->interruptFlag = false;
   pthread_mutex_unlock(&(c->interruptMutex));
}

// Write a byte to the pipe to tell the main hw thread
// that something has happened.
// So far, the value does not matter.
// WTF happens if this blocks???
// Well, the CPU thread suspends until the hardware
// thread is capable of reading again...  Okay.
void signalHardware(vm* c, uint8_t i) {
   // XXX: Check errors
   ssize_t ret = write(c->hwOut, &i, 1);
   if(ret < 1) {
      int errsv = errno;
      char buf[80];
      strerror_r(errsv, buf, 80);
      printf("Errno: %s\n", buf);
      interpreterError("Something squirrelly happened while trying to signal hardware.");
   }
}

void* handleDrives(void* v) {
   vm* c = (vm*) v;
   while(1) {
      pthread_mutex_lock(&(c->driveMutex));
      for(int i = 0; i < NUMDRIVES; i++) {
	 uint64_t length = c->drives[i].opLength;
	 switch(c->drives[i].status) {
	    case ST_READ: {
	       uint64_t src = c->drives[i].driveLocation;
	       uint64_t dst = c->drives[i].memLocation;
	       if(((src + length) > c->memLength)
		  || ((dst + length) > c->drives[i].mapLength)) {
		  ioError("Read off end of memory or drive!");
	       }
	       memcpy(c->mem + dst, c->drives[i].map + src, length);
	       c->drives[i].status = ST_READY;
	       sendIntToCPU(c, i);
	       break;
	    }
	    case ST_WRITE: {
	       uint64_t src = c->drives[i].memLocation;
	       uint64_t dst = c->drives[i].driveLocation;
	       if(((src + length) > c->memLength)
		  || ((dst + length) > c->drives[i].mapLength)) {
		  ioError("Write off end of memory or drive!");
	       }
	       memcpy(c->drives[i].map + dst, c->mem + src, length);
	       c->drives[i].status = ST_READY;
	       sendIntToCPU(c, i);
	       break;
	    }
	    case ST_FLUSH:
	       fdatasync(c->drives[i].fd);
	       c->drives[i].status = ST_READY;
	       sendIntToCPU(c, i);
	       break;
	    case ST_READY:
	       break;
	    case ST_UNATTACHED:
	       break;
	    default:
	       interpreterError("Invalid drive state!");
	 }
      }
      pthread_cond_wait(&(c->driveCond), &(c->driveMutex));
   }
   return NULL;
}


// This handles LOW-LATENCY things.
// Drives are high-latency, so get their own thread.
void* handleHardware(void* v) {
   vm* c = (vm*) v;
   const int numfds = 2;
   struct pollfd fds[numfds];
   fds[0].fd = STDIN_FILENO;
   fds[0].events = POLLIN | POLLERR;
   fds[1].fd = c->hwIn;
   fds[1].events = POLLIN | POLLERR;
   int timeout = 0;
   // If we get an event before the timer expires,
   // the time until the next expiry is saved here.
   int timeLeft = 0;
   struct timespec timespec;
   pthread_mutex_lock(&(c->hwMutex));
   while(1) {
      // Figure out timer.
      // XXX: Error check clock_gettime
      // XXX: Also, make intervalTimerRepeat matter.
      // XXX: Also, what happens if the timer registers are set
      // by the program while this is happening?  Mutex!
      clock_gettime(CLOCK_MONOTONIC, &timespec);
      if(c->intervalTimerInterval == 0) {
	 // Disable timer
	 timeout = -1;
	 timeLeft = 0;
      } else {
	 // Figure out how long we have left before the next timer
	 // interrupt...
	 timeout = MAX(0, c->intervalTimerInterval - timeLeft);
      }

      // poll (time is in milliseconds)
      // The mutex is only unlocked when polling...
      // Thus, the software can only touch the hardware
      // when the hardware isn't already doing something.
      // XXX: Handle errors in poll()...
      pthread_mutex_unlock(&(c->hwMutex));
      int numrets = poll((&fds[0]), numfds, timeout);
      pthread_mutex_lock(&(c->hwMutex));

      // If the timer expired...
      if(numrets == 0) {
	 timeLeft = 0;
	 // Send interrupt.
	 sendIntToCPU(c, INT_INTERVALTIMER);
	 // And decrement timer repeats if necessary.
	 if(c->intervalTimerRepeat > 0) {
	    c->intervalTimerRepeat -= 1;
	    // If that was the last interrupt, 
	    // disable interval timer
	    if(c->intervalTimerRepeat == 0) {
	       c->intervalTimerInterval = 0;
	    }
	 }
      } else if(numrets < 0) {
	 interpreterError("I dunno, something messed up in poll()");
      } else {
	 // If there's input from the console...
	 if(fds[0].revents) {
	    // Put it in the input queue.
	    size_t charsInBuffer = c->console.readspace;
	    size_t freeSpace = CONSOLE_BUFSIZE - charsInBuffer;
	    ssize_t count = read(fds[0].fd, 
				 &(c->console.readbuf[charsInBuffer]),
				 freeSpace);
	    if(count < 0) {
	       ioError("Something screwy happened reading input?");
	    }
	    c->console.readspace += count;
	    // And signal an interrupt
	    sendIntToCPU(c, INT_CONSOLE);
	 }

	 // If there's some other operation...
	 if(fds[1].revents) {
	    // Check and handle console output...
	    size_t charsInBuffer = CONSOLE_BUFSIZE - c->console.writespace;
	    if(charsInBuffer > 0) {
	       ssize_t count = write(STDOUT_FILENO, 
				     &(c->console.readbuf[0]),
				     charsInBuffer);
	       if(count < 0) {
		  ioError("Something screwy happened writing output?");
	       }
	       c->console.writespace += count;
	       sendIntToCPU(c, INT_CONSOLE);
	    }	    
	 }

	 // Figure out how long the poll and I/O lasted,
	 // set next interval trigger time.
	 // This means that long I/O's could stall the timer;
	 // hence, those run on their own thread(s)
	 uint64_t startedAt = (timespec.tv_sec * 1000) +
	    (timespec.tv_nsec / 1000000);
	 clock_gettime(CLOCK_MONOTONIC, &timespec);
	 uint64_t now = (timespec.tv_sec * 1000) +
	    (timespec.tv_nsec / 1000000);
	 uint64_t took = now - startedAt;
	 timeLeft = MAX(0, c->intervalTimerInterval - took);
      }
   }
   return NULL;
}

void spawnThreads(vm* c) {
   if(pthread_create(
	 &(c->hwThread), NULL, 
	 &handleHardware, (void*) c) != 0) {
      generalError("Could not spawn hardware thread!!!\n");
   }

   if(pthread_create(
	 &(c->driveThread), NULL, 
	 &handleDrives, (void*) c) != 0) {
      generalError("Could not spawn hardware thread!!!\n");
   }
}

void run(vm* c) {
   uint32_t instr;
   while(1) {
      instr = getmemT(c, SIP(c));
      //uint32_t instr = getmemT(c, SIP(c));
      //printf("Executing instruction at 0x%lX: 0x%X\n", SIP(c), instr);
      //printf("R0 = 0x%lX\n", c->r[0]);
      runInstruction(c, instr);
      if(c->interruptFlag) {
	 handleInterrupts(c);
      }

      SIP(c) += INSTRBYTES;
      c->instructionsRun += 1;
      //printf("IR: %ld\n", c->instructionsRun);
   }
}


void startVM(vm* c) {
   spawnThreads(c);
   run(c);
}

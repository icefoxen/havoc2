#ifndef _HW_H
#define _HW_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <sys/select.h>
#include <pthread.h>

#define NUMREGS 32
#define NUMSYSTEMREGS 5
#define NUMINTERRUPTS 64
#define NUMDRIVES 8

typedef enum {
   ST_UNATTACHED = 0,
   ST_READY = 1,
   ST_READ = 2,
   ST_WRITE = 3,
   ST_FLUSH = 4,
} sysStatus;


typedef struct {
   int fd;                // File descriptor of mmap'd storage
   uint8_t *map;          // mmap'd storage array
   uint64_t mapLength;    // Size of storage array

   // Interface registers
   sysStatus status;
   uint64_t memLocation;
   uint64_t driveLocation;
   uint64_t opLength;
   bool interruptWhenDone;
} drive;

// Console buffers fixed at 1 kilobyte
#define CONSOLE_BUFSIZE 1024

typedef struct {
   int infd;            // File descriptor of input stream
   int outfd;           // File descriptor of output stream
   uint64_t readspace;  // # of characters in the read buffer
   uint64_t writespace; // # of free bytes in the write buffer
   char readbuf[CONSOLE_BUFSIZE];
   char writebuf[CONSOLE_BUFSIZE];
} console;

// Thing that holds all the VM state.
typedef struct {
   uint64_t r[NUMREGS];         // General purpose registers
   uint64_t s[NUMSYSTEMREGS];   // System registers
   // double f[64];    // Floating-point registers
   uint64_t memLength;  // Size of memory in bytes
   // This should be aligned; calloc does it, so yay.
   uint8_t* mem;

   pthread_t hwThread;
   pthread_mutex_t hwMutex;
   // fd's for the hardware communication pipe.
   int hwOut;
   int hwIn;

   // Various parameters for the console
   console console;

   // Various parameters for long-term storage
   pthread_t driveThread;
   pthread_cond_t driveCond;
   pthread_mutex_t driveMutex;
   drive drives[NUMDRIVES];     // Drive structs

   // Interrupts and misc. hardware
   // Should ideally be nanoseconds, but due to poll(), it isn't.
   uint64_t intervalTimerInterval; // Milliseconds
   uint64_t intervalTimerRepeat; // Times to repeat

   bool interruptFlag; // True if there is an interrupt to service
   bool interrupt[NUMINTERRUPTS];
   pthread_mutex_t interruptMutex;

   // Debugging info...
   uint64_t instructionsRun;

   bool running;
} vm;

typedef enum {
   INT_NONE = -1,
   INT_DRIVE0 = 0,
   INT_DRIVE1 = 1,
   INT_DRIVE2 = 2,
   INT_DRIVE3 = 3,
   INT_DRIVE4 = 4,
   INT_DRIVE5 = 5,
   INT_DRIVE6 = 6,
   INT_DRIVE7 = 7,
   INT_INTERVALTIMER = 8,
   INT_CONSOLE = 9,

} interrupt;

// Start of the hardware memory map
#define HWMEMORY 0xFF00000000000000

// Misc devices.
#define MEM_SIZE (HWMEMORY + 0x0)
#define START_DEBUGGER (HWMEMORY + 0x8)
#define SHUTDOWN (HWMEMORY + 0x10)

// Console addresses
#define CON_OFFSET (HWMEMORY + 0x1000)
#define CON_READREG (CON_OFFSET + 0x0)
#define CON_WRITEREG (CON_OFFSET + 0x8)
#define CON_READSIZE (CON_OFFSET + 0x10)
#define CON_WRITESIZE (CON_OFFSET + 0x18)
// These locations limit the size of the console buffers...
#define CON_READBUF (CON_OFFSET + 0x400)
#define CON_WRITEBUF (CON_OFFSET + 0x800)

// Periodic timer addresses
#define TIM_OFFSET (HWMEMORY + 0x2000)
#define TIM_INTERVAL (TIM_OFFSET + 0x0)
#define TIM_REPEAT (TIM_OFFSET + 0x8)

#define DRV_OFFSET (HWMEMORY + 0x3000)
#define DRIVESTRUCTSIZE 0x28
// Status of the drive: read, write or ready
#define DRV_STAT(n) (DRV_OFFSET + 0x0 + (n) * DRIVESTRUCTSIZE)
// Memory location to read to/write from
#define DRV_MEM(n) (DRV_OFFSET + 0x8 + (n) * DRIVESTRUCTSIZE)
// Drive location to read to/write from
#define DRV_STORE(n) (DRV_OFFSET + 0x10 + (n) * DRIVESTRUCTSIZE)
// Number of bytes to read/write
#define DRV_OPSIZE(n) (DRV_OFFSET + 0x18 + (n) * DRIVESTRUCTSIZE)
// How large the drive is
#define DRV_LENGTH(n) (DRV_OFFSET + 0x20 + (n) * DRIVESTRUCTSIZE)


#define RTC_OFFSET (HWMEMORY + 0x4000)

void initConsole(console* c, int infd, int outfd);
void initDrive(drive* d, char* file);
vm* newVM(uint64_t memory, char **files, const int filecount);
void finalizeConsole(console* c);
void finalizeDrive(drive* d);
void freeVM(vm* c);

uint64_t hwLoad(vm* c, uint64_t addr);
void hwStore(vm* c, uint64_t addr, uint64_t val);

// Hardware handling functions
void sendIntToCPU(vm* c, int i);

void signalHardware(vm* c, uint8_t i);
void handleInterrupts(vm* c);
void* handleDrives(void* v);
void* handlePeriodicTimer(void* v);

void run(vm* c);
void startVM(vm* c);


#endif // _HW_H

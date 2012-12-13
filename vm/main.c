#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <malloc.h>
// Yes these are all necessary
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "cpu.h"
#include "misc.h"
//#include "syscall.h"
#include "debug.h"

void usage() {
   printf("Usage: havoc2 [options] [drive file 1] [...]\n\
Arguments:\n\
   -h:  This message\n\
   -m <number>: Set memory size to <number> megabytes\n\
   -f <file>: Load <file> into the start of memory to start executing; otherwise the first kilobyte of storage will be used.\n\
   -t: Just run test program\n\
\
If any file names are specified after all the options, they will be used as  \
long-term storage drivess, numbered from 0 in the order they were given.  If   \
no program is given to run, the first kilobyte of drive file 0 will be loaded \
into memory and executed.\n");
}

int main(int argc, char **argv) {
   // Parse command line args
   // ...crap.  We want to be able to set memory size,
   // initial program, AND storage file all at once, sooner or later.
   // See http://www.gnu.org/s/libc/manual/html_node/Parsing-Program-Arguments.html
   // For now... hardwire it.
   int memsize = 8;
   char* programFile = NULL;
   int useTestProgram = 0;

   int option = 0;
   if(argc <= 1) {
      usage();
      exit(0);
   }
   while((option = getopt(argc, argv, "m:f:th")) != -1) {
      switch(option) {
	 case 'm':
	    memsize = atoi(optarg);
	    break;
	 case 'f':
	    printf("Loading program file from %s\n", optarg);
	    programFile = optarg;
	    if(access(programFile, F_OK) == -1) {
	       fprintf(stderr, "Program file does not exist!\n");
	       return 1;
	    }
	    break;
	 case 't':
	    printf("Using test program\n");
	    useTestProgram = 1;
	    break;
	 case 'h':
	 case '?':
	    usage();
	    return 1;
      }
   }
   // Now we grab any remaining arguments and use those as drives.
   // We can just pass an offset to argv and the total count into
   // newVM().
   int numdrives = argc - optind;
   if(numdrives > 8) {
      generalError("Only up to 8 drives may be specified!\n");
   }
   int driveOffset = optind;
   for(int index = 0; index < numdrives; index++) {
      printf("Drive %d: %s\n", index, argv[index + optind]);
   }
   char **driveList = argv + driveOffset;
   
   printf("Memory: %d MB\n", memsize);

   vm* c = newVM(MEGABYTES(memsize), driveList, numdrives);

   if(useTestProgram) {
      printf("Loading test program!\n");
      // Load test program into memory
/*
      setmemT(c, 0, composeI(LIU, 0, 0xFF00));
      setmemT(c, 4, composeI(LIL, 0, 0x3000));
      setmemT(c, 8, composeI(LI, 1, 0x100));
      setmemT(c, 12, composeR(SO, 1, 0, 0x18));
      setmemT(c, 16, composeI(LI, 2, 0x3));
      setmemT(c, 20, composeR(SO, 2, 0, 0x0));
      setmemT(c, 24, composeI(LIU, 20, 0xFF00));
      setmemT(c, 28, composeR(SO, 0, 20, 0x8));
      setmemT(c, 32, composeA(HALT, 0));
*/
      setmemT(c, 0, compose2(LI, 10, 0));
      setmemT(c, 4, compose2(LI, 11, 1));
      setmemT(c, 8, compose3(SLI, 11, 11, 0x1A));
      setmemT(c, 12, compose3(ADDI, 10, 10, 1));
      setmemT(c, 16, compose2(CMP, 10, 11));
      //setmemT(c, 20, compose1(BF,-2));
      setmemT(c, 20, compose1C(BF, CD_NE, -2));
      setmemT(c, 24, compose2(LIU, 1, 0xFF00));
      setmemT(c, 28, compose3(ADDI, 1, 1, 0x08));
      setmemT(c, 32, compose2(LI, 2, 1));
      setmemT(c, 36, compose2(LI, 3, 128));
      setmemT(c, 40, compose1(INT, 7));
      setmemT(c, 44, compose3(SO, 2, 1, 0));

   } else if(programFile != NULL) {
      // Load program file into memory
      // Okay, we know file exists.  Find file length, figure out how
      // much of it to chunk into memory, then do it, one way or another.
      int fd = open(programFile, O_RDWR);
      if(fd == -1) {
	 printf("This should never happen!\n");
	 return 1;
      }
      off_t length = lseek(fd, 0, SEEK_END);
      if(length == -1) {
	 printf("lseek: Something horrible happened trying to load program file.");
	 return 1;
      }

      // Mooooove cursor back to the start of file~
      if(lseek(fd, 0, SEEK_SET) == -1) {
	 printf("lseek: Something horrible happened trying to load program file.");
	 return 1;
      }
      // XXX: Yeah yeah I'm a bad person because I Don't check all the return values... fuck 'em.
      read(fd, c->mem, MIN(length, c->memLength));
      close(fd);
	      
   } else {
      // Load first 1 kb of the storage into memory
      if(c->drives[0].fd != -1) {
	 memmove(c->mem, c->drives[0].map, MIN(c->drives[0].mapLength, 1024));
	 debug(c);
      } else {
	 printf("No storage to load program from!\n");
	 return 1;
      }
   }
   // Defined in debug.c
   debugVM = c;
   setupSignalHandlers();
   startVM(c);
   printf("Freeing?\n");
   fflush(NULL);
   // This never gets called on a halt... might wanna fix that somehow.
   freeVM(c);
   return 0;
}

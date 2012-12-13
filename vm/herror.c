// Just general error functions.

#include "debug.h"
#include "herror.h"

void generalError(char* wossname) {
   printf("Error: %s\n", wossname);
   debug();
}

void processorException(hverror err, char* message) {
   printf("Processor exception %d: %s\n", (int) err, message);
   debug();
}

void interpreterError(char* message) {
   printf("Interpreter error: %s\n", message);
   debug();
}

void ioError(char* message) {
   printf("I/O error: %s\n", message);
   debug();
}

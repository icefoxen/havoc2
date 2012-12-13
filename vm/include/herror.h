#ifndef _HERROR_H
#define _HERROR_H

#include <stdio.h>
#include <stdlib.h>

typedef enum {
   ERR_USER,
   ERR_INVALIDOP,
   ERR_MEMERROR,
   ERR_UNKNOWN
} hverror;

void generalError(char* message);

void processorException(hverror error, char* message);

void interpreterError(char* message);

void ioError(char* message);

#endif // _HERROR_H

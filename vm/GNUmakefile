CC = gcc
LINK = gcc

#CC = clang
#LINK = clang

OBJ_BASES := herror cpu hw debug main
OBJS := $(addsuffix .o,$(OBJ_BASES))

CFLAGS := -O3 -I include -Wall -std=c99 -lpthread -lrt
CCF = $(CC) $(CFLAGS)

PROGRAM := havoc2

.PHONY: all clean test

all: $(PROGRAM)

clean:
	rm -f $(OBJS) *~ *.o $(PROGRAM) gmon.out

test: $(PROGRAM)
	$(PROGRAM)

$(PROGRAM): $(OBJS)
	$(LINK) $(CFLAGS) -o $(PROGRAM) $(OBJS)

%.o: %.c
	$(CCF) -c $<

# Now how do we make .c files depend on the appropriate .h files?

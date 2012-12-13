#ifndef _CPU_H
#define _CPU_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "herror.h"
#include "misc.h"
#include "hw.h"

typedef uint8_t opcode;
typedef uint8_t condition;

// Macros for named registers
#define SIP(c) ((c)->s[0])
#define SFLAGS(c) ((c)->s[1])
#define SCPU(c) ((c)->s[2])
#define SIMASK(c) ((c)->s[3])
#define SIDT(c) ((c)->s[4])
#define LP(c) ((c)->r[31])
#define OVER(c) ((c)->r[30])
#define SP(c) ((c)->r[29])

// Types for the flag register
typedef enum {
   FL_EQUAL = 1,
   FL_GREATER = 1 << 1,
   FL_ABOVE = 1 << 2,
   FL_CARRY = 1 << 3,
   FL_OVERFLOW = 1 << 4,
   FL_INTERRUPTSDISABLED = 1 << 5,
} flagType;

// Sets and clears various flags in the flag register
#define SETFLAG(c, f) (SFLAGS(c) |= (f))
#define CLEARFLAG(c, f) (SFLAGS(c) &= ~(f))
#define FLAGISSET(c, f) (SFLAGS(c) & (f))

// This matters sometimes.
#define INSTRBYTES 4



// Checked accessor functions for registers and memory.
inline uint64_t getreg(vm* c, int x) {
   if(x >= 0 && x < NUMREGS) {
      return c->r[x];
   } else {
      interpreterError("Tried to get invalid register");
   }
   return 0;
}

inline void setreg(vm* c, int x, uint64_t val) {
   if(x > 0 && x < NUMREGS) {
      c->r[x] = val;
   } else if(x == 0) {
      /* Do nothing */
   } else {
      interpreterError("Tried to set invalid register");
   }
}

inline uint64_t getregS(vm* c, int x) {
   if(x >= 0 && x < NUMSYSTEMREGS) {
      return c->s[x];
   } else {
      interpreterError("Tried to get invalid register");
   }
   return 0;
}

inline void setregS(vm* c, int x, uint64_t val) {
   if(x >= 0 && x < NUMSYSTEMREGS) {
      c->s[x] = val;
   } else {
      interpreterError("Tried to set invalid system register");
   }
}

inline uint64_t getmemO(vm* c, uint64_t mem) {
   //printf("Mem: 0x%lX\n", mem);
   if(mem < c->memLength) {
      return (*((uint64_t *)&c->mem[mem]));
   } else if(mem >= HWMEMORY) {
      return hwLoad(c, mem);
   } else {
      printf("Mem: 0x%lX\n", mem);
      interpreterError("Tried to get memory out of range");
   }
   return 0;
}
inline uint32_t getmemT(vm* c, uint64_t mem) {
   if(mem < c->memLength) {
      return (*((uint32_t *)&c->mem[mem]));
   } else if(mem >= HWMEMORY) {
      return (uint32_t) hwLoad(c, mem);
   } else {
      interpreterError("Tried to get memory out of range");
   }
   return 0;
}
inline uint16_t getmemW(vm* c, uint64_t mem) {
   if(mem < c->memLength) {
      return (*((uint16_t *)&c->mem[mem]));
   } else if(mem >= HWMEMORY) {
      return (uint16_t) hwLoad(c, mem);
   } else {
      interpreterError("Tried to get memory out of range");
   }
   return 0;
}
inline uint8_t getmemB(vm* c, uint64_t mem) {
   if(mem < c->memLength) {
      return (*((uint8_t *)&c->mem[mem]));
   } else if(mem >= HWMEMORY) {
      return (uint8_t) hwLoad(c, mem);
   } else {
      interpreterError("Tried to get memory out of range");
   }
   return 0;
}

inline void setmemO(vm* c, uint64_t mem, uint64_t val) {
   if(mem < c->memLength) {
      (*((uint64_t *)&c->mem[mem])) = val;
   } else if(mem >= HWMEMORY) {
      hwStore(c, mem, val);
   } else {
      interpreterError("Tried to set memory out of range");
   }
}
inline void setmemT(vm* c, uint64_t mem, uint32_t val) {
   if(mem < c->memLength) {
      (*((uint32_t *)&c->mem[mem])) = val;
   } else if(mem >= HWMEMORY) {
      hwStore(c, mem, val);
   } else {
      interpreterError("Tried to set memory out of range");
   }
}
inline void setmemW(vm* c, uint64_t mem, uint16_t val) {
   if(mem < c->memLength) {
      (*((uint16_t *)&c->mem[mem])) = val;
   } else if(mem >= HWMEMORY) {
      hwStore(c, mem, val);
   } else {
      interpreterError("Tried to set memory out of range");
   }
}
inline void setmemB(vm* c, uint64_t mem, uint8_t val) {
   if(mem < c->memLength) {
      (*((uint8_t *)&c->mem[mem])) = val;
   } else if(mem >= HWMEMORY) {
      hwStore(c, mem, val);
   } else {
      interpreterError("Tried to set memory out of range");
   }
}


typedef enum {
   CD_FLOAT,
   CD_ALWAYS,
   CD_NEVER,
   CD_GT,
   CD_LE,
   CD_LT,
   CD_GE,
   CD_EQ,
   CD_NE,
   CD_AB,
   CD_BE,
   CD_BL,
   CD_AE,
   CD_CS,
   CD_CC
} condType;


typedef enum {
   ADD = 0x0,
   ADDI = 0x1,
   ADC = 0x2,
   ADCI = 0x3,
   SUB = 0x4,
   SUBI = 0x5,
   SBB = 0x6,
   SBBI = 0x7,
   MUL = 0x8,
   MULI = 0x9,
   UMUL = 0xA,
   UMULI = 0xB,
   DIV = 0xC,
   DIVI = 0xD,
   UDIV = 0xE,
   UDIVI = 0xF,

   AND = 0x10,
   ANDI = 0x11,
   OR = 0x12,
   ORI = 0x13,
   XOR = 0x14,
   XORI = 0x15,
   SL = 0x16,
   SLI = 0x17,
   SR = 0x18,
   SRI = 0x19,
   SRU = 0x1A,
   SRUI = 0x1B,
   RL = 0x1C,
   RLI = 0x1D,
   RR = 0x1E,
   RRI = 0x1F,

   NAND = 0x20,
   NANDI = 0x21,
   NOR = 0x22,
   NORI = 0x23,
   NXOR = 0x24,
   NXORI = 0x25,
   B = 0x26,
   BL = 0x27,
   BR = 0x28,
   BRL = 0x29,
   BF = 0x2A,
   BFL = 0x2B,
   GET = 0x2C,
   SET = 0x2D,
   INT = 0x2E,
   RETI = 0x2F,

   LO = 0x30,
   LOR = 0x31,
   LOF = 0x32,
   // Unassigned opcode
   LT = 0x34,
   LTR = 0x35,
   LTF = 0x36,
   // Unassigned opcode
   LW = 0x38,
   LWR = 0x39,
   LWF = 0x3A,
   // Unassigned opcode
   LB = 0x3C,
   LBR = 0x3D,
   LBF = 0x3E,
   // Unassigned opcode

   SO = 0x40,
   SOR = 0x41,
   SOF = 0x42,
   // Unassigned opcode
   ST = 0x44,
   STR = 0x45,
   STF = 0x46,
   // Unassigned opcode
   SW = 0x48,
   SWR = 0x49,
   SWF = 0x4A,
   // Unassigned opcode
   SB = 0x4C,
   SBR = 0x4D,
   SBF = 0x4E,
   // Unassigned opcode

   LIU = 0x50,
   LIUM = 0x51,
   LILM = 0x52,
   LIL = 0x53,
   LI = 0x54,
   CMP = 0x55,
   EXTB = 0x56,
   EXTW = 0x57,
   EXTT = 0x58,
   CMPSZ = 0x59,
   HALT = 0x5A,

   OP_END  // End of opcodes
} op;


// Useful constants for composing and decomposing instructions
// of different formats.
#define O3DESTMASK 0X001F0000
#define O3DESTSHIFT 16
#define O3SRC1MASK 0X0000F800
#define O3SRC1SHIFT 11
#define O3SRC2MASK 0X000007FF
#define O3SRC2LENGTH 11

#define O2DESTMASK 0X001F0000
#define O2DESTSHIFT 16
#define O2MASK 0X0000FFFF
#define O2LENGTH 16

#define O1MASK 0X001FFFFF
#define O1LENGTH 21

#define CONDITIONMASK 0XF0000000
#define OPCODEMASK 0X0FE00000
#define CONDITIONSHIFT 28
#define OPCODESHIFT 21



// Functions to compose and decompose instructions.
inline uint32_t composeOpcode(op o, condType c) {
   uint32_t op = (o << OPCODESHIFT) | (c << CONDITIONSHIFT);
   //printf("Composed op: %X\n", op);
   return op;
}

inline uint32_t compose3C(op o, condType c, int dest, int src1, int src2) {
   uint32_t instr = composeOpcode(o, c);
   instr |= ((dest << O3DESTSHIFT) & O3DESTMASK)
      | ((src1 << O3SRC1SHIFT) & O3SRC1MASK)
      | (src2 & O3SRC2MASK);
   return instr;
}
// Shortcuts to compose opcodes with 'always' flag.
inline uint32_t compose3(op o, int dest, int src1, int src2) {
   return compose3C(o, CD_ALWAYS, dest, src1, src2);
}

inline uint32_t compose2C(op o, condType c, int dest, int src) {
   uint32_t instr = composeOpcode(o, c);
   instr |= ((dest << O2DESTSHIFT) & O2DESTMASK)
      | (src & O2MASK);
   return instr;
}

inline uint32_t compose2(op o, int dest, int src) {
   return compose2C(o, CD_ALWAYS, dest, src);
}

inline uint32_t compose1C(op o, condType c, int val) {
   uint32_t instr = composeOpcode(o, c);
   instr |= (val & O1MASK);
   return instr;
}

inline uint32_t compose1(op o, int val) {
   return compose1C(o, CD_ALWAYS, val);
}

inline uint32_t decomposeOp(uint32_t o) {
   return (o & OPCODEMASK) >> OPCODESHIFT;
}

inline uint32_t decomposeCond(uint32_t o) {
   return (o & CONDITIONMASK) >> CONDITIONSHIFT;
}

inline int32_t decompose3Dest(uint32_t o) {
   return (o & O3DESTMASK) >> O3DESTSHIFT;
}
inline int32_t decompose3Src1(uint32_t o) {
   return (o & O3SRC1MASK) >> O3SRC1SHIFT;
}
inline int32_t decompose3Src2(uint32_t o) {
   return (o & O3SRC2MASK);
}

inline int32_t decompose2Dest(uint32_t o) {
   return (o & O2DESTMASK) >> O2DESTSHIFT;
}
inline int32_t decompose2Src(uint32_t o) {
   return (o & O2MASK);
}

inline int32_t decompose1Val(uint32_t o) {
   return (o & O1MASK);
}

/*
typedef enum {
   IFM_3,
   IFM_2,
   IFM_1,
   IFM_F,
   IFM_END
} instrFormat;

instrFormat getInstrKind(opcode op);
*/
int checkCondition(uint64_t flags, uint32_t conditioncode);
void exec3(vm* c, op op, uint32_t i);
void exec2(vm* c, op op, uint32_t i);
void exec1(vm* c, op op, uint32_t i);

void runInstruction(vm* c, uint32_t instr);

#endif // _CPU_H

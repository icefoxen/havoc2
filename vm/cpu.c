#include <unistd.h>
#include <string.h>
#include "cpu.h"
#include "debug.h"
#include "misc.h"
#include "herror.h"

// Returns 0 if the instruction is not to be executed, anything non-zero otherwise.
int checkCondition(uint64_t flags, uint32_t conditioncode) {
   //printf("Condition code: 0x%X\n", conditioncode);
   switch (conditioncode) {
      case CD_FLOAT:
	 processorException(ERR_INVALIDOP, 
			    "runInstruction: Floating point not implemented");
	 break;

      case CD_ALWAYS:
	 return 1;
      case CD_GT:
	 return flags & FL_GREATER;
      case CD_GE:
	 return (flags & (FL_GREATER | FL_EQUAL));
      case CD_LT:
	 return !(flags & (FL_GREATER | FL_EQUAL));
      case CD_LE:
	 return (!(flags & FL_GREATER)) | (flags & FL_EQUAL);
      case CD_EQ:
	 return flags & FL_EQUAL;
      case CD_NE:
	 //printf("Not equal\n");
	 return !(flags & FL_EQUAL);
      case CD_AB:
	 return flags & FL_ABOVE;
      case CD_BL:
	 return !(flags & (FL_ABOVE | FL_EQUAL));
      case CD_AE:
	 return flags & (FL_ABOVE | FL_EQUAL);
      case CD_BE:
	 return (!(flags & FL_ABOVE)) | (flags & FL_EQUAL);
      case CD_CS:
	 return flags & FL_CARRY;
      case CD_CC:
	 return !(flags & FL_CARRY);
      case CD_NEVER:
	 return 0;
      default:
	 processorException(
	    ERR_INVALIDOP, "checkCondition: Invalid condition code.");
	 return 0;
   }
   processorException(
      ERR_INVALIDOP, "This should never happen!");
   return 0;
}

#define SETUP3(i) \
   int32_t dest = decompose3Dest(i);		\
   int32_t src1 = decompose3Src1(i);		\
   int32_t src2 = decompose3Src2(i);

#define SETUP2(i) \
   int32_t dest = decompose2Dest(i);\
   int32_t src = decompose2Src(i);\

#define SETUP1(i) int32_t val = decompose1Val(i);

// Ideally to make this as fast as possible we should:
// First, pull out and check condition code.
// Next, pull out and check opcode
// Next, decode appropriately
// Finally, execute instruction
void runInstruction(vm* c, uint32_t instr) {
   uint32_t conditionCode = (instr & CONDITIONMASK) >> CONDITIONSHIFT;
   uint64_t flags = SFLAGS(c);
   if(!checkCondition(flags, conditionCode)) {
      //printf("condition bad: %lX, %X\n", flags, conditionCode);
      return;
   } else {
      uint32_t opcode = (instr & OPCODEMASK) >> OPCODESHIFT;
      //printf("Opcode: 0x%X at 0x%lX\n", opcode, SIP(c));
      switch(opcode) {
	 // __int128 is a GCC extension, though it will probably
	 // work on clang as well.
	 case ADD: { 
	    SETUP3(instr);
	    __int128 res = getreg(c, src1) + getreg(c, src2);
	    int64_t carry = HIGH64(res);
	    int64_t result = LOW64(res);
	    if(carry != 0) {
	       SETFLAG(c, FL_CARRY);
	    } else {
	       CLEARFLAG(c, FL_CARRY);
	    }
	    setreg(c, dest, result);
	    break;
	 }
         case ADDI: { 
	    SETUP3(instr);

	    __int128 res = getreg(c, src1) + src2;
	    int64_t carry = HIGH64(res);
	    int64_t result = LOW64(res);
	    if(carry != 0) {
	       SETFLAG(c, FL_CARRY);
	    } else {
	       CLEARFLAG(c, FL_CARRY);
	    }
	    setreg(c, dest, result);
	    break;
	 }
         case ADC: { 
	    SETUP3(instr);
	    int inc = FLAGISSET(c, FL_CARRY) ? 1 : 0;
	    __int128 res = getreg(c, src1) + getreg(c, src2) + inc;
	    int64_t carry = HIGH64(res);
	    int64_t result = LOW64(res);
	    if(carry != 0) {
	       SETFLAG(c, FL_CARRY);
	    } else {
	       CLEARFLAG(c, FL_CARRY);
	    }
	    setreg(c, dest, result);
	    break;
	 }
         case ADCI: { 
	    SETUP3(instr);
	    // Instead of 'if carry' we could just make
	    // an expression where that takes the carry
	    // flag and turns it into a 1 or 0...
	    int inc = FLAGISSET(c, FL_CARRY) ? 1 : 0;
	    __int128 res = getreg(c, src1) + src2 + inc;
	    int64_t carry = HIGH64(res);
	    int64_t result = LOW64(res);
	    if(carry != 0) {
	       SETFLAG(c, FL_CARRY);
	    } else {
	       CLEARFLAG(c, FL_CARRY);
	    }
	    setreg(c, dest, result);
	    break;
	 }
	    // XXX: Implement borrow
         case SUB: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src2) - getreg(c, src1);
	    setreg(c, dest, res);
	    break;
	 }
         case SUBI: { 
	    SETUP3(instr);
	    int64_t res = src2 - getreg(c, src1);
	    setreg(c, dest, res);
	    break;
	 }
	    // XXX: Implement borrow
	 case SBB: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src2) - getreg(c, src1);
	    setreg(c, dest, res);
	    break;
	 }
         case SBBI: { 
	    SETUP3(instr);
	    int64_t res = src2 - getreg(c, src1);
	    setreg(c, dest, res);
	    break;
	 }
         case MUL: { 
	    // XXX: Set overflow bit???
	    SETUP3(instr);
	    int64_t s1 = getreg(c, src1);
	    int64_t s2 = getreg(c, src2);
	    __int128 res = s1 * s2;
	    setreg(c, dest,  LOW64(res));
	    // r30 is the overflow register
	    setreg(c, 30,  HIGH64(res));
	    break;
	 }
         case MULI: { 
	    SETUP3(instr);
	    int64_t s1 = getreg(c, src1);
	    int64_t s2 = src2;
	    __int128 res = s1 * s2;
	    setreg(c, dest,  LOW64(res));
	    // r30 is the overflow register
	    setreg(c, 30,  HIGH64(res));
	    break;
	 }
         case UMUL: { 
	    SETUP3(instr);
	    uint64_t s1 = getreg(c, src1);
	    uint64_t s2 = getreg(c, src2);;
	    unsigned __int128 res = s1 * s2;
	    setreg(c, dest,  LOW64(res));
	    // r30 is the overflow register
	    setreg(c, 30,  HIGH64(res));
	    break;
	 }
         case UMULI: { 
	    SETUP3(instr);
	    uint64_t s1 = getreg(c, src1);
	    uint64_t s2 = src2;
	    unsigned __int128 res = s1 * s2;
	    setreg(c, dest,  LOW64(res));
	    // r30 is the overflow register
	    setreg(c, 30,  HIGH64(res));
	    break;
	 }
         case DIV: { 
	    SETUP3(instr);
	    int64_t s1 = getreg(c, src1);
	    int64_t s2 = getreg(c, src2);
	    setreg(c, dest,  s1 / s2);
	    setreg(c, 30,  s1 % s2);
	    break;
	 }
         case DIVI: { 
	    SETUP3(instr);
	    int64_t s1 = getreg(c, src1);
	    int64_t s2 = src2;
	    setreg(c, dest,  s1 / s2);
	    setreg(c, 30,  s1 % s2);
	    break;
	 }
         case UDIV: { 
	    SETUP3(instr);
	    uint64_t s1 = getreg(c, src1);
	    uint64_t s2 = getreg(c, src2);
	    setreg(c, dest,  s1 / s2);
	    setreg(c, 30,  s1 % s2);
	    break;
	 }
         case UDIVI: { 
	    SETUP3(instr);
	    uint64_t s1 = getreg(c, src1);
	    uint64_t s2 = src2;
	    setreg(c, dest,  s1 / s2);
	    setreg(c, 30,  s1 % s2);
	    break;
	 }
         case AND: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src1) & getreg(c, src2);
	    setreg(c, dest, res);
	    break;
	 }
         case ANDI: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src1) & src2;
	    setreg(c, dest, res);
	    break;
	 }
	    
         case OR: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src1) | getreg(c, src2);
	    setreg(c, dest, res);
	    break;
	 }
         case ORI: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src1) | src2;
	    setreg(c, dest, res);
	    break;
	 }
         case XOR: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src1) ^ getreg(c, src2);
	    setreg(c, dest, res);
	    break;
	 }
         case XORI: { 
	    SETUP3(instr);
	    int64_t res = getreg(c, src1) ^ src2;
	    setreg(c, dest, res);
	    break;
	 }
         case SL: { 
	    SETUP3(instr);
	    setreg(c, dest,  getreg(c, src1) << getreg(c, src2));
	    break;
	 }
         case SLI: { 
	    SETUP3(instr);
	 setreg(c, dest,  getreg(c, src1) << src2);
	    break;
	 }
         case SR: { 
	    SETUP3(instr);
	    setreg(c, dest,  getreg(c, src1) >> getreg(c, src2));
	    break;
	 }
         case SRI: { 
	    SETUP3(instr);
	    setreg(c, dest,  getreg(c, src1) >> src2);
	    break;
	 }
         case SRU: { 
	    SETUP3(instr);
	    uint64_t s1 = getreg(c, src1);
	    uint64_t s2 = getreg(c, src2);
	    setreg(c, dest,  s1 >> s2);
	    OVER(c) = s1 % s2;
	    break;
	 }
         case SRUI: { 
	    SETUP3(instr);
	    uint64_t s1 = getreg(c, src1);
	    uint64_t s2 = src2;
	    setreg(c, dest,  s1 >> s2);
	    OVER(c) = s1 % s2;
	    break;
	 }
	    // XXX: Double-check rolls, make sure they're correct.  This
	    // looks a little silly.
         case RL: { 
	    SETUP3(instr);
	    int64_t value = getreg(c, src1);
	    int64_t shift = getreg(c, src2);
	    if((shift &= sizeof(value)*8 - 1) == 0) {
	       setreg(c, dest,  value);
	    } else {
	       setreg(c, dest,  (value << shift) | (value >> (sizeof(value)*8 - shift)));
	    }
	    break;
	 }
         case RLI: { 
	    SETUP3(instr);
	    int64_t value = getreg(c, src1);
	    int64_t shift = src2;
	    if((shift &= sizeof(value)*8 - 1) == 0) {
	       setreg(c, dest,  value);
	    } else {
	       setreg(c, dest,  (value << shift) | (value >> (sizeof(value)*8 - shift)));
	    }
	    break;
	 }
         case RR: { 
	    SETUP3(instr);
	    int64_t value = getreg(c, src1);
	    int64_t shift = getreg(c, src2);
	    if ((shift &= sizeof(value)*8 - 1) == 0) {
	       setreg(c, dest,  value);
	    } else {
	       setreg(c, dest,  (value >> shift) | (value << (sizeof(value)*8 - shift)));
	    }
	    break;
	 }
         case RRI: { 
	    SETUP3(instr);
	    int64_t value = getreg(c, src1);
	    int64_t shift = src2;
	    if ((shift &= sizeof(value)*8 - 1) == 0) {
	       setreg(c, dest,  value);
	    } else {
	       setreg(c, dest,  (value >> shift) | (value << (sizeof(value)*8 - shift)));
	    }
	    break;
	 }
         case NAND: { 
	    SETUP3(instr);
	    setreg(c, dest,  ~(getreg(c, src1) & getreg(c, src2)));
	    break;
	 }
         case NANDI: { 
	    SETUP3(instr);
	    setreg(c, dest,  ~(getreg(c, src1) & src2));
	    break;
	 }
	    
         case NOR: { 
	    SETUP3(instr);
	    setreg(c, dest,  ~(getreg(c, src1) | getreg(c, src2)));
	    break;
	 }
         case NORI: { 
	    SETUP3(instr);
	    setreg(c, dest,  ~(getreg(c, src1) | src2));
	    break;
	 }
         case NXOR: { 
	    SETUP3(instr);
	    setreg(c, dest,  ~(getreg(c, src1) ^ getreg(c, src2)));
	    break;
	 }
         case NXORI: { 
	    SETUP3(instr);
	    setreg(c, dest,  ~(getreg(c, src1) ^ src2));
	    break;
	 }
         case B: { 
	    SETUP2(instr);
	    uint64_t target = getreg(c, dest) + (src * INSTRBYTES);
	    SIP(c) = target - 4;
	    break;
	 }
         case BL: {
	    SETUP2(instr); 
	    uint64_t target = getreg(c, dest) + (src * INSTRBYTES);
	    LP(c) = SIP(c) + 4;
	    SIP(c) = target - 4;
	    break;
	 }
         case BR: {
	    SETUP2(instr); 
	    uint64_t target = SIP(c) + getreg(c, dest) + (src * INSTRBYTES);
	    SIP(c) = target - 4;
	    break;
	 }
         case BRL: { 
	    SETUP2(instr);
	    uint64_t target = SIP(c) + getreg(c, dest) + (src * INSTRBYTES);
	    LP(c) = SIP(c) + 4;
	    SIP(c) = target - 4;
	    break;
	 }
         case BF: {
	    SETUP1(instr); 
	    uint64_t target = SIP(c) + (SEXT21(val) * INSTRBYTES);
	    //printf("Branch far to target: 0x%lX, got 0x%lX + 0x%X\n", target, SIP(c), (val * INSTRBYTES));
	    SIP(c) = target - 4;
	    break;
	 }
         case BFL: { 
	    SETUP1(instr);
	    uint64_t target = SIP(c) + (val * INSTRBYTES);
	    LP(c) = SIP(c) + 4;
	    SIP(c) = target - 4;
	    break;
	 }
         case GET: { 
	    SETUP2(instr);
	    setreg(c, dest,  getregS(c, src));
	    break;
	 }
         case SET: { 
	    SETUP2(instr);
	    setregS(c, dest, getreg(c, src));
	    break;
	 }
         case INT: { 
	    SETUP1(instr);
	    sendIntToCPU(c, val);
	    break;
	 }
         case RETI: { 
	    //SETUP1(instr);
	    // XXX: Implement RETI.
	    break;
	 }
         case CMPSZ: { 
	    SETUP3(instr);
	    uint64_t a = getreg(c, src1);
	    uint64_t b = getreg(c, src2);
	    if(a == b) {
	       setreg(c, dest, a);
	    } else {
	       setreg(c, dest,  0);
	    }
	    break;
	 }
         case HALT: { 
	    // XXX: Fix this; should wait for interrupts:
	    // Wait for a condition on the main thread, and then,
	    // sendIntToCPU() should trigger that condition.
	    printf("Instructions run: %ld\n", c->instructionsRun);
	    printf("Halting.\n");
	    exit(0);
	    break;
	 }
	    // XXX: Double-check that the loads and stores are good.
	    // Which arguments are which is probably horrifically wrong.
         case LO: { 
	    SETUP3(instr);
	    uint64_t addr = getreg(c, src1) + src2 * 8;
	    setreg(c, dest,  getmemO(c, addr));
	    break;
	 }
         case LOR: { 
	    SETUP3(instr);
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2 * 8;
	    setreg(c, dest,  getmemO(c, addr)); 
	    break;
	 }
         case LOF: { 
	    SETUP2(instr);
	    uint64_t addr = SIP(c) + src * 8;
	    setreg(c, dest,  getmemO(c, addr));
	    break;
	 }
         case LT: { 
	    SETUP3(instr);
	    uint64_t addr = getreg(c, src1) + src2 * 4;
	    setreg(c, dest,  getmemT(c, addr));
	    break;
	 }
         case LTR: { 
	    SETUP3(instr);
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2 * 4;
	    setreg(c, dest,  getmemT(c, addr)); 
	    break;
	 }
         case LTF: { 
	    SETUP2(instr);
	    uint64_t addr = SIP(c) + src * 4;
	    setreg(c, dest,  getmemT(c, addr));
	    break;
	 }
         case LW: { 
	    SETUP3(instr);
	    uint64_t addr = getreg(c, src1) + src2 * 2;
	    setreg(c, dest,  getmemW(c, addr));
	    break;
	 }
         case LWR: { 
	    SETUP3(instr);
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2 * 2;
	    setreg(c, dest,  getmemW(c, addr)); 
	    break;
	 }
         case LWF: { 
	    SETUP2(instr);
	    uint64_t addr = SIP(c) + src * 2;
	    setreg(c, dest,  getmemW(c, addr));
	    break;
	 }
         case LB: { 
	    SETUP3(instr);
	    uint64_t addr = getreg(c, src1) + src2;
	    setreg(c, dest,  getmemB(c, addr));
	    break;
	 }
         case LBR: { 
	    SETUP3(instr);
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2;
	    setreg(c, dest,  getmemB(c, addr)); 
	    break;
	 }
         case LBF: { 
	    SETUP2(instr);
	    uint64_t addr = SIP(c) + src;
	    setreg(c, dest,  getmemB(c, addr));
	    break;
	 }
	    
         case SO: { 
	    SETUP3(instr);
	    uint64_t addr = getreg(c, src1) + src2 * 8;
	    setmemO(c, addr, getreg(c, dest));
	    break;
	 }
         case SOR: { 
	    SETUP3(instr);
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2 * 8;
	    setmemO(c, addr, getreg(c, dest));
	    break;
	 }
         case SOF: { 
	    SETUP2(instr);
	    uint64_t addr = SIP(c) + src * 8;
	    setmemO(c, addr, getreg(c, dest));
	    break;
	 }
         case ST: { 
	    SETUP3(instr);
	    uint64_t addr = getreg(c, src1) + src2 * 4;
	    setmemT(c, addr, getreg(c, dest));
	    break;
	 }
         case STR: { 
	    SETUP3(instr);
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2 * 4;
	    setmemT(c, addr, getreg(c, dest));
	    break;
	 }
         case STF: { 
	    SETUP2(instr);
	    uint64_t addr = SIP(c) + src * 4;;
	    setmemT(c, addr, getreg(c, dest));
	    break;
	 }
         case SW: {
	    SETUP3(instr); 
	    uint64_t addr = getreg(c, src1) + src2 * 2;
	    setmemW(c, addr, getreg(c, dest));
	    break;
	 }
         case SWR: {
	    SETUP3(instr);  
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2 * 2;
	    setmemW(c, addr, getreg(c, dest));
	    break;
	 }
         case SWF: {
	    SETUP2(instr);  
	    uint64_t addr = SIP(c) + src * 2;
	    setmemW(c, addr, getreg(c, dest));
	    break;
	 }
         case SB: {
	    SETUP3(instr);  
	    uint64_t addr = getreg(c, src1) + src2;
	    setmemB(c, addr, getreg(c, dest));
	    break;
	 }
         case SBR: {
	    SETUP3(instr);  
	    uint64_t addr = SIP(c) + getreg(c, src1) + src2;
	    setmemB(c, addr, getreg(c, dest));
	    break;
	 }
         case SBF: {
	    SETUP2(instr);  
	    uint64_t addr = SIP(c) + src;
	    setmemB(c, addr, getreg(c, dest));
	    break;
	 }
	    
         case LIU: { 
	    SETUP2(instr); 
	    uint64_t val = getreg(c, dest) & 0x0000FFFFFFFFFFFF;
	    val = val | (((uint64_t) src) << 48);
	    setreg(c, dest, val);
	    break;
	 }
         case LIUM: { 
	    SETUP2(instr); 
	    uint64_t val = getreg(c, dest) & 0xFFFF0000FFFFFFFF;
	    val = val | (((uint64_t) src) << 32);
	    setreg(c, dest, val);
	    break;
	 }
         case LILM: { 
	    SETUP2(instr); 
	    uint64_t val = getreg(c, dest) & 0xFFFFFFFF0000FFFF;
	    val = val | (((uint64_t) src) << 16);
	    setreg(c, dest, val);
	    break;
	 }
         case LIL: {
	    SETUP2(instr);  
	    uint64_t val = getreg(c, dest) & 0xFFFFFFFFFFFF0000;
	    val = val | ((uint64_t) src);
	    setreg(c, dest, val);
	    break;
	 }
         case LI: { 
	    SETUP2(instr); 
	    setreg(c, dest, src);
	    break;
	 }
	 case CMP: {
	    SETUP2(instr);
	    const uint64_t bits = FL_EQUAL | FL_GREATER | FL_ABOVE;
	    SFLAGS(c) &= ~bits;
	    uint64_t a = getreg(c, dest);
	    uint64_t b = getreg(c, src);
	    int64_t sa = getreg(c, dest);
	    int64_t sb = getreg(c, src);
	    if(a == b) {
	       SFLAGS(c) |= FL_EQUAL;
	       //printf("Equal\n");
	    } else {
	       if(sa > sb) {
		  SFLAGS(c) |= FL_GREATER;
		  // printf("GREATER\n");
	       }
	       if(a > b) {
		  SFLAGS(c) |= FL_ABOVE;
		  //printf("ABOVE\n");
	       }
	    }
	    break;
	 }
         case EXTB: { 
	    SETUP1(instr); 
	    int64_t vl = getreg(c, val);
	    setreg(c, val, vl);
	    break;
	 }
         case EXTW: { 
	    SETUP1(instr); 
	    int64_t vl = getreg(c, val);
	    setreg(c, val, vl);
	    break;
	 }
         case EXTT: { 
	    SETUP1(instr); 
	    int64_t vl = getreg(c, val);
	    setreg(c, val, vl);
	    break;
	 }

	 default: {
	    interpreterError("Invalid instruction!");
	 }

      }
   }
}


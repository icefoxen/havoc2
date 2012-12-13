// Useful miscellaneous stuff.
#ifndef _MISC_H
#define _MISC_H

// Aaaa I should not have to redefine this!
#define MIN(x,y) ((x) > (y) ? (y) : (x))
#define MAX(x,y) ((x) < (y) ? (y) : (x))

#define KILOBYTES(x) ((x) * 1024)
#define MEGABYTES(x) ((x) * 1024*1024)

// This gets the high 64 bits of a 128-bit number
#define HIGH64(x) (((x) >> 64) & 0xFFFFFFFFFFFFFFFF)
// And this gets the low 64 bits.
#define LOW64(x) ((x) & 0xFFFFFFFFFFFFFFFF)

// More obtuse syntax!
// These do sign-extends of 21, 16 and 11-bit values.
// x must be an int32_t
#define SEXT21(x) ((((int32_t)(x)) << 11) >> 11)
#define SEXT16(x) ((((int32_t)(x)) << 16) >> 16)
#define SEXT11(x) ((((int32_t)(x)) << 21) >> 21)



#endif // _MISC_H

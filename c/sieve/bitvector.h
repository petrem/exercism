#ifndef _BITVECTOR_H
#define _BITVECTOR_H

#include <stdint.h>
#include <inttypes.h>
#include <stddef.h>


/* printf family format conversion specifier and length modifier macros:
   uint16_t -> PRIx16
   uint32_t -> PRIx32
   uint64_t -> PRIx64
   uint_fast32_t -> PRIxFAST32

   replace x with u for decimal, etc.
*/

#define WORDTYPE uint_fast32_t
#define WORDFMT  PRIxFAST32
#define WORDSIZE (sizeof(WORDTYPE) * 8)
#define WORDMASK ((WORDTYPE) (WORDSIZE - 1))

#define UNMARKED  (unsigned) 0
#define NOT_PRIME (unsigned) 1

WORDTYPE *bitvector_init(size_t limit);
void bitvector_set(WORDTYPE *vector, size_t index, unsigned value);
unsigned bitvector_get(WORDTYPE *vector, size_t index);
void bitvector_print(WORDTYPE *vector, size_t limit);

#endif

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "bitvector.h"


WORDTYPE *bitvector_init(size_t limit) {
  WORDTYPE *data = calloc((limit + WORDSIZE - 1) / WORDSIZE, WORDSIZE);
  if (data == NULL) {
    fputs("Could not allocate memory. Good bye!", stderr);
    exit(ENOMEM);
  }
  return data;
}

inline void bitvector_set(WORDTYPE *vector, size_t index, unsigned value) {
  size_t word_index = index / WORDSIZE;
  size_t shift = index & WORDMASK;
  WORDTYPE word = vector[word_index];
  if (((word >> shift) & 1) != value) {
    vector[word_index] = word ^ ((WORDTYPE)1 << shift);
  }
}

inline unsigned bitvector_get(WORDTYPE *vector, size_t index) {
  return (vector[index / WORDSIZE] >> (index & WORDMASK)) & 1;
}

void bitvector_print(WORDTYPE *vector, size_t limit) {
  WORDTYPE word;
  word = vector[0];
  for (size_t i = 0; i < sizeof(word); i++)
    printf("0x%.2hhx ", *(((char *)&word)+i));
  printf("\n");
  for (size_t i = 0; i < ((limit + WORDSIZE - 1) / WORDSIZE); i++) {
    word = vector[i];
    printf("word %lu: 0x%.*" WORDFMT "\n\t", i, (int) sizeof(word), word);
    for (size_t j = 0; j < WORDSIZE; j++) {
      printf("%u ", word & ((WORDTYPE)1 << j) && 1);
      if ((j+1) % 32 == 0)
        printf("\n\t");
      else if ((j+1) % 8 == 0)
        printf(" ");
    }
    printf("\n");
  }
}

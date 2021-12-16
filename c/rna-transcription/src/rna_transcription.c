#include "rna_transcription.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

static char transcribe(char nucleotide, bool *err) {
  switch (nucleotide) {
  case 'G':
    return 'C';
  case 'C':
    return 'G';
  case 'T':
    return 'A';
  case 'A':
    return 'U';
  case '\0':
    break;
  default:
    *err = true;
  }
  return '\0';
}

char *to_rna(const char *dna) {
  if (dna == NULL) {
    return NULL;
  }
  char *rna = malloc(strlen(dna) + 1);
  if (rna == NULL) {
    return NULL;
  }
  char *rnaptr = rna;
  bool err = false;
  while((*rnaptr++ = transcribe(*dna++, &err)));
  if (err) {
    free(rna);
    return NULL;
  }
  return rna;
}

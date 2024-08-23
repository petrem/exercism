#include <stdbool.h>
#include <stdint.h>
#include "protein_translation.h"

static inline bool is_little_endian();
static uint32_t to_codon_number(const char *const rna, size_t pos);

proteins_t proteins(const char *const rna) {
  proteins_t response = {true, 0, {0,0,0,0,0,0,0,0,0,0}};
  unsigned int k = 0;
  protein_t protein;

  for (size_t rna_index = 0; rna[rna_index] != '\0'; rna_index++, k = (k + 1) % 3) {
    if (k == 2) {
      uint32_t codon_number = to_codon_number(rna, rna_index - 2);
      switch (codon_number) {
      case AUG:
        protein = Methionine;
        break;
      case UUU:
      case UUC:
        protein = Phenylalanine;
        break;
      case UUA:
      case UUG:
        protein = Leucine;
        break;
      case UCU:
      case UCC:
      case UCA:
      case UCG:
        protein = Serine;
        break;
      case UAU:
      case UAC:
        protein = Tyrosine;
        break;
      case UGU:
      case UGC:
        protein = Cysteine;
        break;
      case UGG:
        protein = Tryptophan;
        break;
      case UAA:
      case UAG:
      case UGA:
        goto STOP;
        break;
      default:
        goto ERROR;
      }
      response.proteins[response.count++] = protein;
    }
  }

  if (k == 0) {
    goto STOP;
  }

  ERROR:
  response.valid = false;
  response.count = 0;

  STOP:
  return response;
}

static inline bool is_little_endian() {
  const unsigned int i = 1;
  const char *const c = (const char *const)&i;
  return *c == 1;
}

static uint32_t to_codon_number(const char *const rna, size_t pos) {
  uint32_t codon_number = 0U;

  if (is_little_endian()) {
    char *p = (char *)&codon_number;
    for (int k = 2; k >= 0; k--, p++) {
      *p = rna[pos + k];
    }
  } else {
    char *p = ((char*)&codon_number + 1);
    for (int k = 0; k < 3; k++, p++) {
      *p = rna[pos + k];
    }
  }

  return codon_number;
}

#ifndef PROTEIN_TRANSLATION_H
#define PROTEIN_TRANSLATION_H

#include <stdbool.h>
#include <stddef.h>

#define MAX_PROTEINS 10

typedef enum {
   Methionine,
   Phenylalanine,
   Leucine,
   Serine,
   Tyrosine,
   Cysteine,
   Tryptophan,
} protein_t;

typedef struct {
   bool valid;
   size_t count;
   protein_t proteins[MAX_PROTEINS];
} proteins_t;

proteins_t proteins(const char *const rna);

#define AUG 0x415547
#define UUU 0x555555
#define UUC 0x555543
#define UUA 0x555541
#define UUG 0x555547
#define UCU 0x554355
#define UCC 0x554343
#define UCA 0x554341
#define UCG 0x554347
#define UAU 0x554155
#define UAC 0x554143
#define UGU 0x554755
#define UGC 0x554743
#define UGG 0x554747
#define UAA 0x554141
#define UAG 0x554147
#define UGA 0x554741

#endif

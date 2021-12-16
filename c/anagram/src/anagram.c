#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <errno.h>

#include "anagram.h"


typedef struct sig_entry {
  unsigned int ch;
  unsigned int freq;
  struct sig_entry *next;
} sig_entry_t;

typedef struct {
  unsigned int ascii_letters[26];
  sig_entry_t *utf_letters;
} signature_t;

static signature_t *u8_signature(const char *word);
static enum anagram_status
u8_is_anagram(const char *word, const signature_t *word_signature, const char *candidate);
static bool u8_compare_signatures(const signature_t *sig1, const signature_t *sig2);
static void free_signature(signature_t *sig);

static char *strtolower(const char *word);
static void *calloc_or_die(size_t n_memb, size_t size);
static inline unsigned int min(unsigned int a, unsigned int b);

#define isascii(c) (((c) & 0x80) == 0)

/* UTF-8 routines, taken from https://www.cprogramming.com/tutorial/unicode.html */

/* note the isutf name is misleading: it actually returns true
   for the second, third or fourth byte in an UTF-8 encoded character
*/
#define isutf(c) (((c) & 0xC0) != 0x80)
static uint32_t u8_nextchar(const char *s, int *i);


void anagrams_for(const char *word, struct candidates *candidates) {
  char *lowword = strtolower(word);
  signature_t *word_signature = u8_signature(lowword);
  for (size_t i = 0; i < candidates->count; i++) {
    struct candidate *candidate = &candidates->candidate[i];
    candidate->is_anagram = u8_is_anagram(lowword, word_signature, candidate->candidate);
  }
  free(lowword);
  free_signature(word_signature);
}

/* returns the string with lowercased ascii letters, but not modifiying other characters */
static char *strtolower(const char *word) {
  int len = min(strlen(word), MAX_STR_LEN);
  int i = 0, prev_i = 0;
  char *dup = calloc_or_die(sizeof(char), len + 1);
  uint32_t ch;

  while (word[i]) {
    ch = u8_nextchar(word, &i);
    if (isascii(ch)) {
      dup[prev_i] = (char)tolower(ch);
    } else {
      for (int j = prev_i; j < i; j++) {
        dup[j] = word[j];
      }
    }
    prev_i = i;
  }
  return dup;
}

static inline unsigned int min(unsigned int a, unsigned int b) {
  return a < b ? a : b;
}

static void *calloc_or_die(size_t n_memb, size_t size) {
  void *ptr = calloc(n_memb, size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}

/* We could use a better data structure than a linked list for non-ASCII chars */
static signature_t *u8_signature(const char *word) {
  signature_t *sig = calloc_or_die(1, sizeof(signature_t));
  int i = 0;
  uint32_t ch;

  while (word[i]) {
    ch = u8_nextchar(word, &i);
    if (isascii(ch) && islower(ch)) {
      sig->ascii_letters[ch - 'a']++;
    } else {
      sig_entry_t *p = sig->utf_letters;
      sig_entry_t **before = &sig->utf_letters;
      for (; p && p->ch < ch; p = p->next) {
        before = &p->next;
      }
      if (p && p->ch == ch) {
          p->freq++;
      } else {
        *before = calloc_or_die(1, sizeof(sig_entry_t));
        **before = (sig_entry_t){ .next=p, .ch=ch, .freq=1 };
      }
    }
  }
  return sig;
}

static bool u8_compare_signatures(const signature_t *sig1, const signature_t *sig2) {
  for (unsigned int i = 0; i < 26 ; i++) {
    if (sig1->ascii_letters[i] != sig2->ascii_letters[i]) {
      return false;
    }
  }
  sig_entry_t *p1 = sig1->utf_letters, *p2 = sig2->utf_letters;
  for (; p1 && p2; p1 = p1->next, p2 = p2->next) {
    if (p1->ch != p2->ch || p1->freq != p2->freq) {
      return false;
    }
  }
  if (p1 != NULL || p2 != NULL) {
    return false;
  }
  return true;
}

static enum anagram_status
u8_is_anagram(const char *word, const signature_t *word_signature, const char *candidate) {
  char * lowcandidate = strtolower(candidate);
  enum anagram_status status = IS_ANAGRAM;
  if (strcmp(word, lowcandidate) == 0)
    status = NOT_ANAGRAM;
  else {
    signature_t *candidate_signature = u8_signature(lowcandidate);
    if (!u8_compare_signatures(word_signature, candidate_signature))
      status = NOT_ANAGRAM;
    free_signature(candidate_signature);
  }
  free(lowcandidate);
  return status;
}

static void free_signature(signature_t *sig) {
  if (sig != NULL) {
    sig_entry_t *p = sig->utf_letters, *q;
    while (p != NULL) {
      q = p;
      p = p->next;
      free(q);
    }
  }
  free(sig);
}


/* UTF-8 library */

static const uint32_t offsetsFromUTF8[6] = {
    0x00000000UL, 0x00003080UL, 0x000E2080UL,
    0x03C82080UL, 0xFA082080UL, 0x82082080UL
};


static uint32_t u8_nextchar(const char *s, int *i)
{
    uint32_t ch = 0;
    int sz = 0;

    do {
        ch <<= 6;
        ch += (unsigned char)s[(*i)++];
        sz++;
    } while (s[*i] && !isutf(s[*i]));
    ch -= offsetsFromUTF8[sz-1];

    return ch;
}

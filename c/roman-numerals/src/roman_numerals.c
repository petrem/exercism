#include "roman_numerals.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* largest 64bit integer has 19 digits */
#define MAX_ARABIC 19

/* assume numeral <= 9999, largest is MMMMMMMMMDCCCLXXXVIII (9888) */
#define MAX_ROMAN 22


#define REST(offset) (MAX_ROMAN - (offset))

static char *digits[4][10] = {
    {"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"},
    {"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"},
    {"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"},
    {"", "M", "MM", "MMM", "MMMM", "MMMMM", "MMMMMM", "MMMMMMM",
     "MMMMMMMM", "MMMMMMMMM"}
};

static void *
calloc_or_die(size_t nmemb, size_t size)
{
  void *ptr = calloc(nmemb, size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}

static size_t append(char *ptr, int offset, char *repr) {

  int written;

  if (REST(offset) <= 0 || ptr == NULL) {
    exit(1);
  }

  written = snprintf(ptr + offset, REST(offset), "%s", repr);
  if (written < 0 || written >= REST(offset)) {
    fputs("Error appending to roman numeral\n", stderr);
    exit(1);
  }
  return written;
}


char *to_roman_numeral(int number) {
  char arabic_numeral[MAX_ARABIC + 1];
  char *ptr = arabic_numeral;
  char *roman_numeral = calloc_or_die(MAX_ARABIC, 1);
  int written, digit;

  written = snprintf(arabic_numeral, MAX_ARABIC + 1, "%d", number);
  if (written < 0) {
    perror("Error converting to string representation\n");
  }
  if(written >= MAX_ARABIC + 1) {
    fputs("Looks like your number is larger than a 64 bit integer\n", stderr);
    exit(1);
  }

  int suite = strlen(arabic_numeral) - 1;
  if (suite > 3) {
    fputs("Try a smaller number!\n", stderr);
    exit(1);
  }

  written = 0;
  for (;*ptr != '\0'; ptr++, suite--) {
    digit = *ptr - '0';
    written += append(roman_numeral, written, digits[suite][digit]);
  }
  return roman_numeral;
}

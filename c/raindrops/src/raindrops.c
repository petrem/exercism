#include "raindrops.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static inline bool is_divisible_by(unsigned n, unsigned m) {
  return n % m == 0;
}

#define RESPONSE_SIZE 16
#define REST(offset) (RESPONSE_SIZE - (offset))

static size_t append(char *ptr, int offset, const char *fmt, ...) {
  va_list args;
  int written;

  if (REST(offset) <= 0) {
    exit(1);
  }
  va_start(args, fmt);
  written = vsnprintf(ptr + offset, REST(offset), fmt, args);
  va_end(args);
  if (written < 0 || written > REST(offset)) {
    exit(1);
  }
  return written;
}

void convert(char *result, unsigned drops) {
  static const char *raindrop_words[] = { "Pling", "Plang", "Plong" };
  static const int factors[] = { 3, 5, 7 };
  char *ptr = result;
  int written = 0;

  for(size_t i = 0; i < sizeof(factors) / sizeof(int); i++) {
    if (is_divisible_by(drops, factors[i])) {
      written += append(ptr, written, raindrop_words[i]);
    }
  }
  if (written == 0) {
    append(ptr, written, "%u", drops);
  }
}

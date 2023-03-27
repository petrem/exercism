#include <string.h>
#include "binary.h"

int convert(const char *input) {
  int acc = 0;
  while (*input && *(input) == '0') input++;
  for (;*input; input++) {
    acc <<= 1;
    switch (*input) {
    case '1':
      acc |= 1;
      break;
    case '0':
      break;
    default:
      return INVALID;
    }
  }
  return acc;
}

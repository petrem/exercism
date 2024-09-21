#ifndef ALL_YOUR_BASE_H
#define ALL_YOUR_BASE_H

#include <stdint.h>
#include <stdlib.h>

#define DIGITS_ARRAY_SIZE 64

typedef int8_t digit_t;
typedef int16_t base_t;

size_t rebase(digit_t digits[], base_t input_base, base_t output_base, size_t input_length);

#endif

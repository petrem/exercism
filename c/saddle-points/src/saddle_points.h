#ifndef SADDLE_POINTS_H
#define SADDLE_POINTS_H

#include <stddef.h>
#include <stdint.h>

typedef struct {
  size_t row, column;
} saddle_point_t;

typedef struct {
  size_t count;
  saddle_point_t points[];  // Thanks, https://stackoverflow.com/questions/2060974/how-to-include-a-dynamic-array-inside-a-struct-in-c
} saddle_points_t;

saddle_points_t *saddlePoints(size_t cols, size_t rows, uint8_t matrix[rows][cols]);

#endif

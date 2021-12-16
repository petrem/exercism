#include "saddle_points.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>

#include <stdio.h>


typedef struct {
  bool is_min, is_max;
} flags;

static inline void add_saddle_point(saddle_points_t *saddle_points, size_t x, size_t y);
static void *malloc_or_die(size_t size);
static void check_max(size_t rows, size_t cols, flags extrema[rows][cols], uint8_t cell, size_t row, size_t col, uint8_t rows_max[]);
static void check_min(size_t rows, size_t cols, flags extrema[rows][cols], uint8_t cell, size_t row, size_t col, uint8_t cols_min[]);

//TODO: reduce list of parameters for check_min|max ?
//TODO: could I (reasonably) write just one function instead of check_min|max?

saddle_points_t *saddlePoints(size_t rows, size_t cols, uint8_t matrix[rows][cols]) {
  /* assume the worst: all cells are saddle points */
  saddle_points_t *saddle_points = malloc_or_die(sizeof(saddle_points_t)
                                                 + cols * rows * sizeof(saddle_point_t));
  saddle_points->count = 0;

  if (cols > 0 && rows > 0) {
    flags extrema[rows][cols];
    memset(extrema, 0, sizeof(extrema));

    uint8_t rows_max[rows];
    for (size_t i = 0; i < rows; i++) {
      rows_max[i] = 0U;
      extrema[i][0].is_max = true;
    }

    uint8_t cols_min[cols];
    for (size_t j = 0; j < cols; j++) {
      cols_min[j] = UINT8_MAX;
      extrema[0][j].is_min = true;
    }
    /* Another way to initialize minimums and maximums would be: */
    /*
    rows_max[0] = matrix[0][0];

    for (size_t j = 0; j < cols; j++) {
      uint8_t cell = matrix[0][j];
      cols_min[j] = cell;
      extrema[0][j].is_min = true;
      check_max(rows, cols, extrema, cell, 0, j, rows_max);
    }

    for (size_t i = 1; i < rows; i++) {
      uint8_t cell = matrix[i][0];
      extrema[i][0].is_max = true;
      rows_max[i] = cell;
      check_min(rows, cols, extrema, cell, i, 0, cols_min);
    }
    */
    /* With this, the following `for`s would start from 1. But it looks
       too convoluted for what it would possibly achieve
    */

    /* check for minimums and maximums */
    for (size_t i = 0; i < rows; i++) {
      for (size_t j = 0; j < cols; j++) {
        uint8_t cell = matrix[i][j];
        check_max(rows, cols, extrema, cell, i, j, rows_max);
        check_min(rows, cols, extrema, cell, i, j, cols_min);
      }
    }
    /* collect results */
    for (size_t i = 0; i < rows; i++) {
      for (size_t j = 0; j < cols; j++) {
        if (extrema[i][j].is_min && extrema[i][j].is_max)
          add_saddle_point(saddle_points, i, j);
      }
    }
  }
  /* now that we know the counts, we can free some memory */
  saddle_points = realloc(saddle_points,
                          sizeof(saddle_points_t)
                          + saddle_points->count * sizeof(saddle_point_t));
  return saddle_points;
}

static void check_max(size_t rows, size_t cols, flags extrema[rows][cols], uint8_t cell, size_t row, size_t col, uint8_t rows_max[]) {
  if (cell >= rows_max[row]) {
    extrema[row][col].is_max = true;
    if (cell > rows_max[row]) {
        rows_max[row] = cell;
        for (size_t k = 0; k < col; k++) {
          extrema[row][k].is_max = false;
        }
    }
  }
}

static void check_min(size_t rows, size_t cols, flags extrema[rows][cols], uint8_t cell, size_t row, size_t col, uint8_t cols_min[]) {
  if (cell <= cols_min[col]) {
    extrema[row][col].is_min = true;
    if (cell < cols_min[col]) {
        cols_min[col] = cell;
        for (size_t k = 0; k < row; k++) {
          extrema[k][col].is_min = false;
        }
    }
  }
}

static inline void add_saddle_point(saddle_points_t *saddle_points, size_t x, size_t y) {
  saddle_point_t sp = (saddle_point_t){x + 1, y + 1};
  saddle_points->points[saddle_points->count++] = sp;
}

static void *malloc_or_die(size_t size) {
  void *ptr = malloc(size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}

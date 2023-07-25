#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include "pascals_triangle.h"

#define _DEBUG 1

static void *safe_calloc(size_t, size_t);

#if _DEBUG == 1
#include <stdio.h>
static void print_triangle(size_t rows, uint8_t *triangle[rows]);
#endif

void free_triangle(uint8_t **triangle, size_t rows) {
  for (size_t row = 0; row < rows; row++)
    free(triangle[row]);
  free(triangle);
}

uint8_t **create_triangle(size_t rows) {
  size_t min_rows = rows > 0 ? rows : 1;
  uint8_t **triangle = safe_calloc(min_rows, sizeof(uint8_t *));
  uint8_t left_value = rows > 0 ? 1 : 0;
  for (size_t row = 0; row < min_rows; row++) {
    triangle[row] = safe_calloc(min_rows, sizeof(uint8_t));
    triangle[row][0] = left_value;
    triangle[row][row] = left_value;
    for (size_t col = 1; col < row; col++)
      /* we could go just to (row+1)/2 and mirror the values,
       but we'd probably need an `if` for the middle value of even rows,
      which might be worse (though... maybe not)*/
      triangle[row][col] = triangle[row - 1][col] + triangle[row - 1][col - 1];
  }

#if _DEBUG == 1
  print_triangle(min_rows, triangle);
#endif
  return triangle;
}

static void *safe_calloc(size_t nmemb, size_t size) {
  void *p = calloc(nmemb, size);
  if (p == NULL) {
    exit(ENOMEM);
  }
  return p;
}

#if _DEBUG == 1
static void print_triangle(size_t rows, uint8_t *triangle[rows]) {
  puts("\n");
  for (size_t i=0; i<rows; i++) {
    for (size_t j=0; j<rows; j++)
      printf("%4u", triangle[i][j]);
    puts("\n");
  }
  puts("\n");
}
#endif

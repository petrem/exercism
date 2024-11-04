#include "minesweeper.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static char *strdup_or_die(const char *source);

int DELTAS[][2] = {{-1, -1}, {-1, 0}, {-1, 1},
                   { 0, -1},          { 0, 1},
                   { 1, -1}, { 1, 0}, { 1, 1}};

char **annotate(const char **minefield, const size_t rows) {
  if (rows < 1) {
    return NULL;
  }
  char **annotation = malloc((rows + 1) * sizeof(char *));
  if (annotation == NULL)
    exit(EXIT_FAILURE);
  
  memset(annotation, 0, (rows + 1) * sizeof(char *));
  for (size_t k = 0; k < rows; k++)
    annotation[k] = strdup_or_die(minefield[k]);

  const size_t cols = strlen(annotation[0]);
  for (size_t row = 0; row < rows; row++) {
    if (strlen(annotation[row]) != cols)
      exit(EXIT_FAILURE);
    for (size_t col = 0; col < cols; col++)
      if (annotation[row][col] == ' ') {
        unsigned int mines = 0;
        for (size_t k = 0; k < 8; k ++)
          /* Fragile ish: assumes underflow will not be smaller than rows/cols */
          if (row + DELTAS[k][0] < rows && col + DELTAS[k][1] < cols && annotation[row + DELTAS[k][0]][col + DELTAS[k][1]] == '*')
            mines++;
        annotation[row][col] = mines ? '0' + mines : ' ';
      }
  }
  return annotation;
}

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(arr[0]))

void free_annotation(char **annotation) {
  if (annotation != NULL) {
    for (size_t k = 0; annotation[k] != NULL; k++)
      free(annotation[k]);
    free(annotation);
  }

}

static char *strdup_or_die(const char *source) {
  char *dest = malloc(strlen(source) + 1);
  if (dest == NULL) {
    exit(EXIT_FAILURE);
  }
  strcpy(dest, source);
  return dest;
}

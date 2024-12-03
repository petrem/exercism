#include "diamond.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Return a char * array where the last pointer is NULL */
char **make_diamond(const char letter) {
  assert(isupper(letter));
  // maximum size can be 2 * 25 + 1 so it definitely fits an int
  int offset = letter - 'A';
  int square_size = 1 + 2 * offset;
  char **diamond = calloc(square_size + 1, sizeof(char *));
  assert(diamond);
  for(int line = 0; line <= offset; line++) {
    diamond[line] = malloc((square_size + 1) * sizeof(char));
    assert(diamond[line]);
    memset(diamond[line], ' ', square_size);
    diamond[line][square_size] = '\0';
    diamond[line][offset - line] = 'A' + line;
    diamond[line][offset + line] = 'A' + line;
  }
  for (int line = offset + 1; line < square_size; line++) {
    diamond[line] = malloc((square_size + 1) * sizeof(char));
    assert(diamond[line]);
    strcpy(diamond[line], diamond[square_size - 1 - line]);
  }
  return diamond;
}

void free_diamond(char **diamond) {
  if (diamond == NULL)
    return;
  for (size_t k = 0; diamond[k] != NULL; k++)
    free(diamond[k]);
  free(diamond);
}

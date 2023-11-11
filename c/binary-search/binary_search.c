#include "binary_search.h"
#include <stddef.h>

int *binary_search(int needle, int haystack[], size_t length) {
  if (haystack != NULL) {
    int left = 0, right = length - 1;
    while (left <= right) {
      int middle = left + ((right - left) / 2);
      if (needle < haystack[middle]) {
        right = middle - 1;
      }
      else if (needle > haystack[middle]) {
        left = middle + 1;
      }
      else {
        return &haystack[middle];
      }
    }
  }
  return NULL;
}

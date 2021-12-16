#include "series.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>


static inline void *calloc_or_die(size_t nmemb, size_t size);


series_results_t series(char *input_text, unsigned int substring_length) {
  series_results_t error = {.substring_count = 0, .substring = NULL};
  if (input_text == NULL)
    return error;

  unsigned int input_len = strlen(input_text);
  if (substring_length == 0 || input_len < substring_length)
    return error;

  series_results_t result;
  result.substring_count = input_len - substring_length + 1;
  result.substring = calloc_or_die(result.substring_count, sizeof(char *));
  for (unsigned int i = 0; i < result.substring_count; i++) {
    result.substring[i] = calloc_or_die(1, substring_length + 1);
    memcpy(result.substring[i], input_text + i, substring_length);
    result.substring[i][substring_length] = '\0';
  }
  return result;
}


static inline void *calloc_or_die(size_t nmemb, size_t size) {
  void *mem = calloc(nmemb, size);
  if (mem == NULL)
    exit(ENOMEM);
  return mem;
}

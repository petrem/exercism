#ifndef WORD_COUNT_ALLOC_H
#define WORD_COUNT_ALLOC_H

#include <stdlib.h>


void *malloc_or_die(size_t size);
void *calloc_or_die(size_t nmemb, size_t size);
int realloc_safe(void **ptr, size_t old_size, size_t new_size);

#endif

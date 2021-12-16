#ifndef WORD_COUNT_ALLOC_H
#define WORD_COUNT_ALLOC_H

#include <stdlib.h>


void *malloc_or_die(size_t size);
void *calloc_or_die(size_t nmemb, size_t size);

#endif

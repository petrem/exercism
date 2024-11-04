#include "circular_buffer.h"

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>


static void *calloc_or_die(size_t nmemb, size_t size);
static inline bool is_buffer_full(circular_buffer_t *buffer);
static inline size_t addm(size_t a, size_t b, size_t modulus);
static inline size_t subm(size_t a, size_t b, size_t modulus);
static inline size_t min(size_t a, size_t b);

circular_buffer_t *new_circular_buffer(size_t capacity) {
  circular_buffer_t *buffer = calloc_or_die(1, sizeof(circular_buffer_t));
  buffer->data = calloc_or_die(capacity, sizeof(buffer_value_t));
  buffer->capacity = capacity;
  return buffer;
}

status_t delete_buffer(circular_buffer_t *buffer) {
  if (buffer != NULL) {
    if (buffer->data != NULL) {
      free(buffer->data);
    }
    free(buffer);
  }
  return EXIT_SUCCESS;
}

status_t read(circular_buffer_t *buffer, buffer_value_t *value) {
  if (buffer->size > 0) {
    size_t tail = subm(buffer->head + buffer->capacity, buffer->size, buffer->capacity);
    *value = buffer->data[tail];
    buffer->data[tail] = 0;
    buffer->size -= 1;
    return EXIT_SUCCESS;
  }
  errno = ENODATA;
  return EXIT_FAILURE;
}

status_t write(circular_buffer_t *buffer, buffer_value_t value) {
  if (buffer == NULL || buffer->data == NULL) {
    errno = EINVAL;
    return EXIT_FAILURE;
  }
  if(is_buffer_full(buffer)) {
    errno = ENOBUFS;
    return EXIT_FAILURE;
  }
  return overwrite(buffer, value);
}

void clear_buffer(circular_buffer_t *buffer) {
  buffer->size = 0;
  memset(buffer->data, 0, buffer->capacity * sizeof(buffer_value_t));

}

status_t overwrite(circular_buffer_t *buffer, buffer_value_t value) {
  if (buffer != NULL && buffer->data != NULL) {
    buffer->data[buffer->head] = value;
    buffer->head = addm(buffer->head, 1, buffer->capacity);
    buffer->size = min(buffer->capacity, buffer->size + 1);
    return EXIT_SUCCESS;
  }
  errno = EINVAL;
  return EXIT_FAILURE;
}

static void *calloc_or_die(size_t nmemb, size_t size) {
  void *ptr = calloc(nmemb, size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}


static inline bool is_buffer_full(circular_buffer_t *buffer) {
  return buffer->size == buffer->capacity;
}

static inline size_t addm(size_t a, size_t b, size_t modulus) {
  return (a + b) % modulus;
}

static inline size_t subm(size_t a, size_t b, size_t modulus) {
  return (a - b) % modulus;
}

static inline size_t min(size_t a, size_t b) {
  return a < b ? a : b;
}

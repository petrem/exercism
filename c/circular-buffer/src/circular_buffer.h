#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H

#include <stddef.h>
#include <stdint.h>

typedef int buffer_value_t;
typedef struct {
  size_t capacity;
  size_t head;
  size_t size;
  buffer_value_t *data;
} circular_buffer_t;
typedef int16_t status_t;


circular_buffer_t *new_circular_buffer(size_t capacity);
status_t read(circular_buffer_t *buffer, buffer_value_t *value);
status_t write(circular_buffer_t *buffer, buffer_value_t value);
status_t overwrite(circular_buffer_t *buffer, buffer_value_t value);
status_t delete_buffer(circular_buffer_t *buffer);
void clear_buffer(circular_buffer_t *buffer);

#endif

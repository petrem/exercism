#include "list_ops.h"

#include <stdlib.h>
#include <string.h>


#define REALLOC_FRACTION 0.75

static inline list_t *realloc_list(list_t *list, size_t length);


list_t *new_list(size_t length, list_element_t elements[]) {
  list_t *list = realloc_list(NULL, length);
  if (list != NULL) {
    list->length = length;
    memcpy(list->elements, elements, length * sizeof(list_element_t));
  }
  return list;
}

list_t *append_list(list_t *list1, list_t *list2) {
  list_t *list = realloc_list(NULL, list1->length + list2->length);
  if (list != NULL) {
    list->length = list1->length + list2->length;
    memcpy(list->elements,  list1->elements, list1->length * sizeof(list_element_t));
    memcpy(list->elements + list1->length,
           list2->elements, list2->length * sizeof(list_element_t));
  }
  return list;
}

list_t *filter_list(list_t *list, bool(*filter) (list_element_t value)) {
  list_t *l = realloc_list(NULL, list->length);
  if (l != NULL) {
    l->length = 0;
    for(size_t i = 0; i < list->length; i++) {
      if (filter(list->elements[i])) {
        l->elements[l->length++] = list->elements[i];
      }
    }
    if (l->length < list->length * REALLOC_FRACTION) {
      l = realloc_list(l, l->length);
    }
  }

  return l;
}

size_t length_list(list_t *list) {
  return list->length;
}

list_t *map_list(list_t * list, list_element_t(*map) (list_element_t value)) {
  list_t *l = realloc_list(NULL, list->length);
  if (l != NULL) {
    l->length = list->length;
    for(size_t i = 0; i < list->length; i++) {
      l->elements[i] = map(list->elements[i]);
    }
  }
  return l;
}

list_element_t foldl_list(list_t * list, list_element_t initial,
                        list_element_t(*foldl) (list_element_t value,
                                              list_element_t initial)) {
  list_element_t acc = initial;
  for (size_t i = 0; i < list->length; i++) {
    acc = foldl(list->elements[i], acc);
  }
  return acc;
}

list_element_t foldr_list(list_t * list, list_element_t initial,
                        list_element_t(*foldr) (list_element_t value,
                                              list_element_t initial)) {
  list_element_t acc = initial;
  for (size_t i = list->length; i > 0; i--) {
    acc = foldr(list->elements[i - 1], acc);
  }
  return acc;
}

list_t *reverse_list(list_t *list) {
  list_t *reversed = realloc_list(NULL, list->length);
  if (reversed != NULL) {
    reversed->length = 0;
    for(size_t i = list->length; i > 0; i--) {
      reversed->elements[reversed->length++] = list->elements[i - 1];
    }
  }
  return reversed;
}

void delete_list(list_t * list) {
  if (list != NULL) {
    free(list);
  }
}

static inline list_t *realloc_list(list_t *list, size_t length) {
  return (list_t *)realloc(list, sizeof(list_t) + length * sizeof(list_element_t));
}

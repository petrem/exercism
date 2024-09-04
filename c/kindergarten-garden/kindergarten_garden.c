#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "kindergarten_garden.h"

#define PLANT_AT(row, pos, offset) ((row)[2*(pos) + (offset)])
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
static plant_t read_plant(char plant);
static size_t get_student_position(const char *student);

plants_t plants(const char * diagram, const char *student) {
  size_t pos = get_student_position(student);
  const char *row1 = diagram;
  const char *first_newline = strpbrk(diagram, "\n");
  if (first_newline == NULL || *(first_newline+1) == '\0') {
    fputs("Second row not found!\n", stderr);
    exit(EXIT_FAILURE);
  }
  const char *row2 = first_newline + 1;
  return (plants_t){{
      read_plant(PLANT_AT(row1, pos, 0)),
      read_plant(PLANT_AT(row1, pos, 1)),
      read_plant(PLANT_AT(row2, pos, 0)),
      read_plant(PLANT_AT(row2, pos, 1))
    }};
}


static size_t get_student_position(const char *student) {
  static const char * students[] = { "Alice", "Bob", "Charlie", "David", "Eve",
                                     "Fred", "Ginny", "Harriet", "Ileana", "Joseph",
                                     "Kincaid", "Larry" };
  for(size_t k = 0; k < ARRAY_SIZE(students); k++) {
    if (0 == strncmp(students[k], student, strlen(students[k]))) {
      return k;
    }
  }
  return -1;
}

static plant_t read_plant(char plant) {
  switch(plant) {
  case 'C':
    return CLOVER;
  case 'G':
    return GRASS;
  case 'R':
    return RADISHES;
  case 'V':
    return VIOLETS;
  default:
    fprintf(stderr, "Unknown plant representation: '%c' (%d)\n", plant, (int) plant);
    exit(EXIT_FAILURE);
  }
}

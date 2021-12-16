#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "../src/bitvector.h"
#include "vendor/unity.h"


static WORDTYPE *bitvector;

void setUp(void)
{
  bitvector = NULL;
}

void tearDown(void)
{
  if (bitvector != NULL)
    free(bitvector);
}

typedef struct {
  unsigned index;
  unsigned value;
} from_list;

static size_t get_max_index(from_list *inputs, size_t count) {
  size_t max_index = 0;
  for (size_t i = 0; i < count; i++)
    if (inputs[i].index > max_index)
      max_index = inputs[i].index;
  return max_index;
}

static void bitvector_set_indexed(WORDTYPE *bitvector, from_list *inputs, size_t input_count) {
  for (size_t i = 0; i < input_count; i++)
    bitvector_set(bitvector, inputs[i].index, inputs[i].value);
}

static void bitvector_check_indexed(WORDTYPE *bitvector, from_list *expected, size_t expected_count, size_t vector_length) {
  char message[50];
  for (size_t i = 0; i < vector_length; i++) {
    bool found = false;
    for (size_t j = 0; j < expected_count; j++) {
      snprintf(message, 50, "Vector idx: %lu, expect pos: %lu, expect idx: %u", i, j, expected[j].index);
      if (i == expected[j].index) {
        TEST_ASSERT_EQUAL_MESSAGE(expected[j].value, bitvector_get(bitvector, i), message);
        found = true;
        break;
      }
    }
    if (!found) {
      TEST_ASSERT_EQUAL_MESSAGE(0, bitvector_get(bitvector, i), message);
    }
  }
}

static void test_bitvector_of_1(void)
{
  from_list inputs1[] = {{0, 1}};
  from_list inputs2[] = {{0, 0}};
  size_t input_count = 1;
  size_t max_index = 0;
  bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs1, input_count);
  bitvector_check_indexed(bitvector, inputs1, input_count, max_index);
  bitvector_set_indexed(bitvector, inputs2, input_count);
  bitvector_check_indexed(bitvector, inputs2, input_count, max_index);
}

static void test_set_few_values_in_first_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{0, 1}, {2, 1}, {5, 1}, {7, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_second_byte(void) {
  //  TEST_IGNORE();
  from_list inputs[] = {{8, 1}, {10, 1}, {13, 1}, {15, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_third_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{16, 1}, {17, 1}, {22, 1}, {23, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_fourth_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{24, 1}, {27, 1}, {28, 1}, {31, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_fifth_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{32, 1}, {34, 1}, {35, 1}, {39, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_sixth_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{40, 1}, {42, 1}, {45, 1}, {47, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_seventh_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{48, 1}, {50, 1}, {52, 1}, {55, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_eighth_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{56, 1}, {59, 1}, {61, 1}, {63, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_nineth_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{64, 1}, {66, 1}, {70, 1}, {71, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_tenth_byte(void) {
  //TEST_IGNORE();
  from_list inputs[] = {{72, 1}, {74, 1}, {75, 1}, {79, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_set_few_values_in_eleventh_byte(void) {
  //  TEST_IGNORE();
  from_list inputs[] = {{80, 1}, {82, 1}, {84, 1}, {87, 1}};
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, inputs, input_count, max_index);
}

static void test_boundary_indexes(void) {
  //TEST_IGNORE();
  from_list inputs1[] = {
      { 0, 1}, { 7, 1}, { 8, 1}, {15, 1}, {16, 1}, {23, 1}, {24, 1}, {31, 1},
      {32, 1}, {39, 1}, {40, 1}, {47, 1}, {48, 1}, {55, 1}, {56, 1}, {63, 1},
      {64, 1}, {71, 1}, {72, 1}, {79, 1}, {80, 1}, {87, 1}, {88, 1}, {95, 1}
  };
  size_t input_count = sizeof(inputs1) / sizeof(from_list);
  size_t max_index = get_max_index(inputs1, input_count);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs1, input_count);
  bitvector_check_indexed(bitvector, inputs1, input_count, max_index);
  // set them back to zero; keep inputs2 same length as inputs1!
  from_list inputs2[] = {
      { 0, 0}, { 7, 0}, { 8, 0}, {15, 0}, {16, 0}, {23, 0}, {24, 0}, {31, 0},
      {32, 0}, {39, 0}, {40, 0}, {47, 0}, {48, 0}, {55, 0}, {56, 0}, {63, 0},
      {64, 0}, {71, 0}, {72, 0}, {79, 0}, {80, 0}, {87, 0}, {88, 0}, {95, 0}
  };
  bitvector_set_indexed(bitvector, inputs2, input_count);
  bitvector_check_indexed(bitvector, inputs2, input_count, max_index);
}

static void test_set_several_values_several_times(void) {
  //TEST_IGNORE();
  from_list inputs[] = {
      {109, 0}, {110, 1}, {111, 1}, {112, 0}, {113, 0}, {114, 1}, {115, 1}, {116, 0},
      {109, 0}, {110, 0}, {111, 1}, {112, 0}, {113, 1}, {114, 1}, {115, 0}, {116, 1},
      {109, 0}, {110, 1}, {111, 0}, {112, 1}, {113, 0}, {114, 1}, {115, 0}, {116, 1},
  };
  size_t input_count = sizeof(inputs) / sizeof(from_list);
  size_t max_index = get_max_index(inputs, input_count);
  from_list expected[] = {
      {110, 1}, {112, 1}, {114, 1}, {116, 1}
  };
  size_t expected_count = sizeof(expected) / sizeof(from_list);
  WORDTYPE *bitvector = bitvector_init(max_index);
  bitvector_set_indexed(bitvector, inputs, input_count);
  bitvector_check_indexed(bitvector, expected, expected_count, max_index);
}


int main(void)
{
   UnityBegin("test/test_bitvector.c");

   RUN_TEST(test_bitvector_of_1);
   RUN_TEST(test_set_few_values_in_first_byte);
   RUN_TEST(test_set_few_values_in_second_byte);
   RUN_TEST(test_set_few_values_in_third_byte);
   RUN_TEST(test_set_few_values_in_fourth_byte);
   RUN_TEST(test_set_few_values_in_fifth_byte);
   RUN_TEST(test_set_few_values_in_sixth_byte);
   RUN_TEST(test_set_few_values_in_seventh_byte);
   RUN_TEST(test_set_few_values_in_eighth_byte);
   RUN_TEST(test_set_few_values_in_nineth_byte);
   RUN_TEST(test_set_few_values_in_tenth_byte);
   RUN_TEST(test_set_few_values_in_eleventh_byte);
   RUN_TEST(test_boundary_indexes);
   RUN_TEST(test_set_several_values_several_times);

   return UnityEnd();
}

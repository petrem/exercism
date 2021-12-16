#include "allergies.h"
#include <string.h>


static inline unsigned int check(unsigned int score, unsigned int index) {
  return score & 1 << index;
}

bool is_allergic_to(allergen_t allergen, unsigned int score) {
  return check(score, allergen) && true;
}

allergen_list_t get_allergens(unsigned int score) {
  allergen_list_t allergens;
  memset(&allergens, 0, sizeof(allergen_list_t));
  for (unsigned int i = 0; i < ALLERGEN_COUNT; i++) {
    if (check(score, i)) {
      allergens.count++;
      allergens.allergens[i] = true;
    }
  }
  return allergens;
}

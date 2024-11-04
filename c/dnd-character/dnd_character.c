#include "dnd_character.h"

#include <math.h>
#include <stdlib.h>
#include <time.h>

int ability(void) {
  int dice[4] = { rand(), rand(), rand(), rand()};
  long min = dice[0], sum = dice[0];
  for (size_t k = 1; k < 4; k++) {
    sum += dice[k];
    min = min <= dice[k]? min : dice[k];
  }
  return 3 + (sum - min) % 16;
}

int modifier(int score) {
  return floorl((score - 10) / 2.0);
}

dnd_character_t make_dnd_character(void) {
  srand(time(NULL));
  dnd_character_t dnd = {ability(), ability(), ability(), ability(), ability(), ability(), 0};
  dnd.hitpoints = 10 + modifier(dnd.constitution);
  return dnd;
}

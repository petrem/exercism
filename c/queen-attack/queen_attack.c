#include <stdlib.h>
#include "queen_attack.h"

attack_status_t can_attack(position_t queen_1, position_t queen_2) {
  if (queen_1.row > 7
      || queen_1.column > 7
      || queen_2.row > 7
      || queen_2.column > 7
      || (queen_1.row == queen_2.row && queen_1.column == queen_2.column))
    return INVALID_POSITION;
  return (queen_1.row == queen_2.row
          || queen_1.column == queen_2.column
          || abs(queen_1.column - queen_2.column) == abs(queen_1.row - queen_2.row));
}

#include <stdlib.h>
#include "robot_simulator.h"

typedef void (*action_function_t)(robot_status_t *);

static inline void turn_left(robot_status_t *robot);
static inline void turn_right(robot_status_t *robot);
static inline void advance(robot_status_t *robot);
static inline robot_position_t add_position(robot_position_t p1, robot_position_t p2);
static inline action_function_t get_action(char code);

robot_status_t robot_create(robot_direction_t direction, int x, int y) {
  return (robot_status_t){direction, (robot_position_t){x, y}};
}

void robot_move(robot_status_t *robot, const char *commands) {
  if (commands != NULL) {
    for (const char *cp = commands; *cp; cp++) {
      get_action(*cp)(robot);
    }
  }
}


static inline void turn_left(robot_status_t *robot) {
  robot->direction = (robot->direction + 3) % 4;
}

static inline void turn_right(robot_status_t *robot) {
  robot->direction = (robot->direction + 5) % 4;
}

static inline void advance(robot_status_t *robot) {
  static robot_position_t displacement[] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
  robot->position = add_position(robot->position, displacement[robot->direction]);
}

static inline robot_position_t add_position(robot_position_t p1, robot_position_t p2) {
  return (robot_position_t){p1.x + p2.x, p1.y + p2.y};
}

static inline action_function_t get_action(char code) {
  switch(code) {
  case 'L':
    return turn_left;
  case 'R':
    return turn_right;
  case 'A':
    return advance;
  default:
    return (action_function_t)NULL;
  }
}

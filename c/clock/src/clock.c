#include "clock.h"

#include <stdio.h>
#include <stdbool.h>


struct clock_hm {
  int hour;
  int minute;
};

static int div_floor(int num, int div);
static struct clock_hm extract_clock(clock_t clock);

#define CLOCK_FMT(precision) "%" #precision "d:%" #precision "d"

clock_t clock_create(int hour, int minute) {
  clock_t clock;
  hour += div_floor(minute, 60);
  minute = minute % 60;
  if (minute < 0)
    minute += 60;
  hour = hour % 24;
  if (hour < 0)
    hour += 24;
  snprintf(clock.text, MAX_STR_LEN, CLOCK_FMT(.2), hour, minute);
  return clock;
}


clock_t clock_add(clock_t clock, int minute_add) {
  struct clock_hm c = extract_clock(clock);
  return clock_create(c.hour, c.minute + minute_add);
}

clock_t clock_subtract(clock_t clock, int minute_subtract) {
  struct clock_hm c = extract_clock(clock);
  return clock_create(c.hour, c.minute - minute_subtract);
}

bool clock_is_equal(clock_t a, clock_t b) {
  struct clock_hm ca = extract_clock(a);
  struct clock_hm cb = extract_clock(b);
  return (ca.hour == cb.hour && ca.minute == cb.minute);
}

static int div_floor(int num, int div) {
    return num >= 0 ? num / div : -1 - (-1 - num) / div;
}


static struct clock_hm extract_clock(clock_t clock) {
  struct clock_hm c;
  sscanf(clock.text, CLOCK_FMT(), &c.hour, &c.minute);
  return c;
}

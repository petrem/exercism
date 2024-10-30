#include "meetup.h"

#include <string.h>
#include <stdbool.h>
#include <time.h>


static int get_weekday(const char *dayname);
static inline bool is_leap(unsigned int year);
static inline unsigned int last_day_of_month(unsigned int year, unsigned int month);
static inline unsigned int get_weekday_for_date(unsigned int year, unsigned int month, unsigned int day);
static inline unsigned int get_base_day(unsigned int year, unsigned int month, const char *week);


int meetup_day_of_month(unsigned int year, unsigned int month, const char *week,
                        const char *day_of_week) {
  int weekday = get_weekday(day_of_week);
  if (weekday < 0)
    goto fail;
  unsigned int base_day = get_base_day(year, month, week);
  if (base_day == (unsigned int)-1)
    goto fail;
  unsigned int base_weekday = get_weekday_for_date(year, month, base_day);
  if (base_weekday == (unsigned int)-1)
    goto fail;
  unsigned int day = base_day + (7 + weekday - base_weekday) % 7;
  if (day > last_day_of_month(year, month))
    goto fail;
  return day;
 fail:
  return -1;
}

static int get_weekday(const char *dayname) {
  static const char *days[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
  for (int i = 0; i < 7; ++i)
    if (!(strcmp(dayname, days[i])))
      return i;
  return -1;
}

static inline bool is_leap(unsigned int year) {
  return !(year % 4) && ((year % 100) || !(year % 400));
}

static inline unsigned int last_day_of_month(unsigned int year, unsigned int month) {
  static const int days[] = {31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  return month == 2 ? (is_leap(year) ? 29: 28) : days[month - 1];
}

static inline unsigned int get_weekday_for_date(unsigned int year, unsigned int month, unsigned int day) {
  struct tm tm = {.tm_year = year - 1900, .tm_mon = month - 1, .tm_mday = day};
  if (mktime(&tm) == (time_t)-1)
    return (unsigned int)-1;
  return tm.tm_wday;
}

static inline unsigned int get_base_day(unsigned int year, unsigned int month, const char *week) {
  static const char *fixed[] = {"first", "second", "third", "fourth", "fifth"};
  for (unsigned int i = 0; i < 5; i++)
    if (!strcmp(week, fixed[i]))
      return 1 + i * 7;
  if (!strcmp(week, "teenth"))
    return 13;
  if (!strcmp(week, "last"))
    return last_day_of_month(year, month) - 6;
  return (unsigned int)-1;
}

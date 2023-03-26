#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include "grade_school.h"

static bool insert_student(roster_t *roster, const char *name, int grade);

void init_roster(roster_t *roster) {
  roster->count = 0;
}

bool add_student(roster_t *roster, const char *name, int grade) {
  return insert_student(roster, name, grade);
}

roster_t get_grade(roster_t *roster, int desired_grade) {
  roster_t grade_roster;
  init_roster(&grade_roster);
  for (size_t k = 0; k < roster->count; k++) {
    if ((roster->students[k]).grade == desired_grade) {
      memcpy( &grade_roster.students[grade_roster.count++]
             , &roster->students[k]
              , sizeof(student_t)
      );
    }
  }
  return grade_roster;
}


typedef struct {
  size_t pos;
  bool found;
} find_result_t;

static find_result_t
find_for_insert(roster_t *roster, const char *name, int grade) {
  if (roster->count == 0) {
    return (find_result_t){0, false};
  }
  
  size_t j = MAX_STUDENTS, k = 0;

  // smaller grades
  for (; k < roster->count && roster->students[k].grade < grade; k++) {
    if (0 == strncmp(roster->students[k].name, name, MAX_NAME_LENGTH))
      return (find_result_t){k, true};
  }
  // equal grades
  for (; k < roster->count && roster->students[k].grade == grade; k++) {
    int cmp = strncmp(roster->students[k].name, name, MAX_NAME_LENGTH);
    if (cmp > 0)
      break;
    else if (cmp == 0)
      return (find_result_t){k, true};
  }
  j = k;

  // remaining equal grades, and higher
  for (; k < roster->count ; k++) {
    if (0 == strncmp(roster->students[k].name, name, MAX_NAME_LENGTH))
      return (find_result_t){k, true};
  }
  return (find_result_t){j, false};
}

static bool insert_student(roster_t *roster, const char *name, int grade) {
  if (roster->count >= MAX_STUDENTS) return false;
  find_result_t result = find_for_insert(roster, name, grade);
  if (result.found) {
    return false;
  }
  for (size_t k = roster->count; k > result.pos; k--) {
    memcpy(&roster->students[k], &roster->students[k-1], sizeof(student_t));
  }
  roster->students[result.pos].grade = grade;
  strlcpy(roster->students[result.pos].name, name, MAX_NAME_LENGTH);
  roster->count++;
  return true;
}

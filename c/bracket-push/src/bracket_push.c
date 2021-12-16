#include "bracket_push.h"
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

static bool isbracket(int c) {
  static const char *brackets = "{[()]}";
  for (const char *p = brackets; *p ; p++)
    if (*p == c)
      return true;
  return false;
}

static inline void push(char **sp, char c) {
  *(*sp)++ = c;
}

static inline char pop(char **sp) {
  return *--(*sp);
}

static bool are_brackets_matching(char start, char end) {
  switch (start) {
  case '(':
    return end == ')';
  case '[':
    return end == ']';
  case '{':
    return end == '}';
  default:
    return false;
  }
}

bool is_paired(const char *expression) {
  char stack[strlen(expression)];
  stack[strlen(expression)] = '\0';
  char *sp = stack;
  const char *p;
  for (p = expression; *p != '\0'; p++)
    if (!isbracket(*p))
      continue;
    else if (*p == '(' || *p == '[' || *p == '{') {
      push(&sp, *p);
    }
    else if (*p == ')' || *p == ']' || *p == '}') {
      char c;
      if (sp == stack)
        return false;
      c = pop(&sp);
      if (!are_brackets_matching(c, *p))
        return false;
    }
    else
      return false;
  return sp == stack;
}


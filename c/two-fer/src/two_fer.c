#include "two_fer.h"
#include <stdio.h>

void two_fer(char *response, const char *who) {
  snprintf(response, RESPONSE_SIZE, "One for %s, one for me.", who ? who : "you");
}

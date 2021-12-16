#include "gigasecond.h"
#include <stdlib.h>
#include <time.h>


time_t gigasecond_after(time_t birthdate) {
  return birthdate + 1000000000;
}

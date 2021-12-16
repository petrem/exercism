#include "space_age.h"

double PLANET_AGES[] = {0.2408467, 0.61519726, 1.0, 1.8808158, 11.862615, 29.447498, 84.016846, 164.79132};
double EARTH_YEAR = 31557600.0;


double convert_planet_age(enum PLANETS planet, long age_in_seconds) {
  return age_in_seconds / EARTH_YEAR / PLANET_AGES[planet];
}

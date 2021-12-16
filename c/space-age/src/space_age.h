#ifndef _SPACE_AGE_H
#define _SPACE_AGE_H

enum  PLANETS {MERCURY, VENUS, EARTH, MARS, JUPITER, SATURN, URANUS, NEPTUNE};
extern double PLANET_AGES[];
extern double EARTH_YEAR;

double convert_planet_age(enum PLANETS, long);

#endif

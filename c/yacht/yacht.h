#ifndef YACHT_H
#define YACHT_H

typedef int _counts[7];

typedef int (*category_t)(_counts);
int ONES(_counts);
int TWOS(_counts);
int THREES(_counts);
int FOURS(_counts);
int FIVES(_counts);
int SIXES(_counts);
int FULL_HOUSE(_counts);
int FOUR_OF_A_KIND(_counts);
int LITTLE_STRAIGHT(_counts);
int BIG_STRAIGHT(_counts);
int CHOICE(_counts);
int YACHT(_counts);


typedef struct {
   int faces[5];
} dice_t;

int score(dice_t dice, category_t category);

#endif

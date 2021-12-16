#include "difference_of_squares.h"
#include <stdio.h>

/* Original versions, where I was lazy to look up or come up with a formula:

long square_of_sum(long n) {
  long sum = 0;
  for (; n>0; n--) {
    sum += n;
  }
  return sum * sum;
}

long sum_of_squares(long n) {
  long sum = 0;
  for (; n>0; n--) {
    sum += n*n;
  }
  return sum;
}
*/

long square_of_sum(long n) {
  long sum = n * (n + 1) / 2;
  return sum * sum;
}

long sum_of_squares(long n) {
  return n * (n + 1) * (2 * n + 1) / 6;
}

/* The following uses a self-concocted formula. In the current, O(1) versions
 * of the functions above, it would be faster to implement it as
 * return square_of_sum(n) - sum_of_squares(n);
 *
 * This is easy to calculate:
 * (1+2+3)*(1+2+3) - 1*1 - 2*2 - 3*3 =
 * 1*1 + 1*2 + 1*3 + 2*1 + 2*2 + 2*3 + 3*1 + 3*2 + 3*3 - 1*1 - 2*2 - 3*3 =
 * 1*2 + 1*3 + 2*1 +  2*3 + 3*1 + 3*2 = (1*3 + 2*3 + 3*1 + 3*2) + (1*2 + 2*1)
 * 2 * [3 * (1+2) + 2 * (1)]
 * So half of it is the sum of n*(sum 1..n-1). It is easier to see if you do the above
 * for higher n, e.g. 4 instead of 3.
 */

long difference_of_squares(long n) {
  long sum = 0;
  long half_diff = 0;
  for (long i = 1; i < n; i++) {
    sum += i;
    half_diff += sum * (i+1);
  }
  return 2 * half_diff;
}

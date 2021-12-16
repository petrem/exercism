#include "complex_numbers.h"
#include <math.h>

static inline complex_t make_complex(double real, double imag) {
  complex_t result  = {.real = real, .imag = imag};
  return result;
}

static inline double square(double x) {
  return x * x;
}

static inline complex_t c_mul_scalar(complex_t x, double m) {
  return make_complex(x.real * m, x.imag * m);
}

complex_t c_add(complex_t a, complex_t b)
{
  return make_complex(a.real + b.real, a.imag + b.imag);
}

complex_t c_sub(complex_t a, complex_t b)
{
  return make_complex(a.real - b.real, a.imag - b.imag);
}

complex_t c_mul(complex_t a, complex_t b)
{
  double real, imag;
  real = a.real * b.real - a.imag * b.imag;
  imag = a.imag * b.real + a.real * b.imag;
  return make_complex(real, imag);
}

complex_t c_div(complex_t a, complex_t b)
{
  double real, imag, denominator;
  denominator = square(b.real) + square(b.imag);
  real = (a.real * b.real + a.imag * b.imag) / denominator;
  imag = (a.imag * b.real - a.real * b.imag) / denominator;
  return make_complex(real, imag);
}

double c_abs(complex_t x)
{
  return sqrt(square(x.real) + square(x.imag));
}

complex_t c_conjugate(complex_t x)
{
  return make_complex(x.real, -x.imag);
}

double c_real(complex_t x)
{
  return x.real;
}

double c_imag(complex_t x)
{
  return x.imag;
}

complex_t c_exp(complex_t x)
{
  return c_mul_scalar(make_complex(cos(x.imag), sin(x.imag)), exp(x.real));
}

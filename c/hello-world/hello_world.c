#include "hello_world.h"

const char *hello(void)
{
  static const char *hello_world = "Hello, World!";
  return hello_world;
}

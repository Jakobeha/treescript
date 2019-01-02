#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "helpers.h"

bool strings_equal(const char* str1, const char* str2) {
  return strcmp(str1, str2) == 0;
}

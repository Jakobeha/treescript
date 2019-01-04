#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "helpers.h"

void* malloc0(size_t size) {
  if (size == 0) {
    return NULL;
  } else {
    return malloc(size);
  }
}
bool strings_equal(const char* str1, const char* str2) {
  return strcmp(str1, str2) == 0;
}

#ifndef HELPERS_H
#define HELPERS_H

#include <stdlib.h>

//allows 0-byte allocations
void* malloc0(size_t size);
bool strings_equal(const char *str1, const char *str2);

#endif

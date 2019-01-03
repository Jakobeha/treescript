#ifndef REDUCE_H
#define REDUCE_H

#include <stdbool.h>
#include "types.h"

bool reduce_props(value_record* x);
bool reduce_children(value* x);
bool reduce_surface(value* x);
bool reduce(value* x);

#endif

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "helpers.h"
#include "misc.h"
#include "reduce.h"

bool reduce_props(value_record* x) {
  value_record in = *x;
  for (int i = 0; i < in.num_props; i++) {
    if (reduce(&in.props[i])) {
      return true;
    }
  }
  return false;
}

bool reduce_children(value* x) {
  if (x->type == RECORD) {
    return reduce_props(&x->as_record);
  } else {
    return false;
  }
}

bool reduce_surface(value* x) {
  // \reduce_surface
}

bool reduce(value* x) {
  return reduce_children(x) || reduce_surface(x);
}

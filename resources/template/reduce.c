#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "helpers.h"
#include "misc.h"
#include "reduce.h"

bool reduce_nested(surface_reducer reduce_surface, const match_arr in_matches, value* x) {
  value in = *x;
  if (in.type == RECORD) {
    value_record in_record = in.as_record;
    if (strings_equal(in_record.head, "E")) {
      *x = in_record.props[0];
      free(in_record.props);
      return reduce_aux(reduce_surface, REDUCE_STANDARD, in_matches, x);
    } else {
      bool reduced = false;
      for (int i = 0; i < in_record.num_props; i++) {
        if (reduce_nested(reduce_surface, in_matches, &in_record.props[i])) {
          reduced = true;
        }
      }
      return reduced;
    }
  } else {
    return false;
  }
}

bool reduce_children(surface_reducer reduce_surface, const match_arr in_matches, value* x) {
  value* in_props = malloc(sizeof(value) * 1);
  in_props[0] = *x;
  value in = {
    .type = RECORD,
    .as_record = (value_record){
      .head = "E",
      .num_props = 1,
      .props = in_props
    }
  };

  if (reduce_surface(REDUCE_EVALCTX, in_matches, &in)) {
    *x = in;
    return true;
  } else {
    return false;
  }
}

// \reduce_extras

bool reduce_main(reduce_type type, const match_arr in_matches, value* x) {
  // \reduce_main
}

bool reduce_aux(surface_reducer reduce_surface, reduce_type type, const match_arr in_matches, value* x) {
  return
    reduce_surface(type, in_matches, x) ||
    (type == REDUCE_STANDARD && reduce_children(reduce_surface, in_matches, x));
}

void reduce(value* x) {
  match_arr matches;
  for (int i = 0; i < MAX_NUM_BINDS; i++) {
    matches[i].is_set = false;
  }
  while (reduce_aux(reduce_main, REDUCE_STANDARD, matches, x)) { ; }
}

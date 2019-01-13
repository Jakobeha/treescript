#ifndef REDUCE_H
#define REDUCE_H

#include <stdbool.h>
#include "types.h"

#define MAX_NUM_BINDS 1  // \max_num_binds

typedef enum {
  REDUCE_STANDARD,
  REDUCE_EVALCTX
} reduce_type;

typedef bool (*surface_reducer)(reduce_type type, const match_arr in_matches, value* x);

bool reduce_nested(surface_reducer reduce_surface, const match_arr in_matches, value* x);
bool reduce_children(surface_reducer reduce_surface, const match_arr in_matches, value* x);
bool reduce_main(reduce_type type, const match_arr in_matches, value* x);
bool reduce_aux(surface_reducer reduce_surface, reduce_type type, const match_arr in_matches, value* x);
void reduce(value* x);

#endif

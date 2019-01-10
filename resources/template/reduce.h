#ifndef REDUCE_H
#define REDUCE_H

#include <stdbool.h>
#include "types.h"

bool reduce_nested(bool (*reduce_surface)(const match_arr in_matches, value* x), const match_arr in_matches, value* x);
bool reduce_children(bool (*reduce_surface)(const match_arr in_matches, value* x), const match_arr in_matches, value* x);
bool reduce_main(const match_arr in_matches, value* x);
bool reduce_aux(bool (*reduce_surface)(const match_arr in_matches, value* x), const match_arr in_matches, value* x);
void reduce(value* x);

#endif

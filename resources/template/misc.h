#ifndef MISC_H
#define MISC_H

#include <stdbool.h>
#include "types.h"

bool records_equal(value_record x, value_record y);
bool values_equal(value x, value y);
value_record dup_record(value_record x);
value dup_value(value x);
void free_record(value_record x);
void free_value(value x);
value bool_to_value(bool x);
int get_record_num_props(char *head);

#endif

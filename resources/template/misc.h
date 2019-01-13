#ifndef MISC_H
#define MISC_H

#include <stdbool.h>
#include <stdlib.h>
#include "types.h"

#define EMPTY_RECORD_VALUE(Head)\
(value){\
  .type = RECORD,\
  {\
    .as_record = (value_record){\
      .head = Head,\
      .num_props = 0,\
      .props = NULL\
    }\
  }\
}

static const value UNIT_VALUE = EMPTY_RECORD_VALUE("Unit");
static const value TRUE_VALUE = EMPTY_RECORD_VALUE("True");
static const value FALSE_VALUE = EMPTY_RECORD_VALUE("False");
static const value NIL_VALUE = EMPTY_RECORD_VALUE("Nil");
static const value HOLE_VALUE = EMPTY_RECORD_VALUE("Hole");

bool records_equal(value_record x, value_record y);
bool values_equal(value x, value y);
value_record dup_record(value_record x);
value dup_value(value x);
void free_record(value_record x);
void free_value(value x);
value bool_to_value(bool x);
value cons_value(value first, value rest);
value subst_value(value x, value old, value rep);
int get_record_num_props(char* head);

#endif

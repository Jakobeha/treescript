#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "helpers.h"
#include "misc.h"

int get_record_num_props(char* head) {
  // \get_record_num_props
}

bool records_equal(value_record x, value_record y) {
  if (!strings_equal(x.head, y.head)) {
    return false;
  }

  assert(x.num_props == y.num_props);
  for (int i = 0; i < x.num_props; i++) { //== y.num_props
    if (!values_equal(x.props[i], y.props[i])) {
      return false;
    }
  }

  return true;
}

bool values_equal(value x, value y) {
  if (x.type != y.type) {
    return false;
  }

  switch (x.type) { //== y.type
  case SPLICE:
    return x.as_splice == y.as_splice;
  case PRIM_INTEGER:
    return x.as_integer == y.as_integer;
  case PRIM_FLOAT:
    return x.as_float == y.as_float;
  case PRIM_STRING:
    return strings_equal(x.as_string, y.as_string);
  case RECORD:
    return records_equal(x.as_record, y.as_record);
  }
}

value_record dup_record(value_record x) {
  value* props = malloc0(sizeof(value) * x.num_props);
  for (int i = 0; i < x.num_props; i++) {
    props[i] = dup_value(x.props[i]);
  }
  return (value_record){
    .head = x.head,
    .num_props = x.num_props,
    .props = props
  };
}

value dup_value(value x) {
  switch (x.type) {
  case SPLICE:
  case PRIM_INTEGER:
  case PRIM_FLOAT:
    return x;
  case PRIM_STRING:
    return (value){
      .type = PRIM_STRING,
      .as_string = strdup(x.as_string)
    };
  case RECORD:
    return (value){
      .type = RECORD,
      .as_record = dup_record(x.as_record)
    };
  }
}

void free_record(value_record x) {
  if (x.num_props > 0) {
    for (int i = 0; i < x.num_props; i++) {
      free_value(x.props[i]);
    }
    free(x.props);
  }
}

void free_value(value x) {
  switch (x.type) {
    case SPLICE:
    case PRIM_INTEGER:
    case PRIM_FLOAT:
      break;
    case PRIM_STRING:
      free(x.as_string);
      break;
    case RECORD:
      free_record(x.as_record);
      break;
  }
}
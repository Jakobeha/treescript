#include "interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../misc.h"

#define CALL_BASE_ARITH(Name, op)                                     \
  value call_Base_##Name(value* props) {                              \
    value lhs = props[0];                                             \
    value rhs = props[1];                                             \
    if (lhs.type == PRIM_INTEGER || rhs.type == PRIM_INTEGER) {       \
      return (value){.type = PRIM_INTEGER,                            \
                     .as_integer = lhs.as_integer op rhs.as_integer}; \
    } else if (lhs.type == PRIM_INTEGER || rhs.type == PRIM_FLOAT) {  \
      return (value){.type = PRIM_FLOAT,                              \
                     .as_float = lhs.as_integer op rhs.as_float};     \
    } else if (lhs.type == PRIM_FLOAT || rhs.type == PRIM_INTEGER) {  \
      return (value){.type = PRIM_FLOAT,                              \
                     .as_float = lhs.as_float op rhs.as_integer};     \
    } else if (lhs.type == PRIM_FLOAT || rhs.type == PRIM_FLOAT) {    \
      return (value){.type = PRIM_FLOAT,                              \
                     .as_float = lhs.as_float op rhs.as_float};       \
    } else {                                                          \
      fprintf(stderr, "invalid types passed to function '" #Name      \
                      " - expected numbers'\n");                      \
      exit(1);                                                        \
    }                                                                 \
  }

#define CALL_BASE_NUM_COMPARE(Name, op)                              \
  value call_Base_##Name(value* props) {                             \
    value lhs = props[0];                                            \
    value rhs = props[1];                                            \
    if (lhs.type == PRIM_INTEGER || rhs.type == PRIM_INTEGER) {      \
      return bool_to_value(lhs.as_integer op rhs.as_integer);        \
    } else if (lhs.type == PRIM_INTEGER || rhs.type == PRIM_FLOAT) { \
      return bool_to_value(lhs.as_integer op rhs.as_float);          \
    } else if (lhs.type == PRIM_FLOAT || rhs.type == PRIM_INTEGER) { \
      return bool_to_value(lhs.as_float op rhs.as_integer);          \
    } else if (lhs.type == PRIM_FLOAT || rhs.type == PRIM_FLOAT) {   \
      return bool_to_value(lhs.as_float op rhs.as_float);            \
    } else {                                                         \
      fprintf(stderr, "invalid types passed to function '" #Name     \
                      "' - expected numbers\n");                     \
      exit(1);                                                       \
    }                                                                \
  }

value setup_Base() {}

value teardown_Base() {}

value call_Base_IsEqual(value* props) {
  value lhs = props[0];
  value rhs = props[1];
  return bool_to_value(values_equal(lhs, rhs));
}

value call_Base_Map(value* props) {
  value func = props[0];
  value xs = props[1];
  if (xs.type == RECORD) {
    value_record xs_rec = xs.as_record;
    if (strings_equal(xs_rec.head, "Nil")) {
      return NIL_VALUE;
    } else if (strings_equal(xs_rec.head, "Cons")) {
      value y = subst_value(func, HOLE_VALUE, xs.props[0]);
      value ys = call_Base_Map((value[]){func, xs.props[1]});
      return cons_value(y, ys);
    }
  }

  fprintf(stderr,
          "invalid second arg passed to function 'Map' - expected list\n");
  exit(1);
}

CALL_BASE_ARITH(Add, +)
CALL_BASE_ARITH(Subtract, -)
CALL_BASE_ARITH(Multiply, *)
CALL_BASE_ARITH(Divide, /)
CALL_BASE_NUM_COMPARE(GreaterThan, >)
CALL_BASE_NUM_COMPARE(LessThan, <)

value call_Base_Append(value* props) {
  value lhs = props[0];
  value rhs = props[1];
  if (lhs.type == PRIM_STRING || rhs.type == PRIM_STRING) {
    const char* lhs_str = lhs.as_string;
    const char* rhs_str = rhs.as_string;
    char* res_str =
        malloc((1 + strlen(lhs_str) + strlen(rhs_str)) * sizeof(char));
    strcpy(res_str, lhs_str);
    strcat(res_str, rhs_str);
    return (value){.type = PRIM_STRING, .as_string = res_str};
  } else {
    fprintf(stderr,
            "invalid types passed to function 'Append' - expected strings\n");
    exit(1);
  }
}

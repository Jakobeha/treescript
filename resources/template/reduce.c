#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "reduce.h"

bool reduce(value* x) {
  /*
  Example:
  value in = *x;
  bool set_matches[4];
  value matches[4];
  if (in.type == PRIM_STRING && strings_equal(in.as_string, "foo")) {
    //produce
    *x = (value){
      .type = PRIM_INTEGER,
      .as_integer = 42
    };
    return true;
  }
  if (in.type == RECORD && strings_equal(in.as_record.head, "Bar")) {
    value* in_props = in.as_record.props;
    value in_0 = in_props[0];
    if (in_0.type == PRIM_FLOAT && in_0.as_float == 2.5) {
      value in_1 = in_props[1];
      if (!set_matches[0] || values_equal(matches[0], in_1)) {
        set_matches[0] = true;
        matches[0] = in_1;
        //produce
        value* out_props = malloc(sizeof(value) * 1);
        out_props[0] = (value){
          .type = PRIM_STRING,
          .as_string = strdup("Hello world!")
        };
        *x = (value){
          .type = RECORD,
          .as_record = (value_record){
            .head = strdup("Foo"),
            .num_props = 1,
            .props = out_props
          }
        };
        return true;
      }
    }
  }
  return false;
  */

  // \reduce
}

#ifndef TYPES_H
#define TYPES_H

typedef enum {
  SPLICE,
  PRIM_INTEGER,
  PRIM_FLOAT,
  PRIM_STRING,
  RECORD
} value_type;

typedef struct {
  char* head;
  int num_props;
  value* props;
} value_record;

typedef struct {
  value_type type;
  union {
    int as_splice;
    int as_integer;
    float as_float;
    char* as_string;
    value_record as_record;
  };
} value;

#endif

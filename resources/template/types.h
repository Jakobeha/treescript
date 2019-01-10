#ifndef TYPES_H
#define TYPES_H

#define MAX_NUM_BINDS 1 // \max_num_binds

struct value;

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
  struct value* props;
} value_record;

typedef struct value {
  value_type type;
  union {
    int as_splice;
    int as_integer;
    float as_float;
    char* as_string;
    value_record as_record;
  };
} value;

typedef struct {
  bool is_set;
  value value;
} match;

typedef match match_arr[MAX_NUM_BINDS];

#endif

#include <stdbool.h>

// = Value Types

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

// = Value Helpers
int get_record_num_props(char* head);
// = Value Parsing
bool streq(const char* str1, const char* str2);
char* scan_word();
int get_record_num_props(char* head);
int scan_integer();
float scan_float();
char* scan_string();
value_record scan_record(char* head);
value scan_value(char* word);
// = Value Printing
void print_string(const char* x);
void print_record(value_record x);
void print_value(value x);
// = Value Reduction
bool reduce(value* x);
// = Value Freeing
void free_record(value_record x);
void free_value(value x);
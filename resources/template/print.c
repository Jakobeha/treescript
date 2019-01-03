#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "print.h"

void print_string(const char* x) {
  putchar('"');
  int i = 0;
  char next;
  while ((next = x[i]) != '\0') {
    if (next == '\n') {
      printf("\\n");
    } else if (next == '"') {
      printf("\\\"");
    } else if (next == '\\') {
      printf("\\\\");
    } else {
      putchar(next);
    }
    i++;
  }
  putchar('"');
  putchar(' ');
}

void print_record(value_record x) {
  printf("%s ", x.head);
  for (int i = 0; i < x.num_props; i++) {
    print_value(x.props[i]);
  }
}

void print_value(value x) {
  switch (x.type) {
    case SPLICE:
      printf("splice %d ", x.as_splice);
      break;
    case PRIM_INTEGER:
      printf("integer %d ", x.as_integer);
      break;
    case PRIM_FLOAT:
      printf("float %f ", x.as_float);
      break;
    case PRIM_STRING:
      printf("string ");
      print_string(x.as_string);
      break;
    case RECORD:
      print_record(x.as_record);
      break;
  }
}

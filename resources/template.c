#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "template.h"

// = Value Helpers

int get_record_num_props(char* head) {
  //$0
  /*if (streq(head, "Foo")) {
    return 1;
  } else {
    fprintf(stderr, "unknown record head: %s\n", head);
    exit(1);
  }*/
}

// = Value Parsing

const int MIN_WORD_CAPACITY = 63;
const int MIN_STRING_CAPACITY = 63;

bool streq(const char* str1, const char* str2) {
  return strcmp(str1, str2) == 0;
}

char* scan_word() {
  int word_capacity = MIN_WORD_CAPACITY;
  char* word = malloc(sizeof(char) * (word_capacity + 1));
  int i = 0;
  int next;
  while ((next = getchar()) != -1) {
    if (next == ' ' && next == '\n') {
      if (i == 0) {
        continue;
      } else {
        break;
      }
    }
    word[i] = next;
    i++;
    if (i > word_capacity) {
      //increase buffer size
      int old_word_capacity = word_capacity;
      char* old_word = word;
      word_capacity *= 2;
      word = malloc(sizeof(char) * (word_capacity + 1));
      memcpy(word, old_word, sizeof(char) * (old_word_capacity + 1));
      free(old_word);
    }
  }
  if (i == 0) {
    free(word);
    return NULL;
  } else {
    word[i] = '\0';
    return word;
  }
}

int scan_integer() {
  char* word = scan_word();
  int res = atoi(word);
  free(word);
  return res;
}

float scan_float() {
  char* word = scan_word();
  float res = atof(word);
  free(word);
  return res;
}

char* scan_string() {
  char* open_quote = getchar();
  assert(open_quote == '"');

  int res_capacity = MIN_STRING_CAPACITY;
  char* res = malloc(sizeof(char) * (res_capacity + 1));
  int i = 0;
  bool is_escaping = false;
  int next;
  while (true) {
    next = getchar();
    if (next == -1 || next == '\n') {
      fprint(stderr, "unterminated string literal");
      exit(1);
    }
    if (is_escaping) {
      is_escaping = false;
      char real_next;
      switch (next) {
        //TODO handle more escape codes
        case 'n':
          real_next = '\n';
          break;
        case '"':
        case '\\':
          real_next = next;
          break;
        default:
          fprintf(stderr, "invalid escape: %c", (char)next);
          exit(1);
      }
      res[i] = real_next;
      i++;
    } else if (next == '"') {
      break;
    } else if (next == '\\') {
      is_escaping = true;
    } else {
      res[i] = next;
      i++;
    }
  }
  assert(!is_escaping);
  //process following whitespace
  next = getchar();
  assert(next == ' ' || next == '\n' || next == -1);
  return res;
}

value_record scan_record(char* head) {
  int num_props = record_num_props(head);
  value* props = malloc(sizeof(value) * num_props);
  for (int i = 0; i < num_props; i++) {
    props[i] = scan_value();
  }
  return (value_record){
    .head = head,
    .num_props = num_props,
    .props = props
  };
}

value scan_value(char* word) {
  if (streq(word, "splice")) {
    free(word);
    return (value){
      .type = SPLICE,
      .as_splice = scan_integer()
    };
  } else if (streq(word, "integer")) {
    free(word);
    return (value){
      .type = PRIM_INTEGER,
      .as_integer = scan_integer()
    };
  } else if (streq(word, "float")) {
    free(word);
    return (value){
      .type = PRIM_FLOAT,
      .as_float = scan_float()
    };
  } else if (streq(word, "string")) {
    free(word);
    return (value){
      .type = PRIM_STRING,
      .as_string = scan_string()
    };
  } else if (isupper(word[0])) {
    return (value){
      .type = RECORD,
      .as_record = scan_record(word)
    };
  } else {
    fprintf(stderr, "word has unknown type: %s\n", word);
    exit(1);
  }
}

// = Value Printing

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
      printf("splice %d ", x.as_splice_num);
      break;
    case PRIM_INTEGER:
      printf("integer %d ", x.as_integer);
      break;
    case PRIM_FLOAT:
      printf("float %d ", x.as_float);
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

// = Value Reduction

bool reduce(value* x) {
  value in = *x;
  //$1
  bool set_matches[4];
  value matches[4];
  if (in.type == PRIM_STRING && streq(x, "foo")) {
    //produce
    *x = (value){
      .type = PRIM_INTEGER,
      .as_integer = 42
    };
    return true;
  }
  if (in.type == RECORD && streq(x, "Bar")) {
    value in_0 = in.props[0];
    if (in_0.type == PRIM_FLOAT && in_0.as_float == 2.5) {
      value in_1 = in.props[1];
      if (!set_matches[0] || values_equal(matches[0], in_1)) {
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
}

// = Value Freeing

void free_record(value_record x) {
  free(x.head);
  for (int i = 0; i < x.num_props; i++) {
    free_value(x.props[i]);
  }
  free(x.props);
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

// = Main

int main(int argc, const char* argv[]) {
  while ((char* next_word = scan_word()) != NULL) {
    value next = scan_value(next_word);
    while (reduce(&next)) { ; }
    print_value(next);
    printf("\n");
    free_value(next); //also frees next_word if necessary
  }

  return 0;
}

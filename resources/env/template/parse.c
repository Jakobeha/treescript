#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "helpers.h"
#include "misc.h"
#include "parse.h"

const int MIN_WORD_CAPACITY = 63;
const int MIN_STRING_CAPACITY = 63;

char* scan_word() {
  int word_capacity = MIN_WORD_CAPACITY;
  char* word = malloc(sizeof(char) * (word_capacity + 1));
  int i = 0;
  int next;
  while ((next = getchar()) != -1) {
    if (next == ' ' || next == '\n') {
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
  int open_quote = getchar();
  assert(open_quote == '"');

  int res_capacity = MIN_STRING_CAPACITY;
  char* res = malloc(sizeof(char) * (res_capacity + 1));
  int i = 0;
  bool is_escaping = false;
  int next;
  while (true) {
    next = getchar();
    if (next == -1 || next == '\n') {
      fprintf(stderr, "unterminated string literal");
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
    if (i > res_capacity) {
      //increase buffer size
      int old_res_capacity = res_capacity;
      char* old_res = res;
      res_capacity *= 2;
      res = malloc(sizeof(char) * (res_capacity + 1));
      memcpy(res, old_res, sizeof(char) * (old_res_capacity + 1));
      free(old_res);
    }
  }
  res[i] = '\0';
  assert(!is_escaping);
  //process following whitespace
  next = getchar();
  assert(next == ' ' || next == '\n' || next == -1);
  return res;
}

value_record scan_record(char* head) {
  int num_props = get_record_num_props(head);
  value* props = malloc0(sizeof(value) * num_props);
  for (int i = 0; i < num_props; i++) {
    char* word = scan_word();
    assert(word != NULL);
    props[i] = scan_value(word);
  }
  return (value_record){
    .head = head,
    .num_props = num_props,
    .props = props
  };
}

value scan_value(char* word) {
  if (strings_equal(word, "splice")) {
    free(word);
    return (value){
      .type = SPLICE,
      .as_splice = scan_integer()
    };
  } else if (strings_equal(word, "integer")) {
    free(word);
    return (value){
      .type = PRIM_INTEGER,
      .as_integer = scan_integer()
    };
  } else if (strings_equal(word, "float")) {
    free(word);
    return (value){
      .type = PRIM_FLOAT,
      .as_float = scan_float()
    };
  } else if (strings_equal(word, "string")) {
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

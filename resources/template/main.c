#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "misc.h"
#include "parse.h"
#include "print.h"
#include "reduce.h"

int main(int argc, const char* argv[]) {
  char* next_word;
  while ((next_word = scan_word()) != NULL) {
    value next = scan_value(next_word);
    while (reduce(&next)) { ; }
    print_value(next);
    printf("\n");
    free_value(next); //also frees next_word if necessary
  }

  return 0;
}

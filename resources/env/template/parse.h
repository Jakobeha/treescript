#ifndef PARSE_H
#define PARSE_H

#include "types.h"

char* scan_word();
int scan_integer();
float scan_float();
char* scan_string();
value_record scan_record(char* head);
value scan_value(char* word);

#endif

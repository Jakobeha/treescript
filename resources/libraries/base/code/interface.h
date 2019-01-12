#ifndef BASE_INTERFACE_H
#define BASE_INTERFACE_H

#include "../types.h"

value call_Base_Add(value* props);
value call_Base_Subtract(value* props);
value call_Base_Multiply(value* props);
value call_Base_Divide(value* props);
value call_Base_LessThan(value* props);
value call_Base_Append(value *props);

#endif

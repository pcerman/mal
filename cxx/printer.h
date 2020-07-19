#pragma once

#include "types.h"

namespace MAL
{

std::string pr_str(Value * val, bool readably);

#ifdef _DEBUG

void print(Value* val);

#endif

}

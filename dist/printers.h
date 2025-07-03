#pragma once

#include "pretty.h"

struct doc* doc_of_quoted(struct arena* st, const char* s);
struct doc* doc_of_int(struct arena* st, int i);

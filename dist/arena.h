#pragma once

#include <stddef.h>

struct arena {
  char* buffer;
  size_t len;
  size_t pos;
};

void arena_init(struct arena* a);
void* arena_alloc(struct arena* a, size_t len);
void arena_destroy(struct arena* a);

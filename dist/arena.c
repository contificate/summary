#include "arena.h"

#include <stdlib.h>
#include <sys/mman.h>

/* TODO: make this less of a joke */
void arena_init(struct arena* st) {
  st->buffer = mmap(NULL, 1024 * 1024, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  st->pos = 0;
}

void* arena_alloc(struct arena* st, size_t len) {
  const size_t a = sizeof(void*);
  len = (len + a - 1) & ~(a - 1);
  void* pos = st->buffer + st->pos;
  st->pos += len;
  return pos;
}

void arena_destroy(struct arena* st) {
  munmap(st->buffer, 1024 * 1024);
}

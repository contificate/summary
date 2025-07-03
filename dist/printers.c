#include "printers.h"
#include <stdio.h>

struct doc* doc_of_foo(struct arena* st, char value) {
  char buffer[] = { '\'', '?', '\'', '\0' };
  buffer[1] = value;
  return text(buffer);
}

struct doc* doc_of_quoted(struct arena* st, const char* s) {
  struct doc* q = text("\"");
  return cons(q, cons(text(s), q));
}

struct doc* doc_of_int(struct arena* st, int i) {
  char* b;
  asprintf(&b, "%d", i);
  struct doc* d = text(b);
  free(b);
  return d;
}

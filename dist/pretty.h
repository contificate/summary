/* A simple transliteration of "Strictly Pretty" by Christian Lindig into C. */
/* https://lindig.github.io/papers/strictly-pretty-2000.pdf */

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "arena.h"

struct doc {
  enum {
    DOC_NIL,
    DOC_CONS,
    DOC_TEXT,
    DOC_NEST,
    DOC_BR,
    DOC_GROUP,
  } type;
  union {
    struct {
      struct doc* left;
      struct doc* right;
    } cons;
    char* text;
    struct {
      unsigned indent;
      struct doc* doc;
    } nest;
    struct {
      char* sep;
    } br;
    struct doc* group;
  } as;
};

struct sdoc {
  enum {
    SDOC_TEXT,
    SDOC_LINE,
  } type;
  union {
    char* text;
    struct {
      unsigned indent;
    } line;
  } as;
  struct sdoc* next;
};

enum mode {
  MODE_FLAT,
  MODE_BREAK,
};

struct part {
  unsigned indent;
  enum mode mode;
  struct doc* doc;
  struct part* next;
};

struct sdoc* alloc_sdoc(struct arena* st, int type);

struct sdoc* sdoc_text(struct arena* st, const char* text, struct sdoc* next);

struct sdoc* sdoc_line(struct arena* st, unsigned indent, struct sdoc* next);

struct doc* alloc_doc(struct arena* st, int type);

struct doc* doc_nil(struct arena* st);

struct doc* doc_cons(struct arena* st, struct doc* left, struct doc* right);

struct doc* doc_text(struct arena* st, const char* text);

struct doc* doc_nest(struct arena* st, unsigned indent, struct doc* doc);

struct doc* doc_br(struct arena* st, const char* sep);

struct doc* doc_group(struct arena* st, struct doc* grouped);

struct part* part(struct arena* st, unsigned indent, enum mode mode,
                  struct doc* doc, struct part* next);

bool fits(int width, struct part* ps);

struct sdoc* format(struct arena* st, int width, struct doc* doc);

void print_sdoc(struct sdoc* s);

void print_sdoc_to_file(FILE* f, struct sdoc* s);

char* string_of_sdoc(struct sdoc* s);

struct record_entry {
  const char* key;
  struct doc* value;
  struct record_entry* next;
};

struct record {
  const char* name;
  struct record_entry* entries;
};

struct record_entry* record_entry(struct arena* a, const char* key, struct doc* value, struct record_entry* next);

struct record* record(struct arena* a, const char* name, struct record_entry* entries);

struct doc* doc_of_record(struct arena* st, struct record* r);

#define nil() (doc_nil(st))
#define text(t) (doc_text(st, t))
#define cons(l, r) (doc_cons(st, l, r))
#define nest(i, d) (doc_nest(st, i, d))
#define br(s) (doc_br(st, s))
#define group(c) (doc_group(st, c))

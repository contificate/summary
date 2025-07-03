#include "pretty.h"
#include "arena.h"

struct sdoc* alloc_sdoc(struct arena* st, int type) {
  struct sdoc* d = arena_alloc(st, sizeof(struct sdoc));
  d->type = type;
  return d;
}

struct sdoc* sdoc_text(struct arena* st, const char* text, struct sdoc* next) {
  const size_t len = strlen(text) + 1;
  char* buffer = arena_alloc(st, len);
  memcpy(buffer, text, len);

  struct sdoc* d = alloc_sdoc(st, SDOC_TEXT);
  d->as.text = buffer;
  d->next = next;
  return d;
}

struct sdoc* sdoc_line(struct arena* st, unsigned indent, struct sdoc* next) {
  struct sdoc* d = alloc_sdoc(st, SDOC_LINE);
  d->as.line.indent = indent;
  d->next = next;
  return d;
}

struct doc* alloc_doc(struct arena* st, int type) {
  struct doc* d = arena_alloc(st, sizeof(struct doc));
  d->type = type;
  return d;
}

struct doc* doc_nil(struct arena* st) {
  return alloc_doc(st, DOC_NIL);
}

struct doc* doc_cons(struct arena* st, struct doc* left, struct doc* right) {
  struct doc* d = alloc_doc(st, DOC_CONS);
  d->as.cons.left = left;
  d->as.cons.right = right;
  return d;
}

struct doc* doc_text(struct arena* st, const char* text) {
  const size_t len = strlen(text) + 1;
  char* buffer = arena_alloc(st, len);
  memcpy(buffer, text, len);

  struct doc* d = alloc_doc(st, DOC_TEXT);
  d->as.text = buffer;
  return d;
}

struct doc* doc_nest(struct arena* st, unsigned indent, struct doc* doc) {
  struct doc* d = alloc_doc(st, DOC_NEST);
  d->as.nest.indent = indent;
  d->as.nest.doc = doc;
  return d;
}

struct doc* doc_br(struct arena* st, const char* sep) {
  const size_t len = strlen(sep) + 1;
  char* buffer = arena_alloc(st, len);
  memcpy(buffer, sep, len);

  struct doc* d = alloc_doc(st, DOC_BR);
  d->as.br.sep = buffer;
  return d;
}

struct doc* doc_group(struct arena* st, struct doc* grouped) {
  struct doc* d = alloc_doc(st, DOC_GROUP);
  d->as.group = grouped;
  return d;
}

struct part* part(struct arena* st, unsigned indent, enum mode mode,
                  struct doc* doc, struct part* next) {
  struct part* p = arena_alloc(st, sizeof(struct part));
  p->indent = indent;
  p->mode = mode;
  p->doc = doc;
  p->next = next;
  return p;
}

bool fits(int width, struct part* ps) {
  if (width <= 0)
    return false;

  if (!ps)
    return true;

  struct arena st;
  arena_init(&st);

  while (ps) {
    unsigned i = ps->indent;
    enum mode m = ps->mode;
    struct doc* d = ps->doc;
    struct part* next = ps->next;
    ps = next;

    if (width < 0)
      goto end;

    switch (d->type) {
      case DOC_NIL:
        /* (i,m,DocNil) :: z -> fits w z */
        continue;

      case DOC_CONS:
        /* (i,m,DocCons(x,y)) :: z -> fits w ((i,m,x)::(i,m,y)::z) */
        ps = part(&st, i, m, d->as.cons.left,
                  part(&st, i, m, d->as.cons.right, ps));
        continue;

      case DOC_NEST: {
        /* (i,m,DocNest(j,x)) :: z -> fits w ((i+j,m,x)::z) */
        const unsigned j = d->as.nest.indent;
        ps = part(&st, i + j, m, d->as.nest.doc, ps);
        continue;
      }

      case DOC_TEXT:
        /* (i,m,DocText(s)) :: z -> fits (w - strlen s) z */
        width -= strlen(d->as.text);
        continue;

      case DOC_BR: {
        /* (i,Break,DocBreak(_)) :: z -> true (* impossible *) */
        if (m == MODE_BREAK)
          goto end;

        /* (i,Flat, DocBreak(s)) :: z -> fits (w - strlen s) z */
        width -= strlen(d->as.br.sep);
        continue;
      }

      case DOC_GROUP:
        /* (i,m,DocGroup(x)) :: z -> fits w ((i,Flat,x)::z) */
        ps = part(&st, i, MODE_FLAT, d->as.group, ps);
        continue;
    }
  }

 end:
  arena_destroy(&st);

  return (width >= 0);
}

struct sdoc* format(struct arena* st, int width, struct doc* doc) {
  struct arena pieces;
  arena_init(&pieces);

  struct part* ps = part(&pieces, 0, MODE_FLAT, doc_group(&pieces, doc), NULL);

  unsigned used = 0;
  struct sdoc* acc = NULL;

  while (ps) {
    unsigned i = ps->indent;
    enum mode m = ps->mode;
    struct doc* d = ps->doc;
    struct part* next = ps->next;
    ps = next;

    switch (d->type) {
      case DOC_NIL:
        continue;

      case DOC_CONS:
        ps = part(&pieces, i, m, d->as.cons.right, ps);
        ps = part(&pieces, i, m, d->as.cons.left, ps);
        continue;

      case DOC_NEST:
        ps = part(&pieces, i + d->as.nest.indent, m, d->as.nest.doc, ps);
        continue;

      case DOC_TEXT:
        acc = sdoc_text(st, d->as.text, acc);
        used += strlen(d->as.text);
        continue;

      case DOC_BR: {
        switch (m) {
          case MODE_FLAT:
            /* (i,Flat, DocBreak(s)) :: z ->
                 SText(s,format w (k + strlen s) z) */
            acc = sdoc_text(st, d->as.br.sep, acc);
            used += strlen(d->as.br.sep);
            break;

          case MODE_BREAK:
            /* (i,Break,DocBreak(s)) :: z -> SLine(i,format w i z) */
            acc = sdoc_line(st, i, acc);
            used = i;
            break;
        }

        continue;
      }

      case DOC_GROUP: {
        /* (i,m,DocGroup(x)) :: z -> if fits (w-k) ((i,Flat,x)::z) */
        struct doc* group = d->as.group;
        if (fits(width - used, part(&pieces, i, MODE_FLAT, group, ps)))
          ps = part(&pieces, i, MODE_FLAT, group, ps);
        else
          ps = part(&pieces, i, MODE_BREAK, group, ps);

        continue;
      }
    }

  }

  struct sdoc* reversed = NULL;
  while (acc) {
    struct sdoc* next = acc->next;
    acc->next = reversed;
    reversed = acc;
    acc = next;
  }

  arena_destroy(&pieces);

  return reversed;
}

void print_sdoc(struct sdoc* s) {
  if (!s) {
    putchar('\n');
    return;
  }

  switch (s->type) {
    case SDOC_TEXT:
      printf("%s", s->as.text);
      break;

    case SDOC_LINE:
      putchar('\n');
      unsigned count = s->as.line.indent;
      while (count--)
        putchar(' ');
      break;
  }

  print_sdoc(s->next);
}

void print_sdoc_to_file(FILE* f, struct sdoc* s) {
  if (!s) {
    return;
  }

  switch (s->type) {
    case SDOC_TEXT:
      fprintf(f, "%s", s->as.text);
      break;

    case SDOC_LINE:
      fprintf(f, "\n");
      unsigned count = s->as.line.indent;
      while (count--)
        fputc(' ', f);
      break;
  }

  print_sdoc_to_file(f, s->next);
}

char* string_of_sdoc(struct sdoc* s) {
  char* buffer;
  size_t len;
  FILE* f = open_memstream(&buffer, &len);
  print_sdoc_to_file(f, s);
  fputc('\0', f);
  fclose(f);
  return buffer;
}

struct record_entry*
record_entry(struct arena* a, const char* key, struct doc* value, struct record_entry* next) {
  struct record_entry* e = arena_alloc(a, sizeof(struct record_entry));
  e->key = key;
  e->value = value;
  e->next = next;
  return e;
}

struct record*
record(struct arena* a, const char* name, struct record_entry* entries) {
  struct record* r = arena_alloc(a, sizeof(struct record));
  r->name = name;
  r->entries = entries;
  return r;
}

struct doc* doc_of_record(struct arena* st, struct record* r) {
  struct record_entry* it = r->entries;

  if (!it)
    return text(r->name);

  struct doc* middle = nil();
  while (it) {
    struct doc* tail = it->next ? cons(text(","), br(" ")) : nil();
    middle = cons(middle, cons(group(cons(text(it->key), cons(text(" ="), nest(2, cons(br(" "), it->value))))), tail));
    it = it->next;
  }

  struct doc* entries = middle;
  entries = nest(2, cons(br(" "), entries));
  return cons(text(r->name), cons(text(" "), cons(text("{"), cons(entries, cons(br(" "), text("}"))))));
}

#include <stdio.h>
#include "lambda.h"

#define var(x) (lam_variable(st, x))
#define abs(x, e) (lam_abs(st, x, e))
#define i(i) (lam_integer(st, i))
#define app(f, x) (lam_app(st, f, x))

int main(void) {
  struct arena a;
  struct arena* st = &a;
  arena_init(st);

  /* k = (fun x y -> x) */
  struct lam* k = abs("x", abs("y", var("x")));

  /* l = k 3 4 */
  struct lam* l = app(app(k, i(3)), i(4));

  int widths[] = { 40, 80, 100, 800, 0 };
  for (unsigned i = 0; widths[i]; i++) {
    char* p = pretty_lam(widths[i], l);
    puts(p);
    puts("====");
    free(p);
  }

  arena_destroy(st);
}

A toy ASDL (Abstract Syntax Description Language) intended to make
writing compilers in C less tedious.

---

The input language allows you to specify variant types (using an
OCaml-like syntax):

```
%{
#include <stddef.h>
#include <stdint.h>
#include <arena.h>
#include <pretty.h>
#include <printers.h>
%}

type lam =
  | Variable of { name: "const char*/quoted" }
  | Integer of { value: "int/int" }
  | Abs of {
      param: "const char*/quoted";
      body: lam;
    }
  | App of { fn: lam; arg: lam; }
```

The generator then produces C representations of the specified
datatypes (as tagged unions or simple enums), construction functions,
and pretty printers.  The `"foo/bar"` notation allows the user to
specify a C type for a member (the `/bar` notation specifies that the
related pretty printer for `foo` must be called `doc_of_bar`).

The sample above comes from `examples/lambda.sm` and can be executed as follows:
```
cd examples
make run
```
The program pretty prints the lambda term representing `(fun x y -> x) 3 4`
and prints it to the screen at different widths.

For example, a truncated output may look like:
```
...
App {
  fn =
    App {
      fn = Abs { param = "x", body = Abs { param = "y", body = Variable { name = "x" } } },
      arg = Integer { value = 3 }
    },
  arg = Integer { value = 4 }
}
====
App { fn = App { fn = Abs { param = "x", body = Abs { param = "y", body = Variable { name = "x" } } }, arg = Integer { value = 3 } }, arg = Integer { value = 4 } }
====
```

---

### TODO

- You should be able specify (literal) default values for fields in
  `with { ... }` (undocumented) - then, if a foreign field has been
  defaulted, it can be omitted from the constructor's parameter
  listing.
- Add some builtin type constructors, e.g. `lam list` should pretty
  print as `[...]`.
- Perhaps generate hashing and equality routines, to support generated
  datatypes being keys in hash tables etc.
- Mutual recursion isn't currently implemented. By design, all `type`s
  are considered mutually recursive, so various parts of the emission
  code ought to emit prototypes.
- Cleanup a lot of duplicated emission code.
- If more features are added, they should be configurable.
- Better CLI (currently, given `file.sm`, the generator just emits
  `file.c` and `file.h` into the working directory).

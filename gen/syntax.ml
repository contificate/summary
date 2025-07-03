
type ty =
  | Ctor of ty list * string
  | Foreign of string * string option
[@@deriving show { with_path = false }]

type component = string * ty
[@@deriving show { with_path = false }]

type ctor = string * component list
[@@deriving show { with_path = false }]

type decl =
  | Variant of
      { name: string;
        variants: ctor list;
        extra: component list; }
  | Enum of { name: string; names: string list; }
[@@deriving show { with_path = false }]

type md =
  | Module of { preamble: string list; decls: decl list; }
[@@deriving show { with_path = false }]

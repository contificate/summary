type t =
  | IDENT of string
  | QUOTED of string
  | PREAMBLE of string
  | TYPE
  | OF
  | WITH
  | EQUAL
  | PIPE
  | LBRACE
  | RBRACE
  | COLON
  | SEMICOLON
  | EOF
[@@deriving show { with_path = false }]

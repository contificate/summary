
let _prog = {|
%{
#include <stddef.h>
#include <stdint.h>
#include "arena.h"
#include "pretty.h"
#include "printers.h"
%}

type bop = Add | Sub | Mul

type expr =
  | Variable of { name: "struct ident*" }
  | Bop of { op: bop; left: expr; right: expr; }
 with { ty: "struct type*" }

type re =
  | Void
  | Epsilon
  | Character of { value: "char/foo" }
  | Concat of { left: re; right: re; }
  | Or of { left: re; right: re; }
  | Star of { re: re; }
|}

let try_compile file =
  try
    let ch = open_in file in
    let header_file, source_file =
      let prefix =
        match String.split_on_char '.' file with
        | t :: _ -> t
        | _ -> file
      in
      prefix ^ ".h", prefix ^ ".c"
    in
    let md = Parser.parse_module_from_channel ch in
    let header = open_out header_file in
    let source = open_out source_file in
    Printf.fprintf source "#include \"%s\"" header_file;
    Emit.emit_module ~header ~source md;
    close_out header;
    close_out source
  with
  | _ -> ()

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: tl -> List.iter try_compile tl
  | _ -> ()

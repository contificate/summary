open Syntax
open Printf

let ctor_is_nullary (_, xs) =
  xs = []

(* A variant is nullary if all its constructors are nullary and it has
   no extra structural elements. *)
let variant_is_nullary ~variants ~extra =
  extra = [] && List.for_all ctor_is_nullary variants

(* Rewrite variants with no structure (only comprising nullary
   constructors) as enums. This allows simpler unboxing of the
   constructors. *)
let enumify_decl = function
  | Variant { name; variants; extra }
       when variant_is_nullary ~variants ~extra ->
     let names, _ = List.split variants in
     Enum { name; names }
  | e -> e

let is_an_enum = function
  | Enum _ -> true
  | _ -> false

type emission_ctx =
  { lookup: string -> decl; }

(* For each declared type, emit a table - indexable by enums - of
   constructor names. *)
let emit_name_tables ~out decls =
  let entries =
    let get = function
      | Enum { name; names } -> name, names
      | Variant { name; variants; _ } ->
         let names, _ = List.split variants in
         name, names
    in
    List.map get decls
  in
  let go (n, ns) =
    let count = List.length ns in
    let ns =
      List.map (sprintf {|"%s"|}) ns
      |> String.concat ", "
    in
    fprintf out "const char* %s_name_table[%d] = { %s };\n\n" n count ns
  in
  List.iter go entries

let enumify ~prefix name =
  let prefix, name = String.(uppercase_ascii prefix, uppercase_ascii name) in
  sprintf "%s_%s" prefix name

let emit_decl_structure ~ctx ~out decls =
  let go = function
    | Enum { name; names } ->
       let names =
         let prefix = String.uppercase_ascii name in
         List.map (enumify ~prefix) names
         |> String.concat ", "
       in
       fprintf out "enum %s { %s };\n\n" name names
    | Variant { name; variants; extra } ->
       let type_is_an_enum = function
         | Ctor ([], t) -> is_an_enum (ctx.lookup t)
         | _ -> false
       in
       let typify = function
         | Ctor ([], t) as ty when type_is_an_enum ty ->
            sprintf "enum %s" t
         | Ctor (_, t) ->
            sprintf "struct %s*" t
         | Foreign (t, _) -> t
       in
       let names, _components = List.split variants in
       let enums =
         let prefix = String.uppercase_ascii name in
         List.map (enumify ~prefix) names
         |> String.concat ", "
       in
       fprintf out "struct %s {\n" name;
       fprintf out "  enum { %s } type;\n" enums;
       let emit_union () =
         let go (ctor, parts) =
           (* Only emit a union entry for non-nullary variants. *)
           if parts <> [] then
             begin
               let accessor = String.lowercase_ascii ctor in
               fprintf out "    struct {\n";
               let go' (p, pty) =
                 let p = String.lowercase_ascii p in
                 let pty = typify pty in
                 fprintf out "      %s %s;\n" pty p
               in
               List.iter go' parts;
               fprintf out "    } %s;\n" accessor;
             end
         in
         fprintf out "  union {\n";
         List.iter go variants;
         fprintf out "  } as;\n";
       in
       (* Only emit a union if there exists a non-nullary variant. *)
       if not (variant_is_nullary ~variants ~extra:[]) then
         emit_union ();
       (* Emit the extra structural elements. *)
       let emit_extra (e, ety) =
         let e = String.lowercase_ascii e in
         let ety = typify ety in
         fprintf out "  %s %s;\n" ety e
       in
       List.iter emit_extra extra;
       fprintf out "};\n\n";
  in
  List.iter go decls

(* Emits foo_bar(arena, bar specific fields..., extra...); constructor
   prototypes (for the header file). *)
let emit_ctor_prototypes ~ctx ~out decls =
  let type_is_an_enum = function
    | Ctor ([], t) -> is_an_enum (ctx.lookup t)
    | _ -> false
  in
  let typify = function
    | Ctor ([], t) as ty when type_is_an_enum ty ->
       sprintf "enum %s" t
    | Ctor (_, t) ->
       sprintf "struct %s*" t
    | Foreign (t, _) -> t
  in
  let go = function
    | Enum _ ->
       (* Currently, we don't emit enum constructors. However, it may
          be worth doing so in order to simplify extension
          (i.e. adding structure to a previously-an-enum type). *)
       ()
    | Variant { name; variants; extra } ->
       let rty = sprintf "struct %s*" name in
       let prefix = String.lowercase_ascii name in
       let go (ctor, parts) =
         let ctor = String.lowercase_ascii ctor in
         let args =
           parts @ extra
           |> List.map (fun (p, pty) -> sprintf "%s %s" (typify pty) p)
           |> List.cons "struct arena* arena"
           |> String.concat ", "
         in
         fprintf out "%s %s_%s(%s);\n" rty prefix ctor args
       in
       List.iter go variants
  in
  List.iter go decls

(* Emit the constructor definitions (source file). *)
let emit_ctors ~ctx ~out decls =
  let type_is_an_enum = function
    | Ctor ([], t) -> is_an_enum (ctx.lookup t)
    | _ -> false
  in
  let typify = function
    | Ctor ([], t) as ty when type_is_an_enum ty ->
       sprintf "enum %s" t
    | Ctor (_, t) ->
       sprintf "struct %s*" t
    | Foreign (t, _) -> t
  in
  let go = function
    | Enum _ -> ()
    | Variant { name; variants; extra } ->
       let ty = sprintf "struct %s" name in
       let rty = sprintf "%s*" ty in
       let prefix = String.lowercase_ascii name in
       let go (ctor, parts) =
         let ctor = String.lowercase_ascii ctor in
         let args =
           parts @ extra
           |> List.map (fun (p, pty) -> sprintf "%s %s" (typify pty) p)
           |> List.cons "struct arena* arena"
           |> String.concat ", "
         in
         fprintf out "%s %s_%s(%s) {\n" rty prefix ctor args;
         fprintf out "  %s r = arena_alloc(arena, sizeof(%s));\n" rty ty;
         fprintf out "  r->type = %s;\n" (enumify ~prefix:name ctor);
         let populate_field accessor (f, _) =
           let f = String.lowercase_ascii f in
           fprintf out "  r->%s%s = %s;\n" accessor f f
         in
         let accessor = sprintf "as.%s." (String.lowercase_ascii ctor) in
         List.iter (populate_field accessor) parts;
         List.iter (populate_field "") extra;
         fprintf out "  return r;\n";
         fprintf out "}\n\n"
       in
       List.iter go variants
  in
  List.iter go decls

(* For each structural type, create a function that populates a
   generic record data structure (for which a pretty printer already
   exists). *)
let emit_record_ctors ~ctx:_ ~out decls =
  let go = function
    | Enum { name; _ } ->
       let name = String.lowercase_ascii name in
       fprintf out "struct record* record_of_%s(struct arena* a, enum %s t) {\n" name name;
       fprintf out "  return record(a, %s_name_table[t], NULL);\n" name;
       fprintf out "}\n\n"
    | Variant { name; variants; _ } ->
       let converter_of_type : ty -> (string -> string) option = function
         | Foreign (_, Some tag) ->
            Some (fun arg -> sprintf "doc_of_%s(a, %s)" tag arg)
         | Ctor (_, t) ->
            Some (fun arg ->
                sprintf "doc_of_record(a, record_of_%s(a, %s))" t arg)
         | _ -> None
       in
       let name = String.lowercase_ascii name in
       fprintf out "struct record* record_of_%s(struct arena* a, struct %s* t) {\n" name name;
       fprintf out "  const char* name = %s_name_table[t->type];\n" name;
       fprintf out "  struct record_entry* es = NULL;\n";
       fprintf out "\n";
       fprintf out "  switch (t->type) {\n";
       let emit_case (ctor, parts) =
         let enum = enumify ~prefix:name ctor in
         fprintf out "    case %s: {\n" enum;
         let go' (p, pty) =
           match converter_of_type pty with
           | Some f ->
              let ctor = String.lowercase_ascii ctor in
              let converter = f (sprintf "t->as.%s.%s" ctor p) in
              fprintf out "      es = record_entry(a, \"%s\", %s, es);\n" p converter
           | _ -> ()
         in
         List.(iter go' (rev parts));
         fprintf out "      break;\n";
         fprintf out "    }\n";
       in
       List.iter emit_case variants;
       fprintf out "  }\n";
       fprintf out "\n";
       fprintf out "  return record(a, name, es);\n";
       fprintf out "}\n\n"
  in
  List.iter go decls

let emit_pretty_printer_prototypes ~ctx:_ ~out decls =
  let go = function
    | Variant { name; _ } | Enum { name; _ } as d ->
       let name = String.lowercase_ascii name in
       let ty =
         if is_an_enum d then
           sprintf "enum %s" name
         else
           sprintf "struct %s*" name
       in
       fprintf out "char* pretty_%s(int width, %s t);\n\n" name ty
  in
  List.iter go decls

let emit_pretty_printers ~ctx:_ ~out decls =
  let go = function
    | Variant { name; _ } | Enum { name; _ } as d ->
       let name = String.lowercase_ascii name in
       let ty =
         if is_an_enum d then
           sprintf "enum %s" name
         else
           sprintf "struct %s*" name
       in
       fprintf out "char* pretty_%s(int width, %s t) {\n" name ty;
       fprintf out "  struct arena a;\n";
       fprintf out "  arena_init(&a);\n";
       fprintf out "  char* s = string_of_sdoc(format(&a, width, doc_of_record(&a, record_of_%s(&a, t))));\n" name;
       fprintf out "  arena_destroy(&a);\n";
       fprintf out "  return s;\n";
       fprintf out "}\n\n"
  in
  List.iter go decls

let emit_prettifiers ~ctx ~out decls =
  emit_record_ctors ~ctx ~out decls;
  emit_pretty_printers ~ctx ~out decls

let emit_header ~ctx ~out (Module { preamble; decls }) =
  fprintf out "// begin header\n";
  fprintf out "#pragma once\n";
  List.iter (fprintf out "%s\n") preamble;
  emit_decl_structure ~ctx ~out decls;
  emit_ctor_prototypes ~ctx ~out decls;
  emit_pretty_printer_prototypes ~ctx ~out decls

let emit_source ~ctx ~out (Module { decls; _ }) =
  fprintf out "// begin source\n";
  emit_name_tables ~out decls;
  emit_ctors ~ctx ~out decls;
  emit_prettifiers ~ctx ~out decls

let emit_module ~header ~source (Module { preamble; decls }) =
  (* Identify variants that represent simple enums. *)
  let decls = List.map enumify_decl decls in
  let md =
    Module { preamble; decls }
  in
  let ctx =
    let lookup =
      let tbl = Hashtbl.create 32 in
      let add = function
        | Variant { name; _ } | Enum { name; _ } as d ->
           Hashtbl.replace tbl name d
      in
      List.iter add decls;
      Hashtbl.find tbl
    in
    { lookup }
  in
  emit_header ~out:header ~ctx md;
  emit_source ~out:source ~ctx md;
  ()

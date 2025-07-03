
module Lexer = struct
  type token = Token.t

  type t =
    { mutable tokens: token list; }

  let of_lbuf lbuf =
    let rec go acc =
      let t = Lexer.tokenise lbuf in
      let acc = t :: acc in
      if t <> Token.EOF then
        go acc
      else
        List.rev acc
    in
    let tokens = go [] in
    (* List.iter (fun t -> Printf.printf "%s\n" (Token.show t)) tokens; *)
    { tokens }

  let of_string input = of_lbuf (Lexing.from_string input)

  let of_channel ch = of_lbuf (Lexing.from_channel ch)

  let advance ({ tokens } as l) =
    match tokens with
    | t :: ts ->
       l.tokens <- ts;
       t
    | [] -> EOF

  let peek l =
    let prev = l.tokens in
    let t = advance l in
    l.tokens <- prev;
    t

  let skip l =
    ignore (advance l : Token.t)

  let expect l f =
    let t = advance l in
    if not (f t) then
      failwith "didn't match"

  let expect_as l f =
    let t = advance l in
    match f t with
    | Some a -> a
    | _ -> failwith "didn't match"
end

module type Lex = sig
  type t
  type token
  val advance : t -> token
  val peek : t -> token
  val skip : t -> unit
  val expect : t -> (token -> bool) -> unit
  val expect_as : t -> (token -> 'a option) -> 'a
end

module Make(L: Lex with type token := Token.t) = struct
  open L
  open Token
  open Syntax

  let parse_ident lexer =
    expect_as lexer (function IDENT i -> Some i | _ -> None)

  let parse_type lexer = match advance lexer with
    | IDENT i -> Ctor ([], i)
    | QUOTED q ->
       let prefix, tag =
         (match String.split_on_char '/' q with
          | prefix :: tag :: _ -> prefix, Some tag
          | _-> q, None)
       in
       Foreign (prefix, tag)
    | _ -> failwith "failed to parse type"

  let parse_component lexer =
    let name = parse_ident lexer in
    expect lexer ((=) COLON);
    let ty = parse_type lexer in
    (name, ty)

  let parse_record lexer =
    expect lexer ((=) LBRACE);
    let rec go cs = match peek lexer with
      | RBRACE -> cs
      | SEMICOLON ->
         skip lexer;
         (match peek lexer with
          | RBRACE -> cs
          | _ -> go (parse_component lexer :: cs))
      | _ -> failwith "asfaf"
    in
    let cs = go [parse_component lexer] in
    expect lexer ((=) RBRACE);
    List.rev cs

  (* Foo [of { x: ty; ... }] *)
  let parse_variant lexer : Syntax.ctor =
    let name = parse_ident lexer in
    let components =
      match peek lexer with
      | OF ->
         skip lexer;
         parse_record lexer
      | _ -> []
    in
    (name, components)

  let parse_variants lexer =
    if peek lexer = PIPE then
      skip lexer;
    let rec go vs = match peek lexer with
      | PIPE -> skip lexer; go (parse_variant lexer :: vs)
      | _ -> vs
    in
    let vs = go [parse_variant lexer] in
    List.rev vs

  let parse_type_decl lexer =
    expect lexer ((=) TYPE);
    let name = parse_ident lexer in
    expect lexer ((=) EQUAL);
    let variants = parse_variants lexer in
    let extra =
      if peek lexer = WITH then
        (skip lexer; parse_record lexer)
      else
        []
    in
    Variant { name; variants; extra }

  let parse_module lexer =
    let rec go (pres, decls) = match (peek lexer) with
      | PREAMBLE p ->
         skip lexer;
         go (p :: pres, decls)
      | TYPE ->
         let d = parse_type_decl lexer in
         go (pres, d :: decls)
      | EOF -> List.(rev pres, rev decls)
      | t -> failwith ("what " ^ Token.show t)
    in
    let pres, decls = go ([], []) in
    Module { preamble = pres; decls }
end

let parse_module input =
  let lexer = Lexer.of_string input in
  let module P = Make(Lexer) in
  P.parse_module lexer

let parse_module_from_channel ch =
  let lexer = Lexer.of_channel ch in
  let module P = Make(Lexer) in
  P.parse_module lexer

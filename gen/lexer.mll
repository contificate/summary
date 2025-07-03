{ open Token }

rule tokenise = parse
| [' ' '\t' '\n' '\r']+ { tokenise lexbuf }
| "type" { TYPE }
| "of" { OF }
| "with" { WITH }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_']* as i { IDENT i }
| "%{" { preamble (Buffer.create 32) lexbuf }
| '"' { quoted (Buffer.create 16) lexbuf }
| '{' { LBRACE }
| '}' { RBRACE }
| '=' { EQUAL }
| '|' { PIPE }
| ':' { COLON }
| ';' { SEMICOLON }
| eof { EOF }
and quoted buffer = parse
| '"' { QUOTED (Buffer.contents buffer) }
| _ as c { Buffer.add_char buffer c; quoted buffer lexbuf }
and preamble buffer = parse
| "%}" { PREAMBLE (Buffer.contents buffer) }
| _ as c { Buffer.add_char buffer c; preamble buffer lexbuf }

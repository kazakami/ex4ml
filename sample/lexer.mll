{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("and", Parser.AND);
  ("fun", Parser.FUN);
  ("dfun", Parser.DFUN);
  ("rec", Parser.REC);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "-" { Parser.MINUS }
| "*" { Parser.MULT }
| "/" { Parser.DIV}
| "(" " "* ([':' '=' '<' '>' '@' '^' '|' '&' '+' '-' '*' '/' '$' '%'] ['&' '!' '$' '%' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']* as mid) " "* ")"
    { Parser.PMID mid }
| "<" { Parser.LT }
| "&&" { Parser.LAND }
| "||" { Parser.LOR }
| "=" { Parser.EQ }
| "->" { Parser.RARROW }
| "::" { Parser.CONS }
| "[" " "* "]" { Parser.EMPLIST }
| "|" { Parser.PIPE }
| "[" { Parser.LSPAREN }
| "]" { Parser.RSPAREN }
| ";" { Parser.SEMIC }
| "_" { Parser.UNDERSCORE }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }



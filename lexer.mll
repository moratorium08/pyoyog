let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let small = ['a'-'z']
let big   = ['A'-'Z']
let symid = small (small | big)*
let varid = big (small | big)*

rule main = parse
| space+       { main lexbuf }
| "("          { Parser.OPAR }
| ")"          { Parser.CPAR }
| ":-"         { Parser.ARROW }
| ","          { Parser.COMMA }
| "."          { Parser.DOT }
| "assert"     { Parser.ASSERT }
| "\\+"        { Parser.NOT }
| digit+ as n  { Parser.INT (int_of_string n) }
| symid as id  { Parser.SYMID id }
| varid as id  { Parser.VARID id }
| _            { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}


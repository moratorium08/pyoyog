
%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <string> SYMID
%token <string> VARID
%token OPAR CPAR DOT COMMA
%token ARROW
%token ASSERT

%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
    | ASSERT OPAR rule CPAR DOT       { CRule ($3) }
    | goal DOT                        { CAsk  ($1) }
;

rule:
  | predicate                   { ($1, []) }
  | predicate ARROW goal        { ($1, $3) }
;

goal:
  | predicate COMMA goal            { $1 :: $3}
  | predicate                       { [$1] }
;

predicate:
  | pname OPAR terms CPAR            { ($1, $3) }
  | pname                           { ($1, [])}
;

terms:
  | term COMMA terms   {$1 :: $3 }
  | term               { [$1] }
;

term:
  | SYMID                   { EConstSym ($1) }
  | VARID                   { EVar ($1) }
  | pname OPAR terms CPAR   { EFunctor($1, $3) }

pname:
  | SYMID { $1 }
;

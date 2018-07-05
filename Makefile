SOURCES = syntax.ml lexer.mll parser.mly tySyntax.ml constraintSolver.ml rules.ml eval.ml main.ml
RESULT  = main

YFLAGS = -v
OCAMLFLAGS = -g


all: byte-code byte-code-library

-include OCamlMakefile

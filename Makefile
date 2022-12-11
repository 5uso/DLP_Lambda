.PHONY: run

all: lambda parser lexer main run
	ocamlc -o top str.cma lambda.cmo parser.cmo lexer.cmo main.cmo
	ocamlc -o run str.cma lambda.cmo parser.cmo lexer.cmo run.cmo

lambda: lambda.ml lambda.mli
	ocamlc -c lambda.mli lambda.ml

parser: parser.mly
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

main: main.ml
	ocamlc -c main.ml

run: run.ml
	ocamlc -c run.ml

clean:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmo *~


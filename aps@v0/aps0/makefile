LEX_ML = ocamllex
YACC_ML = /usr/local/bin/ocamlyacc
OCAMLC = ocamlc

eval: type eval.ml
	$(OCAMLC) -o eval ast.cmo lexer.cmo parser.cmo eval.ml

type: parser apsToProlog.ml
	$(OCAMLC) -o type ast.cmo lexer.cmo parser.cmo apsToProlog.ml

parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

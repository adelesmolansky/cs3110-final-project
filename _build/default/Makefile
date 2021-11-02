.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

client:
	ocamlbuild -use-ocamlfind src/client/client_view.byte && ./client_view.byte

server: 
	ocamlbuild -use-ocamlfind src/server/server_view.byte && ./server_view.byte

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f finalproject.zip
	zip -r finalproject.zip . -x@exclude.lst

clean:
	dune clean
	rm -f finlproject.zip

doc:
	dune build @doc
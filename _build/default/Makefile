.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f finlproject.zip
	zip -r finlproject.zip . -x@exclude.lst

clean:
	dune clean
	rm -f finlproject.zip

doc:
	dune build @doc

server:
	ocamlbuild -use-ocamlfind server_view.byte && ./server_view.byte -port ${PORT}
#eg make PORT=9999 server

client:
	ocamlbuild -use-ocamlfind client_view.byte && ./client_view.byte -host ${HOST} -port ${PORT}
#eg make HOST="127.0.0.1" PORT=9999 client

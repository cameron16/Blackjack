SHELL=/bin/bash

main:
	ocamlbuild types.byte
	ocamlbuild processor.byte
	ocamlbuild -pkgs graphics gui.byte
	ocamlbuild -pkgs graphics main.byte

clean:
	ocamlbuild -clean

play:
	ocamlbuild types.byte
	ocamlbuild processor.byte
	ocamlbuild -pkgs graphics gui.byte
	ocamlbuild -pkgs graphics main.byte
	utop


# test:
# 	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte

ocamlbuild src/bson.byte
ocamlbuild src/bson.native
ocamlbuild -I src test/test_bson.native

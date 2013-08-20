SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

syntax: setup.data.syntax
	$(SETUP) -build $(BUILDFLAGS)

client: setup.data.client
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	@ocamldoc -html -d doc/ src/bson.mli

test: setup.data build
	@ocamlbuild -use-ocamlfind -package bson.syntax -package deriving-ocsigen -package deriving-ocsigen.syntax -I src test/test_bson.native

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

setup.data.syntax:
	$(SETUP) -configure --enable-syntax $(CONFIGUREFLAGS)

setup.data.client:
	$(SETUP) -configure --enable-syntax --enable-client $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

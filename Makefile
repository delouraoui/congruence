OCAMLBUILD=ocamlbuild

LANGS = \
	src/closure \

SRCDIR = src

BYTETARGETS = $(LANGS:=.byte)
NATIVETARGETS = $(LANGS:=.native)

.PHONY: native byte

default: native

native: $(NATIVETARGETS)

byte: $(BYTETARGETS)

$(NATIVETARGETS): %.native : %.ml
	$(OCAMLBUILD) -use-ocamlfind -pkg dolmen -I $(SRCDIR) $@

$(BYTETARGETS): %.byte : %.ml
	$(OCAMLBUILD) -use-ocamlfind -use-menhir -pkg dolmen -I (SRCDIR) $@
clean:
	$(OCAMLBUILD) -clean

#all:
#	ocamlbuild src/* -use-ocamlfind closure.native

#clean:
#	ocamlbuild -clean
#	rm -f *.dot

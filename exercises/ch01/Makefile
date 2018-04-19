MAKEFLAGS := --no-builtin-rules

OCAMLC_OPTIONS := -w A -warn-error A
OCAMLC_BYTE    := ocamlc.opt   $(OCAMLC_OPTIONS)

EXECUTABLES := \
	straight_line_program_interpreter \
	tree

.PHONY: build clean demo_unbalanced

build : $(EXECUTABLES)

%: %.ml %.cmo %.cmi
	$(OCAMLC_BYTE) -o $@ $*.cmo

%.cmi: %.mli
	$(OCAMLC_BYTE) -o $@ -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC_BYTE) -c $<

clean:
	rm -f $(EXECUTABLES)

tree.png: tree
	./tree a b c d e f g h i j k l m n o p q r s t u v foo bar kgkvbkvg lkhjlk gfjyfjf fdtrdchfhtr trhfgfch hjlilijhl iygkyugkgkhy | neato -T png > tree.png

demo_unbalanced: tree.png
	sxiv ./tree.png

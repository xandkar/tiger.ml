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

tree : tree.ml tree.cmo tree_sig.cmo tree_unbalanced_vanilla.cmo tree_balanced_red_black.cmo
	$(OCAMLC_BYTE) -o tree tree_sig.cmo tree_unbalanced_vanilla.cmo tree_balanced_red_black.cmo tree.cmo

tree.cmo : tree.ml tree.cmi tree_sig.cmo tree_unbalanced_vanilla.cmo tree_balanced_red_black.cmo
	$(OCAMLC_BYTE) -c $<

tree_balanced_red_black.cmo : tree_balanced_red_black.ml tree_balanced_red_black.cmi tree_sig.cmo
	ocamlc.opt -w A -warn-error A-4 -c $<

tree_sig.cmo tree_sig.cmi: tree_sig.ml
	$(OCAMLC_BYTE) -c $<

clean:
	rm -f $(EXECUTABLES) tree.dot  # There's also tree.png, but I'm keeping it.

tree.dot: tree
	./tree a b c d e f g h i j k l m n o p q r s t u v foo bar kgkvbkvg lkhjlk gfjyfjf fdtrdchfhtr trhfgfch hjlilijhl iygkyugkgkhy > tree.dot

tree.png: tree.dot
	neato -T png tree.dot > tree.png

demo_unbalanced: tree.png
	sxiv ./tree.png

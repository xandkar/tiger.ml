MAKEFLAGS := --no-builtin-rules

OCAMLC_OPTIONS := -w A -warn-error A
OCAMLC_BYTE    := ocamlc.opt   $(OCAMLC_OPTIONS)

.PHONY: build clean

build : \
	straight_line_program_interpreter \
	tree

%: %.ml %.cmo %.cmi
	$(OCAMLC_BYTE) -o $@ $*.cmo

%.cmi: %.mli
	$(OCAMLC_BYTE) -o $@ -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC_BYTE) -c $<

clean:
	rm -f straight_line_program_interpreter

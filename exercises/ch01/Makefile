MAKEFLAGS := --no-builtin-rules

OCAMLC_OPTIONS := -w A -warn-error A
OCAMLC_BYTE    := ocamlc.opt   $(OCAMLC_OPTIONS)

EXECUTABLES := \
 straight_line_program_interpreter \
 tree_demo
EXECUTABLES := $(addsuffix .byte,$(EXECUTABLES))

SET_MEMBERS := \
 a b c d e f g h i j k l m n o p q r s t u v foo bar kgkvbkvg \
 lkhjlk gfjyfjf fdtrdchfhtr trhfgfch hjlilijhl iygkyugkgkhy


.PHONY: \
 all \
 clean \
 build \
 demos

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) demos

clean:
	rm -rf ./_build/
	rm -f $(EXECUTABLES)
	# There're also tree demo PNGs, but I want to keep them around, for
	# referencing without having to build the project.

build:
	ocamlbuild $(EXECUTABLES)

demos: \
 run_straight_line_program_interpreter \
 tree_demo_unbalanced.png \
 tree_demo_balanced.png

run_straight_line_program_interpreter: straight_line_program_interpreter.byte
	./$<

tree_demo_unbalanced.dot: tree_demo.byte
	./$< unbalanced $(SET_MEMBERS) > $@

tree_demo_unbalanced.png: tree_demo_unbalanced.dot
	neato -T png $< > $@

tree_demo_balanced.dot: tree_demo.byte
	./$< balanced $(SET_MEMBERS) > $@

tree_demo_balanced.png: tree_demo_balanced.dot
	neato -T png $< > $@

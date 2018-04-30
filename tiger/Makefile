MAKEFLAGS := --no-builtin-rules

EXE_TYPE := byte # byte | native
EXECUTABLES := tiger
OCAMLBUILD_FLAGS := -I src/exe -I src/lib/tiger
OCAMLBUILD := ocamlbuild $(OCAMLBUILD_FLAGS)

.PHONY: \
 all \
 build \
 clean \
 test

all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) test

build:
	$(OCAMLBUILD) $(addsuffix _main.$(EXE_TYPE),$(EXECUTABLES))
	mkdir -p bin/exe
	$(foreach exe,$(EXECUTABLES),cp _build/src/exe/$(exe)_main.$(EXE_TYPE) bin/exe/$(exe))
	rm $(addsuffix _main.$(EXE_TYPE),$(EXECUTABLES))

clean:
	$(OCAMLBUILD) -clean
	rm -rf ./bin

test: bin/exe/tiger
	./$<

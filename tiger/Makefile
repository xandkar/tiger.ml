MAKEFLAGS := --no-builtin-rules

EXE_TYPE         := byte  # byte | native
EXECUTABLES      := tigerc tiger_tests
OCAMLBUILD_FLAGS := -I src/exe -I src/lib/tiger
OCAMLBUILD       := ocamlbuild $(OCAMLBUILD_FLAGS)

.PHONY: \
 all \
 build \
 clean \
 test

all:
	@$(MAKE) -s clean
	@$(MAKE) -s build
	@$(MAKE) -s test

build:
	@$(OCAMLBUILD) $(addsuffix .$(EXE_TYPE),$(EXECUTABLES))
	@mkdir -p bin/exe
	$(foreach exe,$(EXECUTABLES),cp _build/src/exe/$(exe).$(EXE_TYPE) bin/exe/$(exe); )
	@rm $(addsuffix .$(EXE_TYPE),$(EXECUTABLES))

clean:
	@$(OCAMLBUILD) -clean
	@rm -rf ./bin

test: bin/exe/tiger_tests
	@./$<

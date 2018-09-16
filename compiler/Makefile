MAKEFLAGS := --no-builtin-rules

DIR_TEST_CASES := ./testcases

EXE_TYPE              := native  # byte | native
EXECUTABLES           := tigerc tigert
OCAMLBUILD_FLAGS_PKGS := -pkg unix
OCAMLBUILD_FLAGS_DIRS := -I src/exe -I src/lib/tiger
OCAMLBUILD_FLAGS_COMP := -cflags '-g -w A'
OCAMLBUILD_FLAGS_YACC := -yaccflag '-v'
OCAMLBUILD            := \
  ocamlbuild \
    $(OCAMLBUILD_FLAGS_PKGS) \
    $(OCAMLBUILD_FLAGS_COMP) \
    $(OCAMLBUILD_FLAGS_DIRS) \
    $(OCAMLBUILD_FLAGS_YACC)

.PHONY: \
  all \
  build \
  clean \
  test_all \
  test_book \
  test_micro \
  test

all: clean
	@$(MAKE) -s test

build:
	@$(OCAMLBUILD) $(addsuffix .$(EXE_TYPE),$(EXECUTABLES))
	@mkdir -p bin/exe
	$(foreach exe,$(EXECUTABLES),cp _build/src/exe/$(exe).$(EXE_TYPE) bin/exe/$(exe); )
	@rm $(addsuffix .$(EXE_TYPE),$(EXECUTABLES))

clean:
	@$(OCAMLBUILD) -clean
	@rm -rf ./bin

test: test_all

test_all: build
	@./bin/exe/tigert all -dir $(DIR_TEST_CASES)

test_book: build
	@./bin/exe/tigert book -dir $(DIR_TEST_CASES)

test_micro: build
	@./bin/exe/tigert micro

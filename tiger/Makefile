MAKEFLAGS := --no-builtin-rules

EXE_TYPE              := byte  # byte | native
EXECUTABLES           := tigerc tiger_tests
OCAMLBUILD_FLAGS_DIRS := -I src/exe -I src/lib/tiger
OCAMLBUILD_FLAGS_COMP := -cflags '-w A'
OCAMLBUILD            := \
  ocamlbuild \
    -classic-display \
    $(OCAMLBUILD_FLAGS_COMP) \
    $(OCAMLBUILD_FLAGS_DIRS)

.PHONY: \
  all \
  build \
  clean \
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

test: build
	@./bin/exe/tiger_tests

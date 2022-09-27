GHC ?= ghc
GHCFLAGS = -dynamic -no-keep-hi-files -no-keep-o-files -Wall

BUILDDIR ?= ./build

.PHONY: build
build: clean
	@mkdir -p $(BUILDDIR)
	$(GHC) $(GHCFLAGS) -o $(BUILDDIR)/main ./main.hs

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)

.PHONY: test
test: build
	@echo "Running tests :)"
	
	@for input_file in $$(ls ./testdata/input.*); do \
		printf "Running test case from \"%s\"... " $${input_file}; \
		diff -a <($(BUILDDIR)/main < $${input_file}) ./testdata/expected.$${input_file##*.} && echo OK || (echo FAILED && exit 2); \
	done

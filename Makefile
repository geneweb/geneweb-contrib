.DEFAULT_GOAL = all
.PHONY: clean all

all:
	dune build @all

%.exe:
	dune build $@

clean:
	dune clean

.DEFAULT_GOAL = all
.PHONY: clean all

all:
	dune build @all

clean:
	dune clean

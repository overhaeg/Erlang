ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM)

test:
	./run.sh server_multiple_actors test

clean:
	rm *.beam

%.beam: %.erl
	erlc +debug_info $*.erl

.PHONY: all clean

all: compile start

compile:
	rebar compile

start:
	erl -pa 'ebin' -boot start_sasl -eval 'application:start(tictactoe).'

clean:
	$(RM) ebin/* erl_crash.dump

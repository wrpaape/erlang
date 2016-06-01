.PHONY: all clean

all:
	rebar compile
	erl -pa 'ebin' -boot start_sasl

clean:
	$(RM) ebin/* erl_crash.dump

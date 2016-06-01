.PHONY: all clean

all:
	rebar compile
	erl -pa 'ebin' -boot start_sasl -eval 'application:start(hash_table).'

clean:
	$(RM) ebin/* erl_crash.dump

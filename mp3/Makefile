.PHONY: all clean

ERL := erl
ERL_FLAGS = -boot start_clean

ERLC := erlc
ERLC_FLAGS = -Wall -b beam -pa ebin -o ebin

define ERLC_RULE
$(strip $1: $2 $3)
	$(ERLC) $(ERLC_FLAGS) $$<

TARGETS += $1
endef

define RULES
$(foreach module,$(wildcard *.erl),
$(call ERLC_RULE,$(module:%.erl=ebin/%.beam),$(module)))
endef

$(eval $(RULES))

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

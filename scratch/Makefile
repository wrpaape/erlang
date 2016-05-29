.PHONY: all clean

define ERLC_RULE
$(strip $1: $2 $3)
	erlc -W -b beam -pa ebin -o ebin $$<

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

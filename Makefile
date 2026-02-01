.PHONY: test eunit ct proper compile clean

# Suppress logger output during tests
TEST_FLAGS = ERL_FLAGS="-kernel logger_level emergency"

compile:
	rebar3 compile

test: eunit ct proper

eunit:
	$(TEST_FLAGS) rebar3 eunit --cover

ct:
	rebar3 ct --readable true --cover

proper:
	$(TEST_FLAGS) rebar3 proper --cover

dialyzer:
	rebar3 dialyzer

xref:
	rebar3 xref

cover:
	rebar3 cover

clean:
	rebar3 clean

.DEFAULT_GOAL := compile

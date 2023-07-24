ERL = erl
REBAR = rebar3

all: compile

compile:
	@$(REBAR) compile

get-deps:
	@$(REBAR) upgrade

deps:
	@$(REBAR) compile

.PHONY: clean
clean:
	@$(REBAR) clean

eunit:
	@$(REBAR) eunit

run:
	@echo $(OS)
	@.\\script\\start.bat

stop:
	@echo $(OS)
	@.\\script\\stop.bat

# windows 环境
win: compile
	@.\\script\\start.bat

.PHONY: test
test: eunit
	@$(ERL) -make
	@$(ERL)  \
		-config config/sys.config \
		-pa _build/default/lib/bson/ebin \
		-pa _build/default/lib/cowboy/ebin \
		-pa _build/default/lib/cowlib/ebin \
		-pa _build/default/lib/gamechat/ebin \
		-pa _build/default/lib/goldrush/ebin \
		-pa _build/default/lib/gpb/ebin \
		-pa _build/default/lib/jsx/ebin \
		-pa _build/default/lib/lager/ebin \
		-pa _build/default/lib/metronome/ebin \
		-pa _build/default/lib/mongodb/ebin \
		-pa _build/default/lib/pbkdf2/ebin \
		-pa _build/default/lib/poolboy/ebin \
		-pa _build/default/lib/ranch/ebin \
		-pa _build/default/lib/catsvr/ebin \
		-pa _build/test/lib/catsvr/ebin \
		-s main_test run \
		-s init stop

show:
	@echo $(OS)


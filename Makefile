REPO       ?= hashids
ERLANG_BIN  = $(shell dirname $(shell which erl))

DEPS_PLT     = $(REPO)_plt
DEF_PLT_APPS = sasl erts kernel stdlib eunit
PLT_APPS     = $(DEF_PLT_APPS)

# The release branch should have a file named USE_REBAR_LOCKED
use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  	rebar_config = rebar.config.locked
else
  	rebar_config = rebar.config
endif
REBAR = ./rebar -C $(rebar_config)

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

.PHONY: all deps clean test coverage typer build_plt analysis

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:
	@rm -rf ./.eunit
	@rm -rf ./.rebar
	@rm -rf ./ebin
	@$(REBAR) clean

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) -v skip_deps=true eunit

coverage:
	@open ./.eunit/index.html

typer:
	@typer --plt $(DEPS_PLT) -r ./src

build_plt:
	@dialyzer --build_plt --output_plt $(DEPS_PLT) --apps $(PLT_APPS)

analysis:
	@dialyzer --src ./src --plts $(DEPS_PLT) -Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_behaviours -Wunderspecs

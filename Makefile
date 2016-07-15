REPO       ?= hashids
ERLANG_BIN  = $(shell dirname $(shell which erl))

DEPS_PLT     = $(REPO)_plt
DEF_PLT_APPS = sasl erts kernel stdlib eunit
PLT_APPS     = $(DEF_PLT_APPS)

REBAR = ./rebar3

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

.PHONY: all clean distclean test coverage typer build_plt analysis edoc

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean: clean
	@rm -rf ./_plt
	@rm -rf ./_build

test:
ifeq ($(suite),)
	@$(REBAR) do eunit, cover --verbose
else
	@$(REBAR) do eunit --suite $(suite), cover --verbose
endif

coverage:
	@open ./_build/test/cover/index.html

typer:
	@typer --plt $(DEPS_PLT) -r ./src

build_plt:
	@dialyzer --build_plt --output_plt $(DEPS_PLT) --apps $(PLT_APPS)

analysis:
	@dialyzer --src ./src --plts $(DEPS_PLT) -Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_behaviours -Wunderspecs

edoc:
	rm -rf ./doc
	@$(REBAR) edoc

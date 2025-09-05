MAIN := zeta
TEST := ./tests/tests.scm
LIBS := ./zeta-lib/cmds.scm ./zeta-lib/prompts.scm ./zeta-lib/system.scm ./zeta-lib/term.scm
BUILD := guild compile -L ./
BUILD_NO_DEPS := guild compile

.PHONY: all test

all: $(MAIN) cmds system term prompts
	$(BUILD) $(MAIN)

test: cmds system term prompts
	$(BUILD) $(TEST)
	guile -L . $(TEST)
cmds: system prompts term ./zeta-lib/cmds.scm
	$(BUILD) ./zeta-lib/cmds.scm

system: term ./zeta-lib/system.scm
	$(BUILD) ./zeta-lib/system.scm

prompts: ./zeta-lib/prompts.scm
	$(BUILD_NO_DEPS) ./zeta-lib/prompts.scm

term: ./zeta-lib/term.scm
	$(BUILD_NO_DEPS) ./zeta-lib/term.scm


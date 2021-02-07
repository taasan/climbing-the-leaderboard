HOOKS = $(addprefix .git/hooks/, pre-commit pre-push)

.PHONY: all
all: intl lint $(HOOKS)

hooks: $(HOOKS)

.git/hooks/%: hooks/hooks.sh
	ln -s -f  ../../$< $@

.PHONY: test-ps
test-ps:
	npm t

.PHONY: test-haskell
test-haskell:
	cabal test

.PHONY: test-exec
test-exec:
	test/run-tests

.PHONY: test
test: test-ps test-haskell test-exec

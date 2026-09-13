EMACS ?= emacs
PKG = org-x
LOAD_PATH  += -L .
LOAD_PATH  += -L ./tests

.PHONY: test init

init: ## initiate
init:
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l init.el;

test: ## Run tests
test:
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l org-x-tests.el \
		 --eval "(generate-run-tests-batch-and-exit)";

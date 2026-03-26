EMACS ?= emacs
PKG = org-x
LOAD_PATH  += -L .
LOAD_PATH  += -L ./tests

.PHONY: test

test: ## Run tests
test: 
	$(EMACS) --batch -L . \
		 $(LOAD_PATH) \
		 -l org-x-tests.el \
		 --eval "(ert-run-tests-batch-and-exit)";



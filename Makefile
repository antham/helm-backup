EMACS ?= emacs
EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
CASK=cask
PKG_DIR := $(shell ${CASK} package-directory)

test: unit-tests

unit-tests:
	${CASK} exec ert-runner

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

downloads :
	${CASK} install

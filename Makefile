EMACS=emacs
EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch

CASK=cask

test: unit-tests

unit-tests:
	${CASK} exec ${EMACS} ${EMACS_BATCH} -L . \
		-l helm-backup-test.el -f ert-run-tests-batch-and-exit
test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

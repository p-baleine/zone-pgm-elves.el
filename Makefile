EMACS ?= emacs
CASK ?= cask

test:
	${MAKE} clean \
	&& ${MAKE} build \
	&& ${MAKE} unittest \
	&& ${MAKE} clean

unittest:
	${CASK} exec ert-runner

build:
	${CASK} build

clean:
	rm -f *.elc

quiet:
	${CASK} emacs -q

.PHONY: test clean quiet

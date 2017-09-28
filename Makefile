# Makefile for mumble
#
# Habitual Lisp users can just use the ASDF systems directly; the
# purpose of this Makefile is to have an easy way to build a
# standalone executable (with buildapp) for those not sure what to do.

mumble:	$(wildcard src/*.lisp) mumble.asd vendor
	buildapp --output $@ --asdf-tree vendor --load-system mumble --entry mumble:main

vendor:
	git submodule update --init --recursive

check:
	sbcl --non-interactive --eval '(asdf:test-system :mumble)'

.PHONY: check

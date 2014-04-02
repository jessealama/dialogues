.PHONY: clean test all

emacs-backups = $(wildcard *~)
ccl-fasls = $(wildcard *.dx64fsl)
plain-fasls = $(wildcard *.fasl)
fasls = $(ccl-fasls) $(plain-fasls)

all: kuno

clean:
ifneq ($(strip $(emacs-backups)),)
	rm -f $(emacs-backups)
endif
ifneq ($(strip $(fasls)),)
	rm -f $(fasls)
endif

kuno: kuno.lisp
	CC=gcc sbcl --script $<

test:
	test -d test
	test -r test/Makefile
	+make -C test

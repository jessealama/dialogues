.PHONY: clean test all

emacs-backups = $(wildcard *~)
ccl-fasls = $(wildcard *.dx64fsl)
plain-fasls = $(wildcard *.fasl)
fasls = $(ccl-fasls) $(plain-fasls)

all: kuno

clean:
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	find . -mindepth 1 -maxdepth 1 -type f -name '.*~' -delete
	find . -mindepth 1 -maxdepth 1 -type d ! -name '.git' -exec basename {} ';' | parallel --jobs=1 --halt-on-error=1 $(MAKE) -C {}
	rm -f kuno

kuno: kuno.lisp
	which sbcl
	CC=gcc sbcl --script $<

test:
	test -d test
	test -r test/Makefile
	+make -C test

# Local Variables:
# mode: makefile-gmake
# End:

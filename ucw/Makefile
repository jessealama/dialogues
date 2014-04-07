.PHONY: clean test

emacs-backups = $(wildcard *~)
ccl-fasls = $(wildcard *.dx64fsl)
plain-fasls = $(wildcard *.fasl)
fasls = $(ccl-fasls) $(plain-fasls)

clean:
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	find . -mindepth 1 -maxdepth 1 -type f -name '.*~' -delete
	find . -mindepth 1 -maxdepth 1 -type f -name '*.fasl' -delete
	find . -mindepth 1 -maxdepth 1 -type f -name '*.dx64fsl' -delete
	find . -mindepth 1 -maxdepth 1 -type d ! -name '.git' -exec basename {} ';' | parallel --jobs=1 --halt-on-error=1 $(MAKE) -C {}

test:
	test -d test
	test -r test/Makefile
	+make -C test

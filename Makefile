.PHONY: clean test

asd-files = $(wildcard *.asd)
lisp-files = $(wildcard *.lisp)
editable-files = $(asd-files) $(lisp-files) Makefile README.mkd .gitignore
emacs-backups = $(wildcard *~)
ccl-fasls = $(wildcard *.dx64fsl)
plain-fasls = $(wildcard *.fasl)
fasls = $(ccl-fasls) $(plain-fasls)

clean:
ifneq ($(strip $(emacs-backups)),)
	rm -f $(emacs-backups)
endif
ifneq ($(strip $(fasls)),)
	rm -f $(fasls)
endif

test:
	test -d test
	test -r test/Makefile
	+make -C test

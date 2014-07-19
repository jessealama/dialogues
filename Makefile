.PHONY: clean test all

lisps = $(wildcard *.lisp)

all: kuno

include clean.mk

clean: clean-emacs clean-subdirectories
	$(call trash,'*.fasl')
	$(call trash,'*.dx64fsl')
	rm -f kuno

kuno: $(lisps) Makefile
	which sbcl
	CC=gcc sbcl --script kuno.lisp

test: kuno
	# no tests defined

# Local Variables:
# mode: makefile-gmake
# End:

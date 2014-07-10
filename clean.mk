.PHONY: clean-emacs clean-subdirectories

# functions
trash = find . -maxdepth 1 -mindepth 1 -type f -name $1 -delete

clean-emacs:
	$(call trash,'*~')
	$(call trash,'.*~')

clean-subdirectories:
	which parallel
	find . -maxdepth 1 -mindepth 1 -type d ! -name '.git' -exec basename {} ';' | parallel --jobs=1 --halt-on-error=1 $(MAKE) -C {} clean

# Local Variables:
# mode: makefile-gmake
# End:

lisp_LISP = gyach.el gyach-extra.el gyach-button.el gyach-faces.el gyach-url.el \
	gyach-state.el gyach-version.el 
DIST_SOURCES  = $(lisp_LISP) gyach-version.el.in
EXTRA_DIST = gyach-version.el.in
BUILT_SOURCES = gyach-version.el
CLEAN_FILES = gyach-version.el

gyach-version.el: Makefile
	sed -e "s/VERSION/$(VERSION)/g" <$@.in >$@

lisp_LISP = gyach.el gyach-extra.el gyach-button.el gyach-faces.el gyach-version.el
DIST_SOURCES  = $(lisp_LISP)

BUILT_SOURCES = gyach-version.el
CLEAN_FILES = gyach-version.el

gyach-version.el: Makefile
	echo "(defvar gyach-version \"$(VERSION)\")" >$@

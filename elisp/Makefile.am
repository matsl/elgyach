lisp_LISP = gyach-away.el gyach-custom.el gyach-highlight.el gyach-pal.el gyach-score.el gyach.el utility.el gyach-button.el gyach-events.el gyach-ignore.el gyach-profile.el gyach-state.el match.el gyach-cipher.el gyach-extra.el gyach-logging.el gyach-quote.el gyach-url.el mpg123el-show.el gyach-complete.el gyach-faces.el gyach-mpg123el.el gyach-room.el gyach-version.el rijndael.el

DIST_SOURCES  = $(lisp_LISP) gyach-version.el.in
EXTRA_DIST = gyach-version.el.in
BUILT_SOURCES = gyach-version.el
CLEAN_FILES = gyach-version.el

gyach-version.el: Makefile
	sed -e "s/BUILD_VERSION/$(VERSION)/g" <$@.in >$@

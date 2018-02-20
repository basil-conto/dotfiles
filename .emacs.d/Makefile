## Build Emacs configuration for blc

## Author:   Basil L. Contovounesios <basil.conto@gmail.com>
## Homepage: https://github.com/basil-conto/dotfiles

## Files

libs   := $(addprefix blc-,lib magit mbsync notmuch pass pkg)
libdir := lisp/
pkgdir := elpa/

## Macros

libouts = $(patsubst %,$(libdir)%.elc,$(filter $(1)%,$(libs)))
thmouts = $(1)*-theme.elc

## Emacs Lisp

.SUFFIXES:
.SUFFIXES: .el .elc

EMACS ?= emacs

%.elc: %.el
	$(EMACS) $(ELFLAGS) --batch --funcall=batch-byte-compile $^

## Sweeping generalisations

.PHONY: all
all: themes libs gnus init

.PHONY: clean
clean: clean-themes clean-libs clean-gnus clean-init

.PHONY: distclean
distclean: clean
	$(RM) --recursive $(pkgdir)

## Libs

%-theme.elc: $(libdir)%-theme.elc
	install --mode=644 $< .

.PHONY: themes
themes: $(call thmouts)

.PHONY: clean-themes
clean-themes:
	$(RM) $(call thmouts,$(libdir))

.PHONY: blc-lib
blc-lib: $(call libouts,blc-lib)

.PHONY: blc-magit
blc-magit: blc-lib blc-pkg $(call libouts,blc-magit)

.PHONY: blc-mbsync
blc-mbsync: blc-lib $(call libouts,blc-mbsync)

.PHONY: blc-notmuch
blc-notmuch: blc-lib $(call libouts,blc-notmuch)

.PHONY: blc-pass
blc-pass: blc-lib $(call libouts,blc-pass)

.PHONY: blc-pkg
blc-pkg: blc-lib $(call libouts,blc-pkg)

.PHONY: libs
libs: $(libs)

.PHONY: clean-libs
clean-libs:
	$(RM) $(call libouts)

## Init files

.PHONY: gnus
gnus: blc-lib blc-mbsync blc-notmuch gnus.elc

.PHONY: clean-gnus
clean-gnus:
	$(RM) gnus.elc

.PHONY: init
init: themes blc-lib blc-magit blc-mbsync blc-pass blc-pkg \
		early-init.elc init.elc

.PHONY: clean-init
clean-init:
	$(RM) early-init.elc init.elc

.PHONY: profile
profile: ELFLAGS := -nw
profile: themes blc-lib blc-pkg
	$(EMACS) $(ELFLAGS) --quick --load=$(libdir)profile-dotemacs.el \
		--funcall=profile-dotemacs

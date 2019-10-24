## Build Emacs configuration for blc

## Author:   Basil L. Contovounesios <basil.conto@gmail.com>
## Homepage: https://gitlab.com/basil-conto/dotfiles

## Emacs Lisp

EMACS ?= emacs

%.elc: %.el
	$(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile $<

## Macros

lib    = $(1:%=$(libdir)blc-%.elc)
liball = $(patsubst %.el,%.elc,$(wildcard $(libdir)blc-*-$(1).el))

## Files

pkgdir := elpa/
libdir := lisp/
libs   := $(call lib,backup lib magit mbsync notmuch pass pkg)
themes := $(call liball,theme)
tests  := $(call liball,tests)
inits  := $(addsuffix .elc,early-init init)

## Actions

.PHONY: all
all: themes libs gnus inits test

.PHONY: clean
clean: clean-themes clean-libs clean-gnus clean-inits clean-tests

.PHONY: distclean
distclean: clean
	$(RM) --recursive $(pkgdir) index/package-quickstart.el

.PHONY: themes clean-themes
themes:               $(themes)
clean-themes: ; $(RM) $(themes)

.PHONY: libs clean-libs
libs:               $(libs)
clean-libs: ; $(RM) $(libs)

.PHONY: gnus clean-gnus
gnus:               gnus.elc
clean-gnus: ; $(RM) gnus.elc

.PHONY: inits clean-inits
inits:               $(inits)
clean-inits: ; $(RM) $(inits)

.PHONY: test
test: $(tests)
	$(EMACS) --batch $(ELFLAGS) $(addprefix --load=,$^) \
		--funcall=ert-run-tests-batch-and-exit

.PHONY: clean-tests
clean-tests:
	$(RM) $(tests)

.PHONY: profile
profile: init.elc
	$(EMACS) --quick $(ELFLAGS) --load=$(libdir)profile-dotemacs.el \
		--funcall=profile-dotemacs

## Dependencies

$(call lib,backup):    $(call lib,lib)
$(call lib,magit):     $(call lib,lib pkg)
$(call lib,mbsync):    $(call lib,lib)
$(call lib,notmuch):   $(call lib,lib)
$(call lib,pass):      $(call lib,lib)
$(call lib,pkg):       $(call lib,lib)
$(call lib,lib-tests): $(call lib,lib)

gnus.elc: $(call lib,lib mbsync notmuch)
init.elc: $(themes) $(call lib,backup lib magit mbsync pass pkg) early-init.elc

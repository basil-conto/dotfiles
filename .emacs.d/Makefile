## Build Emacs configuration for blc

## Author:   Basil L. Contovounesios <basil@contovou.net>
## Homepage: https://git.sr.ht/~blc/dotfiles

## Emacs Lisp

EMACS ?= emacs

%.elc: %.el
	$(EMACS) --quick --batch $(ELFLAGS) --funcall=batch-byte-compile $<

## Macros

lib   = $(1:%=$(libdir)/blc-%.elc)
wildc = $(addsuffix c,$(wildcard $(1)))

## Files

pkgdir := elpa
libdir := lisp
early  := early-init.elc
init   := init.elc
gnus   := gnus.elc
tests  := $(call wildc,$(libdir)/*-tests.el)
libs   := $(call lib,backup lib magit mbsync pkg)
inits  := $(early) $(init) $(call wildc,$(libdir)/default.el)

## Actions

.PHONY: all
all: libs gnus inits test

.PHONY: clean
clean: clean-libs clean-gnus clean-inits clean-tests

.PHONY: distclean
distclean: clean
	$(RM) --recursive $(pkgdir) index/package-quickstart.el \
		index/package-quickstart.elc

.PHONY: libs clean-libs
libs:               $(libs)
clean-libs: ; $(RM) $(libs)

.PHONY: gnus clean-gnus
gnus:               $(gnus)
clean-gnus: ; $(RM) $(gnus)

.PHONY: inits clean-inits
inits:               $(inits)
clean-inits: ; $(RM) $(inits)

.PHONY: test
test: $(tests)
	$(EMACS) --quick --batch $(ELFLAGS) $(addprefix --load=,$^) \
		--funcall=ert-run-tests-batch-and-exit

.PHONY: clean-tests
clean-tests:
	$(RM) $(tests)

.PHONY: profile
profile: init.elc
	$(EMACS) --quick $(ELFLAGS) --load=$(libdir)/profile-dotemacs.el \
		--funcall=profile-dotemacs

## Dependencies

$(call lib,backup):    $(call lib,lib)
$(call lib,magit):     $(call lib,lib pkg)
$(call lib,mbsync):    $(call lib,lib)
$(call lib,pkg):       $(call lib,lib)
$(call lib,lib-tests): $(call lib,lib)

$(gnus): $(call lib,lib mbsync)
$(init): $(libs) $(early)

# Build blc emacs configuration

# Author:   Basil L. Contovounesios <basil.conto@gmail.com>
# Homepage: https://github.com/basil-conto/dotfiles

# Files

libs := blc-lib blc-macs blc-notmuch blc-pass
srcs := gnus init magit

libdir := lisp/
pkgdir := elpa/

# Macros

libouts = $(patsubst %,$(libdir)%.elc,$(filter $(1)%,$(libs)))
srcouts = $(addsuffix .elc,$(filter $(1)%,$(srcs)))

# Emacs Lisp

EL ?= emacs

%.elc: %.el
	$(EL) $(ELFLAGS) --batch --funcall batch-byte-compile $^

# Rules

.NOTPARALLEL:

.PHONY: all
all: libs sources

.PHONY: libs
libs: $(libs)

.PHONY: sources
sources: $(srcs)

.PHONY: blc-lib
blc-lib: $(call libouts,blc-lib)

.PHONY: blc-macs
blc-macs: blc-lib $(call libouts,blc-macs)

.PHONY: blc-notmuch
blc-notmuch: blc-lib $(call libouts,blc-notmuch)

.PHONY: blc-pass
blc-pass: blc-lib blc-macs $(call libouts,blc-pass)

.PHONY: gnus
gnus: $(filter-out blc-pass,$(libs)) $(call srcouts,gnus)

.PHONY: clean-gnus
clean-gnus:
	$(RM) $(call srcouts,gnus)

.PHONY: init
init: $(filter-out blc-notmuch,$(libs)) $(call srcouts,init)

.PHONY: clean-init
clean-init:
	$(RM) $(call srcouts,init)

.PHONY: magit
magit: init $(call srcouts,magit)

.PHONY: clean-magit
clean-magit:
	$(RM) $(call srcouts,magit)

.PHONY: profile
profile: ELFLAGS := -nw
profile:
	$(EL) $(ELFLAGS) --quick --load $(addprefix $(libdir),profile-dotemacs.el) \
		--funcall profile-dotemacs

.PHONY: clean
clean: clean-gnus clean-init clean-magit
	$(RM) $(call libouts,)

.PHONY: distclean
distclean: clean
	$(RM) --recursive $(pkgdir)

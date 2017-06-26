# Build blc emacs configuration

# Author:   Basil L. Contovounesios <basil.conto@gmail.com>
# Homepage: https://github.com/basil-conto/dotfiles

# Files

libs := blc-lib blc-macs
srcs := gnus init

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
all: $(srcs)

.PHONY: blc-lib
blc-lib: $(call libouts,blc-lib)

.PHONY: blc-macs
blc-macs: $(call libouts,blc-macs)

.PHONY: gnus
gnus: $(libs) $(call srcouts,gnus)

.PHONY: init
init: $(libs) $(call srcouts,init)

.PHONY: profile
profile: ELFLAGS := -nw
profile:
	$(EL) $(ELFLAGS) --quick --load $(addprefix $(libdir),profile-dotemacs.el) \
		--funcall profile-dotemacs

.PHONY: clean
clean:
	$(RM) $(call libouts,) $(call srcouts,)

.PHONY: distclean
distclean: clean
	$(RM) --recursive $(pkgdir)

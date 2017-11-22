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
thmouts = $(1)*-theme.elc
srcouts = $(addsuffix .elc,$(filter $(1)%,$(srcs)))

# Emacs Lisp

EL ?= emacs

.SUFFIXES:
.SUFFIXES: .el .elc

%.elc: %.el
	$(EL) $(ELFLAGS) --batch --funcall batch-byte-compile $^

%-theme.elc: $(libdir)%-theme.elc
	install --mode=644 $< .

# Rules

.NOTPARALLEL:

.PHONY: all
all: libs themes sources

.PHONY: clean
clean: clean-libs clean-themes clean-sources

.PHONY: distclean
distclean: clean
	$(RM) --recursive $(pkgdir)

.PHONY: libs
libs: $(libs)

.PHONY: clean-libs
clean-libs:
	$(RM) $(call libouts,)

.PHONY: sources
sources: $(srcs)

.PHONY: clean-sources
clean-sources:
	$(RM) $(call srcouts,)

.PHONY: themes
themes: $(call thmouts,)

.PHONY: clean-themes
clean-themes:
	$(RM) $(call thmouts,$(libdir))

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
init: $(filter-out blc-notmuch,$(libs)) themes $(call srcouts,init)

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
profile: themes
	$(EL) $(ELFLAGS) --quick --load $(addprefix $(libdir),profile-dotemacs.el) \
		--funcall profile-dotemacs

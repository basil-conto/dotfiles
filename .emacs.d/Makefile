### Files

init    := init
gnus    := gnus
prof    := profile-dotemacs

lispdir := lisp/
elpadir := elpa/

suff-el := .el
suff-bc := .elc

targets := $(init) $(gnus)
list-bc := $(addsuffix $(suff-bc),$(targets))

vpath %$(suff-el) $(lispdir)

### Emacs Lisp

ifeq ($(strip $(EL)),)
	EL := emacs
endif

ELFILES        = --eval '(byte-compile-file "$<")'
LINK$(suff-el) = $(EL) $(ELFLAGS) $(ELPRE) $(ELFILES) $(ELPOST)

%$(suff-bc): %$(suff-el)
	$(LINK$(suff-el))

### Rules

.NOTPARALLEL:
.DEFAULT_GOAL: $(init)

.PHONY: all
all: $(targets)

.PHONY: $(init)
$(init): ELFLAGS := --batch
$(init): $(filter $(init)%,$(list-bc))

.PHONY: $(gnus)
$(gnus): ELFLAGS := -nw
$(gnus): ELPRE   := --funcall gnus-slave
$(gnus): ELPOST  := --funcall gnus-group-save-newsrc --funcall kill-emacs
$(gnus): $(filter $(gnus)%,$(list-bc))

.PHONY: $(prof)
$(prof): ELFLAGS  := -nw
$(prof): ELPRE    := --quick
$(prof): ELFILES   = --load '$<'
$(prof): ELPOST    = --funcall '$@'
$(prof): $(addsuffix $(suff-el),$(prof))
	$(LINK$(suff-el))

.PHONY: profile
profile: $(prof)

.PHONY: clean
clean:
	$(RM) $(list-bc)

.PHONY: distclean
distclean: clean
	$(RM) -r $(elpadir)

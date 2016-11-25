init-el := init
prof-fn := profile-dotemacs

lispdir := lisp/
elpadir := elpa/

suff-el := .el
suff-bc := .elc

init-bc := $(init-el)$(suff-bc)
prof-el := $(prof-fn)$(suff-el)

vpath %$(suff-el) $(lispdir)

%$(suff-bc): %$(suff-el)
	emacs $(ELFLAGS) --batch --user $(shell whoami) \
		--eval '(byte-compile-file "$<")'

.PHONY: all
all: init

.PHONY: init
init: $(init-bc)

.PHONY: profile
profile: ELFLAGS := -nw
profile: $(prof-el)
	emacs $(ELFLAGS) --quick --load $< --funcall $(prof-fn)

.PHONY: clean
clean:
	$(RM) $(init-bc)

.PHONY: distclean
distclean: clean
	$(RM) -r $(elpadir)

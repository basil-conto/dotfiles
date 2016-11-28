init-el := init
gnus-el := .gnus
prof-fn := profile-dotemacs

lispdir := lisp/
elpadir := elpa/

suff-el := .el
suff-bc := .elc

list-bc := $(addsuffix $(suff-bc), $(init-el) $(gnus-el))
prof-el := $(prof-fn)$(suff-el)

vpath %$(suff-el) $(lispdir)

%$(suff-bc): %$(suff-el)
	emacs $(ELFLAGS) --batch --user $(shell whoami) \
		--eval '(byte-compile-file "$<")'

.PHONY: all
all: init gnus

.PHONY: init
init: $(filter $(init-el)%, $(list-bc))

.PHONY: gnus
gnus: $(filter $(gnus-el)%, $(list-bc))

.PHONY: profile
profile: ELFLAGS := -nw
profile: $(prof-el)
	emacs $(ELFLAGS) --quick --load $< --funcall $(prof-fn)

.PHONY: clean
clean:
	$(RM) $(list-bc)

.PHONY: distclean
distclean: clean
	$(RM) -r $(elpadir)

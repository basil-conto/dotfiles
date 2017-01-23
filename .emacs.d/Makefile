init-el := init
gnus-el := gnus
prof-fn := profile-dotemacs

lispdir := lisp/
elpadir := elpa/

suff-el := .el
suff-bc := .elc

list-bc := $(addsuffix $(suff-bc), $(init-el) $(gnus-el))
prof-el := $(addsuffix $(suff-el), $(prof-fn))

vpath %$(suff-el) $(lispdir)

ELTARGET          = --eval '(byte-compile-file "$<")'
COMPILE$(suff-el) = emacs $(ELFLAGS) $(ELPRE) $(ELTARGET) $(ELPOST)

%$(suff-bc): %$(suff-el)
	$(COMPILE$(suff-el))

.NOTPARALLEL:

.PHONY: all
all: init gnus

.PHONY: init
init: ELFLAGS := --batch
init: $(filter $(init-el)%, $(list-bc))

.PHONY: gnus
gnus: ELFLAGS := -nw
gnus: ELPRE   := --funcall gnus-slave
gnus: ELPOST  := --funcall gnus-group-save-newsrc --funcall kill-emacs
gnus: $(filter $(gnus-el)%, $(list-bc))

.PHONY: profile
profile: ELFLAGS  := -nw
profile: ELPRE     = --quick --load '$<' --funcall $(prof-fn)
profile: ELTARGET :=
profile: $(prof-el)
	$(COMPILE$(suff-el))

.PHONY: clean
clean:
	$(RM) $(list-bc)

.PHONY: distclean
distclean: clean
	$(RM) -r $(elpadir)

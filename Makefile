EMACS := emacs
PORT ?= 9090
SERVER ?= localhost

BATCH_EMACS=$(EMACS) --batch

ifneq ($(THEME),)
SET_THEME=--eval '(load-theme (quote $(THEME)) t)'
endif

ifneq ($(DOCROOT),)
SET_DOCROOT=--eval '(setq el-sprunge-docroot "$(DOCROOT)")'
endif

# Package variables
NAME=el-sprunge
VERSION=0.$(shell date +%Y%m%d)
DOC="Emacs powered sprunge server"
REQ=((emacs-web-server \"20130416.826\") (htmlize \"20130207\"))
DEFPKG="(define-package \"$(NAME)\" \"$(VERSION)\" \n  \"$(DOC)\" \n  '$(REQ))"
PACKAGE=$(NAME)-$(VERSION)

.PHONY: all src start package clean

SRC=$(wildcard *.el)

all: src

show-path:
	$(BATCH_EMACS) --eval "(mapc (lambda (p) (message \"%S\" p)) load-path)"

src: $(SRC)
	$(BATCH_EMACS) -f batch-byte-compile $^

%.txt: %
	$(BATCH_EMACS) $< -f org-ascii-export-to-ascii

%.html: %
	$(BATCH_EMACS) $< -f org-html-export-to-html

$(PACKAGE).tar: $(SRC) README.txt
	mkdir $(PACKAGE); \
	cp $^ $(PACKAGE); \
	mv $(PACKAGE)/README.txt $(PACKAGE)/README; \
	echo -e $(DEFPKG) > $(PACKAGE)/$(NAME)-pkg.el; \
	tar cf $(PACKAGE).tar $(PACKAGE); \
	rm -r $(PACKAGE)

package: $(PACKAGE).tar

start: $(SRC)
	$(filter-out --batch, $(BATCH_EMACS)) -Q -l $< \
	$(SET_THEME) $(SET_DOCROOT) \
	--eval '(setq el-sprunge-servername "$(SERVER)")' \
	--eval '(ews-start el-sprunge-handler $(PORT))'

clean:
	rm -f $(SRC:.el=.elc) $(NAME)-*.tar

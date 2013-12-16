EMACS := emacs

# Set these environment variables so that they point to the
# development directories of elnode.
ELPADIR ?= ~/.emacs.d/elpa/

BATCH_EMACS=$(EMACS) --batch --execute \
   '(mapc (lambda (dir) (add-to-list (quote load-path) dir)) \
     `(,@(mapcar (lambda (p) (expand-file-name p "$(ELPADIR)")) \
                 (directory-files "$(ELPADIR)"))))'

# Package variables
NAME=el-sprunge
VERSION=0.$(shell date +%Y%m%d)
DOC="Emacs powered sprunge server"
REQ=((elnode \"20130416.826\"))
DEFPKG="(define-package \"$(NAME)\" \"$(VERSION)\" \n  \"$(DOC)\" \n  '$(REQ))"
PACKAGE=$(NAME)-$(VERSION)

.PHONY: all src example package clean check test

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

clean:
	rm -f $(SRC:.el=.elc) $(NAME)-*.tar

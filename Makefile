SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/simple-log
headersDir = generated/headers

LFNs = simple-log example tests
LISPs = $(addsuffix .lisp, $(LFNs))
package = $(LISPs) simple-log.asd description.org version.org

OFNs = simple-log packaging
ORGs = $(addsuffix .org, $(OFNs))

# unless I mention generated/from/*.org files here, they will be considered temporary and auto-erased so emacsclient will always be called on every make:
all: quicklisp README.md generated/simple-log.tbz $(addprefix generated/from/, $(ORGs)) $(quicklispDir)/example.bin git
quicklisp: $(quicklispDir)/ $(addprefix $(quicklispDir)/, $(package)) $(addprefix generated/from/, $(ORGs))

$(quicklispDir)/example.bin: quicklisp generated/description.org
	@echo "*** COMPILING THE BINARY ***"
	$(SBCL) --quit --eval "(asdf:make :simple-log/example)"
	@echo "*** COMPILED THE BINARY ***"
	@echo "Now run it to see the log messages both in terminal and in the log file"
	-@chgrp tmp $@

generated/simple-log.tbz: quicklisp
	@echo "Testing before we package it:"
	@$(SBCL) --eval "(asdf:operate 'asdf:test-op :simple-log)" --eval "(uiop:quit simple-log/tests:N-failed)"
	@echo "\n\n`date '+%m/%d %H:%M'` ALL TESTS PASSED :)\n"
	tar jcfv $@ --directory=$(quicklispDir)/..  simple-log
	-@chgrp tmp $@

generated/description.org: description.org
	cat $< > $@
	-@chgrp tmp $@

$(quicklispDir)/%.lisp: generated/from/simple-log.org generated/from/packaging.org
	cat generated/headers/$(notdir $@) generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.asd: generated/from/packaging.org
	cat generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.org: %.org
	cat $< > $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/
	echo `emacsclient -e '(printangle "$<")'` | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-@chgrp tmp $@
	-@chmod a-x $@

clean:
	-$(SBCL) --quit --eval '(progn (asdf:clear-system :simple-log) (asdf:clear-system :simple-log/example)  (asdf:clear-system :simple-log/tests))'
	-rm -r $(quicklispDir) generated version.org

.PHONY: clean quicklisp all git

%/:
	[ -d $@ ] || mkdir -p $@

git: generated/simple-log.tbz next-commit.txt README.md
	@echo "===="
	-@echo "git commit -am '"`head -n1 next-commit.txt`"'"
	@echo "git push origin master"

version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | sed 's/"//g' > $@
	echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@

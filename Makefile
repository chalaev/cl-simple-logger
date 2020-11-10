
# The parameters SBCL and quicklispDir should probably be different on your computer:
# SBCL is the SBCL excecutable. You may use the standard one (/usr/bin/sbcl), but it is better to compile it manually.
SBCL = ~/local/bin/sbcl
# quicklispDir is the Quicklisp directory where my local packages are stored:
quicklispDir = ~/quicklisp/local-projects/simple-log/

simple: README.md generated/example.bin generated/description.org
	@echo "***TESTING SOURCE CODE IN INTERPRETING MODE***"
	$(SBCL) --quit --eval '(ql:quickload :simple-log)' --load generated/example.lisp --eval '(simple-log/example:main)'
	@echo "***=====***"
	@echo "*** You should have seen 4 log messages above, both in terminal and in log file ***"
	@echo "***=====***"
	@echo "*** Running the binary ./generated/example.bin should produce similar results (with different time stamps) ***"

generated/example.bin: $(quicklispDir)example.bin
	cp -a $< $@
	-chgrp tmp $@

$(quicklispDir)example.bin: generated/simple-log.lisp generated/description.org
	rsync -au goodies generated/example.lisp generated/simple-log.asd generated/simple-log.lisp description.org $(quicklispDir)
	@echo "*** COMPILING THE BINARY***"
	$(SBCL) --quit --load generated/simple-log.asd --eval "(asdf:make :simple-log/example)"
	-chgrp tmp $@

generated/simple-log.lisp: simple-log.org
	emacsclient -e '(org-babel-tangle-file "$<")'

generated/description.org: description.org
	rsync -au $< $@
	-chgrp tmp $@

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-chgrp tmp $@

clean:
	-rm -r generated/* $(quicklispDir)*

.PHONY: clean simple

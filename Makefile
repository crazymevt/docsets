# Makefile for docsets. Needs Quicklisp, loaded from your Lisp's init file:
# libraries are loaded with ql:quickload.

LISP ?= sbcl
RUN = $(LISP) --non-interactive --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)'
# The same without SBCL's banner, for targets that print a report
QUIET = $(LISP) --noinform --non-interactive --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: docset all outdated update check list test clean

# Write one library's docset to ~/.local/share/docsets/LIBRARY/:
#   make docset LIBRARY=bordeaux-threads
# A library with no file in libraries/ is documented from its docstrings.
docset:
	$(RUN) --eval '(ql:quickload :docsets :silent t)' \
		--eval '(let ((dir (docsets:generate "$(LIBRARY)"))) (uiop:quit (if (zerop (docset:check-docset dir)) 0 1)))'

# Every library in libraries/
all:
	$(RUN) --eval '(ql:quickload :docsets :silent t)' \
		--eval '(let ((n 0)) (dolist (dir (docsets:generate-all)) (incf n (docset:check-docset dir))) (uiop:quit (if (zerop n) 0 1)))'

# Which docsets in ~/.local/share/docsets/ are out of date: their library has
# a newer release installed (after (ql:update-dist "quicklisp"), say), or the
# generator has changed since they were made. Nothing is downloaded.
outdated:
	@$(QUIET) --eval '(ql:quickload :docsets :silent t)' --eval '(docsets:report-outdated)' 2>/dev/null

# Regenerate the docsets that are out of date, and check them
update:
	$(RUN) --eval '(ql:quickload :docsets :silent t)' \
		--eval '(let ((n 0)) (dolist (dir (docsets:update)) (incf n (docset:check-docset dir))) (uiop:quit (if (zerop n) 0 1)))'

# Check every docset in ~/.local/share/docsets/
check:
	$(RUN) --eval '(ql:quickload :docset :silent t)' \
		--eval '(let ((n 0)) (dolist (d (docset:find-docsets :systems nil)) (incf n (docset:check-docset (docset:docset-directory d)))) (uiop:quit (if (zerop n) 0 1)))'

list:
	@$(QUIET) --eval '(ql:quickload :docsets :silent t)' --eval '(format t "~{~a~%~}" (docsets:library-names))' 2>/dev/null

test:
	$(RUN) --eval '(ql:quickload :docsets/tests :silent t)' \
		--eval '(uiop:quit (if (eq :passed (parachute:status (parachute:test :docsets/tests))) 0 1))'

clean:
	find . -name '*.fasl' -delete

# Makefile for docsets. Needs Quicklisp, loaded from your Lisp's init file:
# libraries are loaded with ql:quickload.

LISP ?= sbcl
RUN = $(LISP) --non-interactive --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: docset all check list test clean

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

# Check every docset in ~/.local/share/docsets/
check:
	$(RUN) --eval '(ql:quickload :docset :silent t)' \
		--eval '(let ((n 0)) (dolist (d (docset:find-docsets :systems nil)) (incf n (docset:check-docset (docset:docset-directory d)))) (uiop:quit (if (zerop n) 0 1)))'

list:
	@$(RUN) --noinform --eval '(ql:quickload :docsets :silent t)' --eval '(format t "~{~a~%~}" (docsets:library-names))' 2>/dev/null

test:
	$(RUN) --eval '(ql:quickload :docsets/tests :silent t)' \
		--eval '(uiop:quit (if (eq :passed (parachute:status (parachute:test :docsets/tests))) 0 1))'

clean:
	find . -name '*.fasl' -delete

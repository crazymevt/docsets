;;;; uiop.lisp — ASDF's portability layer: files, pathnames, processes, images
;;;;
;;;; From docstrings. The UIOP package re-exports a dozen others (uiop/os,
;;;; uiop/pathname, uiop/run-program, ...), so the contents are grouped by
;;;; them. Code writes uiop:run-program, so the entries are UIOP's.

(define-library "uiop"
  :title "UIOP"
  :subtitle "Utilities for Implementation- and OS- Portability, part of ASDF"
  :source "https://gitlab.common-lisp.net/asdf/asdf/-/tree/master/uiop"
  :license "MIT (Expat)"
  :attribution "From the docstrings of UIOP, by the ASDF authors."
  :packages (("UIOP" :group-by-home-package t)))

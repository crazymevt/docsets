;;;; alexandria.lisp — the utilities most Lisp code uses
;;;;
;;;; From docstrings. ALEXANDRIA-2 re-exports all of ALEXANDRIA and adds a
;;;; few; only those few get entries of their own under it. "A" is the local
;;;; nickname many projects give it, so a:curry finds its entry.

(define-library "alexandria"
  :title "Alexandria"
  :subtitle "A collection of portable public domain utilities"
  :source "https://gitlab.common-lisp.net/alexandria/alexandria"
  :license "Public Domain / 0-clause MIT"
  :attribution "From the docstrings of Alexandria, by its authors."
  :packages (("ALEXANDRIA" :nicknames ("A") :toc-title "Alexandria")
             ("ALEXANDRIA-2" :toc-title "Alexandria 2 additions")))

;;;; plump.lisp — a lenient HTML and XML parser
;;;;
;;;; From docstrings, which plump keeps complete, with its README as the
;;;; overview. PLUMP re-exports plump-dom, plump-parser and plump-lexer, so
;;;; the contents are grouped by them.

(define-library "plump"
  :title "Plump"
  :subtitle "A lenient HTML and XML parser"
  :source "https://github.com/Shinmera/plump"
  :license "zlib"
  :attribution "From Plump's docstrings and README, by Yukari Hafner (Shinmera)."
  :readme "README.md"
  :packages (("PLUMP" :group-by-home-package t)))

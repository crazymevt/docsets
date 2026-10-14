;;;; dexador.lisp — an HTTP client
;;;;
;;;; Few of dexador's symbols have docstrings; its README documents the
;;;; request functions' arguments and how to use them, so it's the overview.
;;;; DEXADOR re-exports its error conditions and a few utilities from
;;;; packages of their own, so the contents are grouped by them.

(define-library "dexador"
  :title "Dexador"
  :subtitle "A fast HTTP client"
  :source "https://github.com/fukamachi/dexador"
  :license "MIT"
  :attribution "From Dexador's docstrings and README, by Eitaro Fukamachi."
  :readme "README.markdown"
  :packages (("DEXADOR" :group-by-home-package t)))

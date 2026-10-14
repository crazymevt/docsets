;;;; jzon.lisp — a correct and safe JSON reader and writer
;;;;
;;;; From docstrings, with jzon's README, which is its manual, as the
;;;; overview. Its package has no nickname of its own; JZON is the local
;;;; nickname code gives it, so jzon:parse finds its entry.

(define-library "jzon"
  :system "com.inuoe.jzon"
  :title "jzon"
  :subtitle "A correct and safe JSON parser and writer"
  :source "https://github.com/Zulu-Inuoe/jzon"
  :license "MIT"
  :attribution "From jzon's docstrings and README, by Wilfredo Velázquez-Rodríguez (Zulu-Inuoe)."
  :readme "../README.md"
  :packages (("COM.INUOE.JZON" :nicknames ("JZON"))))

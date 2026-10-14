;;;; bordeaux-threads.lisp — threads, locks, condition variables, semaphores
;;;;
;;;; APIv2 (BT2) is documented by the HyperSpec-style pages of the library's
;;;; site, which ship in its docs/content/; what they leave out comes from
;;;; docstrings. APIv1 (BT), which most code still uses, has only docstrings;
;;;; each of its entries links to the APIv2 entry of the same name.

(define-library "bordeaux-threads"
  :title "Bordeaux Threads"
  :subtitle "Portable shared-state concurrency"
  :source "https://github.com/sionescu/bordeaux-threads"
  :license "MIT"
  :attribution "From the bordeaux-threads documentation site (docs/content/) and docstrings, by the bordeaux-threads authors."
  :packages (("BORDEAUX-THREADS-2" :nicknames ("BT2") :pages "docs/content/" :toc-title "APIv2 (BT2)")
             ("BORDEAUX-THREADS" :nicknames ("BT") :prefix "bt:" :toc-title "APIv1 (BT)"
              :see-also "BORDEAUX-THREADS-2")))

;;;; docsets.asd

(defsystem "docsets"
  :description "Writes docsets of Lisp libraries from their docstrings, lambda lists, documentation pages and Texinfo manuals."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("docset" "sb-introspect" "sb-cltl2")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "text")
                             (:file "symbols")
                             (:file "markdown")
                             (:file "generate")
                             (:file "texinfo"))))
  :in-order-to ((test-op (test-op "docsets/tests"))))

(defsystem "docsets/tests"
  :description "Tests of the docstring, Markdown and Texinfo readers, and of a generated docset."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("docsets" "parachute")
  :components ((:file "tests/docsets"))
  :perform (test-op (o c) (uiop:symbol-call :parachute :test :docsets/tests)))

;;;; package.lisp

(defpackage #:docsets
  (:use #:cl)
  (:export
   ;; Libraries
   #:define-library #:find-library #:library-names #:load-libraries
   ;; Generating
   #:generate #:generate-all #:default-output-directory
   ;; Versions
   #:library-release #:generator-fingerprint #:docset-status #:docset-statuses
   #:report-outdated #:update
   ;; The pieces, for tests and for library files that need them
   #:docstring-blocks #:text-inlines #:summary
   #:symbol-definitions #:lambda-list-inlines
   #:markdown-inlines #:markdown-blocks #:read-page #:parse-title))

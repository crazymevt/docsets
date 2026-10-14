;;;; docsets.lisp — the docstring and Markdown readers, and a whole docset
;;;; written from the test package below and the pages in tests/pages/.

(defpackage #:docsets/tests
  (:use #:cl #:parachute))

(defpackage #:docsets-test-widgets
  (:use #:cl)
  (:export #:frob #:widget #:*widgets* #:with-widget #:make-widget))

(in-package #:docsets-test-widgets)

(defvar *widgets* '() "All the WIDGETs made so far.")

(defclass widget () () (:documentation "A thing to FROB."))

(defun make-widget () "Makes a WIDGET." (make-instance 'widget))

(defun frob (widget &key (hard t)) (declare (ignore hard)) widget)

(defmacro with-widget ((var &key (count 1)) &body body)
  "Binds VAR to a new widget around BODY.

  - COUNT is how many.
  - It is pushed on *WIDGETS*.

    (with-widget (w) (frob w))"
  (declare (ignore count))
  `(let ((,var (make-widget))) ,@body))

(defpackage #:docsets-test-gadgets
  (:use #:cl)
  (:import-from #:docsets-test-widgets #:frob #:make-widget)
  (:export #:frob #:make-widget #:polish))

(defun docsets-test-gadgets:polish (gadget) "Polishes GADGET." gadget)

(in-package #:docsets/tests)

(define-test docsets/tests)

(define-test docstrings
  :parent docsets/tests
  (let ((docsets::*params* '("var" "count" "body"))
        (docsets::*link-names* (make-hash-table :test 'equal)))
    (setf (gethash "*widgets*" docsets::*link-names*) "variable/*widgets*")
    (is equal '((:p "Binds " (:param "var") " to " (:code "nil") "."))
        (docsets:docstring-blocks "Binds VAR to NIL."))
    (is equal '((:p "Kept on " (:ref "variable/*widgets*" "*widgets*") ", as with " (:code "push") ". It IS so."))
        (docsets:docstring-blocks "Kept on *WIDGETS*, as with PUSH. It IS so."))
    (let ((blocks (docsets:docstring-blocks (documentation 'docsets-test-widgets:with-widget 'function))))
      (is equal '(:p "Binds " (:param "var") " to a new widget around " (:param "body") ".") (first blocks))
      (is equal '(:list (:item (:p (:param "count") " is how many."))
                  (:item (:p "It is pushed on " (:ref "variable/*widgets*" "*widgets*") ".")))
          (second blocks))
      (is equal '(:code-block "lisp" "(with-widget (w) (frob w))") (third blocks)))
    (is equal "Binds var to nil." (docsets:summary (docsets:docstring-blocks "Binds VAR to NIL. More.")))))

(define-test lambda-lists
  :parent docsets/tests
  (is equal '("(var " (:code "&key") " (count 1)) " (:code "&body") " body")
      (docsets:lambda-list-inlines '((docsets-test-widgets::var &key (docsets-test-widgets::count 1))
                                     &body docsets-test-widgets::body)))
  (is equal '("var" "count" "body")
      (docsets::lambda-list-params '((docsets-test-widgets::var &key (docsets-test-widgets::count 1))
                                     &body docsets-test-widgets::body)))
  (is equal '("place" "timeout" "supplied")
      (docsets::lambda-list-params '(place &optional ((:timeout timeout) 5 supplied)))))

(define-test definitions
  :parent docsets/tests
  (is equal '(:macro) (mapcar (lambda (d) (getf d :kind)) (docsets:symbol-definitions 'docsets-test-widgets:with-widget)))
  (is equal '(:variable) (mapcar (lambda (d) (getf d :kind)) (docsets:symbol-definitions 'docsets-test-widgets:*widgets*)))
  (is equal '(:class) (mapcar (lambda (d) (getf d :kind)) (docsets:symbol-definitions 'docsets-test-widgets:widget))))

(define-test markdown
  :parent docsets/tests
  (multiple-value-bind (kind kind-name names) (docsets:parse-title "Function ACQUIRE-LOCK, RELEASE-LOCK")
    (is eq :function kind)
    (is equal "Function" kind-name)
    (is equal '("acquire-lock" "release-lock") names))
  (multiple-value-bind (kind kind-name names) (docsets:parse-title "Host type NATIVE-LOCK")
    (is eq :host-type kind)
    (is equal "Host Type" kind-name)
    (is equal '("native-lock") names))
  (let ((docsets::*page-links* (make-hash-table :test 'equal))
        (docsets::*link-names* (make-hash-table :test 'equal)))
    (setf (gethash "lock" docsets::*page-links*) "class/lock")
    (is equal '("a " (:ref "class/lock" (:b "lock")) " or " (:link "http://x/y" (:i "*")) " " (:code "nil"))
        (docsets:markdown-inlines "a [**lock**](../lock) or [*\\**](http://x/y) `nil`"))
    (is equal '("missing") (docsets:markdown-inlines "[missing](nowhere)"))))

(define-test generated
  :parent docsets/tests
  (eval '(docsets:define-library "docsets-test-widgets"
          :system "docsets/tests" :title "Widgets"
          :packages (("DOCSETS-TEST-WIDGETS" :nicknames ("DTW") :pages "tests/pages/"))))
  (let ((dir (merge-pathnames (format nil "docsets-test-~D/" (random 1000000 (make-random-state t)))
                              (uiop:temporary-directory))))
    (unwind-protect
         (let* ((dir (docsets:generate "docsets-test-widgets" :output-directory dir))
                (d (docset:open-docset dir))
                (frob (docset:docset-entry d "function/frob")))
           (is = 0 (docset:check-docset dir :stream (make-broadcast-stream)))
           (is equal '("page/overview" "page/widgets" "function/frob"
                       "variable/*widgets*" "function/make-widget" "class/widget" "macro/with-widget")
               (mapcar #'docset:record-id (docset:docset-index d)))
           ;; The page, not the docstring-less function, documents FROB
           (is equal '(:syntax :kind :function :name ("frob") :args ("widget " (:code "&key") " " "(hard " "t)") :values ("widget"))
               (first (docset:entry-section frob "Syntax")))
           (is equal '(:p (:param "widget") "—a " (:ref "class/widget" "widget") ".")
               (first (docset:entry-section frob "Arguments and Values")))
           (is equal '(:p "Frobs " (:param "widget") ", harder when " (:param "hard") " is true. See "
                       (:ref "function/frob" "frob") ".")
               (first (docset:entry-section frob "Description")))
           (is = 2 (length (rest (second (docset:entry-section frob "Description")))))
           (is equal '(:code-block "lisp" "(frob (make-widget))") (third (docset:entry-section frob "Description")))
           ;; Looked up by nickname, as code writes it
           (is equal '("macro/with-widget")
               (mapcar (lambda (hit) (docset:record-id (cdr hit))) (docset:lookup-symbol (list d) "with-widget" "DTW")))
           (is equal '((:title "Overview" :entry "page/overview")
                       (:title "Widgets" :entry "page/widgets" :children ((:title "frob" :entry "function/frob")))
                       (:title "Other variables" :children ((:title "*widgets*" :entry "variable/*widgets*")))
                       (:title "Other functions" :children ((:title "make-widget" :entry "function/make-widget")))
                       (:title "Other classes" :children ((:title "widget" :entry "class/widget")))
                       (:title "Other macros" :children ((:title "with-widget" :entry "macro/with-widget"))))
               (docset:docset-toc d)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(defun generate-ids (library-form)
  (eval library-form)
  (let ((dir (merge-pathnames (format nil "docsets-test-~D/" (random 1000000 (make-random-state t)))
                              (uiop:temporary-directory))))
    (unwind-protect
         (let ((d (docset:open-docset (docsets:generate (second library-form) :output-directory dir))))
           (values (mapcar #'docset:record-id (docset:docset-index d)) (docset:docset-toc d)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(define-test re-exports
  :parent docsets/tests
  ;; Gadgets re-exports two widget functions: documented once, under widgets
  (is equal '("variable/*widgets*" "function/frob" "function/make-widget" "class/widget" "macro/with-widget"
              "function/gadgets:polish")
      (generate-ids '(docsets:define-library "docsets-test-both" :system "docsets/tests"
                      :packages ("DOCSETS-TEST-WIDGETS" ("DOCSETS-TEST-GADGETS" :prefix "gadgets:"))))))

(define-test group-by-home-package
  :parent docsets/tests
  ;; Alone, gadgets documents what it re-exports, grouped by where it comes from
  (multiple-value-bind (ids toc)
      (generate-ids '(docsets:define-library "docsets-test-gadgets" :system "docsets/tests"
                      :packages (("DOCSETS-TEST-GADGETS" :group-by-home-package t))))
    (is equal '("function/frob" "function/make-widget" "function/polish") ids)
    (is equal '((:title "docsets-test-gadgets" :children ((:title "Functions" :children ((:title "polish" :entry "function/polish")))))
                (:title "docsets-test-widgets" :children ((:title "Functions" :children ((:title "frob" :entry "function/frob")
                                                                                         (:title "make-widget" :entry "function/make-widget"))))))
        toc)))

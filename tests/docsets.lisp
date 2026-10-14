;;;; docsets.lisp — the docstring, Markdown and Texinfo readers, and a whole docset
;;;; written from the test package below, the pages in tests/pages/ and the
;;;; manual in tests/manual.texinfo.

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

(define-test readme-markdown
  :parent docsets/tests
  (let ((docsets::*page-links* (make-hash-table :test 'equal))
        (docsets::*link-names* (make-hash-table :test 'equal))
        (docsets::*params* '()))
    ;; Badges (an image inside a link) are dropped; links around text stay
    (is equal '((:p "Docs " (:link "http://d" "here")))
        (docsets:markdown-blocks '("[![Build](http://b.svg)](http://ci) Docs [here](http://d)")))
    (is equal '((:p "b")) (docsets:markdown-blocks '("[![Build](http://b.svg)](http://ci)" "" "b")))
    (is equal '("set " (:code "dex:*proxy*") " first") (docsets:markdown-inlines "set ```dex:*proxy*``` first"))
    ;; Reference-style links, defined on a line of their own anywhere
    (is equal '((:p "A " (:link "https://r" "RFC") " reader.") (:p "b"))
        (docsets:markdown-blocks '("A [RFC][rfc] reader." "" "[RFC]: https://r" "" "b")))
    ;; A line with ``` at both ends is a paragraph, not a fence; comments vanish
    (is equal '((:p "a " (:code "x")) (:p "b"))
        (docsets:markdown-blocks '("a ```x```" "<!-- - `force-string` -->" "" "b"))))
  ;; Docstrings quoting `like this' (Emacs's style)
  (let ((file (merge-pathnames "tests/readme.md" (asdf:system-source-directory "docsets"))))
    (is equal '((:heading 1 "What") (:p "Frobs."))
        (docsets::readme-blocks file '("widgets")))
    (is equal '((:p "Frobs."))
        (docsets::readme-blocks file '("what"))))
  (let ((docsets::*params* '("in")) (docsets::*link-names* (make-hash-table :test 'equal)))
    (is equal '((:p "Reads " (:param "in") ", or a " (:code "span") "; don't use " (:code "quotes") " here."))
        (docsets:docstring-blocks "Reads `in', or a `span'; don't use `quotes` here."))))

(define-test texinfo
  :parent docsets/tests
  (let ((docsets::*params* '("x"))
        (docsets::*link-names* (make-hash-table :test 'equal))
        (docsets::*node-links* (make-hash-table :test 'equalp)))
    (setf (gethash "frob" docsets::*link-names*) "function/frob"
          (gethash "Frobbing" docsets::*node-links*) "page/frobbing")
    (is equal '((:param "x") " is " (:ref "function/frob" "frob") "ed, " (:code "(f " (:param "y") ")") " ⇒ " (:i "it"))
        (docsets::texinfo-inlines "@var{x} is   @code{frob}ed, @code{(f @var{y})} @result{} @emph{it}"))
    (is equal '("See " (:ref "page/frobbing" "how") "; " (:link "http://x" "X") " “quoted” @{}")
        (docsets::texinfo-inlines "@xref{Frobbing,,how}; @uref{http://x,X} ``quoted'' @@@{@}"))
    (is equal '((:p "a") (:p "b") (:code-block "lisp" "(f) ⇒ 1"))
        (docsets::texinfo-blocks '("a @*" "b" "" "@lisp" "(f) @result{} 1" "@end lisp")))
    (is equal '((:list (:item (:p "One.")) (:item (:p "Two."))))
        (docsets::texinfo-blocks '("@itemize" "@item" "One." "@item Two." "@end itemize")))
    (is equal '(:syntax :kind :standard-generic-function :name ("frob") :args ("x " (:code "&key") " " "y") :values ("z"))
        (docsets::definition-syntax "deffn" "{Generic Function} frob x &key y @result{} z")))
  (eval '(docsets:define-library "docsets-test-texinfo"
          :system "docsets/tests" :title "Widgets"
          :texinfo "tests/manual.texinfo"
          :packages ("DOCSETS-TEST-WIDGETS")))
  (let ((dir (merge-pathnames (format nil "docsets-test-~D/" (random 1000000 (make-random-state t)))
                              (uiop:temporary-directory))))
    (unwind-protect
         (let* ((dir (docsets:generate "docsets-test-texinfo" :output-directory dir))
                (d (docset:open-docset dir))
                (chapter (docset:docset-entry d "page/widgets"))
                (with-widget (docset:docset-entry d "macro/with-widget"))
                (make-widget (docset:docset-entry d "function/make-widget")))
           (is = 0 (docset:check-docset dir :stream (make-broadcast-stream)))
           ;; Top and the index are left out; the class is documented by its @deftp
           (is equal '("page/widgets" "page/making-widgets" "class/widget" "macro/with-widget" "function/make-widget"
                       "variable/*widgets*" "function/frob")
               (mapcar #'docset:record-id (docset:docset-index d)))
           (is equal '((:title "Widgets" :entry "page/widgets"
                        :children ((:title "Making widgets" :entry "page/making-widgets"
                                    :children ((:title "widget" :entry "class/widget")))
                                   (:title "with-widget" :entry "macro/with-widget")
                                   (:title "make-widget" :entry "function/make-widget")))
                       (:title "Other variables" :children ((:title "*widgets*" :entry "variable/*widgets*")))
                       (:title "Other functions" :children ((:title "frob" :entry "function/frob"))))
               (docset:docset-toc d))
           ;; A chapter: macros expanded, footnotes as notes, @xref and @menu as links
           (let ((blocks (docset:entry-section chapter "")))
             (is equal '(:p "WD widgets are things to frob." (:sup "1") " See " (:ref "page/making-widgets" "Making widgets")
                         ", to start; Luís says “frob them”.")
                 (first blocks))
             (is equal '(:list (:item (:p (:ref "page/making-widgets" "Making widgets") " — How.")))
                 (third blocks))
             (is equal '(:heading 3 "Dictionary") (fourth blocks))
             (is equal '(:p (:sup "1") " " "Gently.") (car (last blocks))))
           ;; A definition, as a Markdown page's
           (is equal '((:syntax :kind :macro :name ("with-widget") :args ("(var " (:code "&key") " " "count) " (:code "&body") " " "body")
                        :values ("result")))
               (docset:entry-section with-widget "Syntax"))
           (is equal '((:p (:param "var") "—a symbol.") (:p (:param "count") ", " (:param "body") "—forms, evaluated."))
               (docset:entry-section with-widget "Arguments and Values"))
           (is equal '((:p "Binds " (:param "var") " to a widget from " (:ref "function/make-widget" "make-widget") " around "
                        (:param "body") ".")
                       (:p "It is pushed on " (:ref "variable/*widgets*" "*widgets*") "."))
               (docset:entry-section with-widget "Description"))
           (is equal '((:code-block "lisp" "(with-widget (w) w) ⇒ #<WIDGET>")) (docset:entry-section with-widget "Examples"))
           (is equal '((:p (:ref "function/make-widget" "make-widget"))) (docset:entry-section with-widget "See Also"))
           ;; An empty Description is the docstring's, before the Examples
           (is equal '("Syntax" "Description" "Examples") (mapcar #'car (getf (cddr make-widget) :sections)))
           (is equal '((:p "Makes a " (:ref "class/widget" "widget") ".")) (docset:entry-section make-widget "Description")))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(defun rewrite-manifest (dir function)
  "Rewrites DIR's manifest as FUNCTION returns it, given the manifest."
  (let* ((file (merge-pathnames "manifest.sexp" dir))
         (manifest (with-open-file (in file) (let ((*package* (find-package :keyword))) (read in)))))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (with-standard-io-syntax (prin1 (funcall function manifest) out)))))

(define-test versions
  :parent docsets/tests
  (is equal "dexador-20260101-git"
      (docsets::quicklisp-release #p"/home/q/quicklisp/dists/quicklisp/software/dexador-20260101-git/"))
  (is equal "jzon-v1.1.4"
      (docsets::quicklisp-release #p"/home/q/quicklisp/dists/quicklisp/software/jzon-v1.1.4/src/"))
  (false (docsets::quicklisp-release #p"/home/q/projects/lisp/gtk4/"))
  (false (docsets:library-release "no-such-system-anywhere"))
  ;; The fingerprint follows the library's definition file
  (let ((a (merge-pathnames "tests/pages/_index.md" (asdf:system-source-directory "docsets")))
        (b (merge-pathnames "tests/readme.md" (asdf:system-source-directory "docsets"))))
    (is equal (docsets:generator-fingerprint (docsets::make-library :file a))
        (docsets:generator-fingerprint (docsets::make-library :file a)))
    (isnt equal (docsets:generator-fingerprint (docsets::make-library :file a))
          (docsets:generator-fingerprint (docsets::make-library :file b))))
  ;; A docset is current when made, and out of date when what it records differs
  (eval '(docsets:define-library "docsets-test-versions" :system "docsets/tests"
          :packages ("DOCSETS-TEST-GADGETS")))
  (let* ((root (merge-pathnames (format nil "docsets-test-~D/" (random 1000000 (make-random-state t)))
                                (uiop:temporary-directory)))
         (dir (merge-pathnames "docsets-test-versions/" root)))
    (unwind-protect
         (flet ((status () (getf (docsets:docset-status dir) :status)))
           (docsets:generate "docsets-test-versions" :output-directory dir)
           (is eq :current (status))
           (rewrite-manifest dir (lambda (m) (setf (getf m :generator) "0000000000000000") m))
           (is eq :generator-changed (status))
           (rewrite-manifest dir (lambda (m) (setf (getf m :release) "docsets-old") m))
           (is eq :library-changed (status))
           (rewrite-manifest dir (lambda (m) (remf m :release) m))
           (is eq :unrecorded (status))
           (is = 1 (docsets:report-outdated :directory root :stream (make-broadcast-stream)))
           (is equal (list (truename dir)) (mapcar #'truename (docsets:update :directory root)))
           (is eq :current (status))
           (is = 0 (docsets:report-outdated :directory root :stream (make-broadcast-stream))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

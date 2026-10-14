;;;; generate.lisp — a library's docset, from its definition in libraries/
;;;;
;;;; A library is documented package by package. A package's entries come
;;;; from its documentation pages when it has HyperSpec-style ones (see
;;;; markdown.lisp), and from the docstrings of its external symbols for
;;;; anything the pages don't cover.

(in-package #:docsets)

;;; Libraries

(defstruct library
  id system title subtitle source license attribution packages readme)

(defvar *libraries* (make-hash-table :test 'equal)
  "Library id -> its LIBRARY, as libraries/*.lisp define them.")

(defmacro define-library (id &key system title subtitle source license attribution packages readme)
  "Defines how to document the library ID. PACKAGES lists the packages to
document, each a name or (name &key nicknames prefix pages toc-title see-also
group-by-home-package):
NICKNAMES are other names code uses for it; PREFIX starts its entries' names
in ids, to tell apart packages exporting the same names; PAGES is a directory
of HyperSpec-style Markdown pages, relative to the system's; TOC-TITLE heads
its part of the contents; SEE-ALSO names a package whose entries of the same
name each entry links to; GROUP-BY-HOME-PACKAGE groups the contents by the
package each symbol comes from, for a package that re-exports others (UIOP).
A symbol another of the packages exports from its own home is documented
there, not again. README, a Markdown file relative to the system's directory,
becomes an Overview page at the top of the contents, for a library whose
README documents more than its docstrings do."
  `(setf (gethash ,id *libraries*)
         (make-library :id ,id :system ,(or system id) :title ,(or title id) :subtitle ,subtitle
                       :source ,source :license ,license :attribution ,attribution
                       :packages ',packages :readme ,readme)))

(defun libraries-directory ()
  (asdf:system-relative-pathname "docsets" "libraries/"))

(defun load-libraries ()
  "Loads every library definition in libraries/."
  (dolist (file (directory (merge-pathnames "*.lisp" (libraries-directory))))
    (let ((*package* (find-package '#:docsets)))
      (load file))))

(defun library-names ()
  (load-libraries)
  (sort (loop for id being the hash-keys of *libraries* collect id) #'string<))

(defun find-library (id)
  "ID's definition, or, for a library with none, one documenting the package of
the same name from docstrings."
  (or (gethash id *libraries*)
      (progn (load-libraries) (gethash id *libraries*))
      (make-library :id id :system id :title id :packages (list (string-upcase id)))))

(defun package-option (spec key &optional default)
  (if (consp spec) (getf (rest spec) key default) default))

(defun package-spec-name (spec) (if (consp spec) (first spec) spec))

;;; Planning: which entries a package has, before any is written

(defstruct item
  id names kind kind-name source (weight 0) dictionary
  home)                                 ; a symbol's home package name, for :group-by-home-package

(defun prefixed (prefix name) (format nil "~@[~a~]~a" prefix name))

(defun page-items (directory prefix)
  "The items of the pages under DIRECTORY: an overview from its _index.md, a
dictionary from each subdirectory's _index.md, and a definition from every
other page."
  (let ((items '()))
    (dolist (file (directory (merge-pathnames "**/*.md" directory)))
      (multiple-value-bind (title weight lines) (read-page file)
        (let* ((slug (pathname-name file))
               (dir (car (last (pathname-directory file))))
               (top (equal (pathname-directory file) (pathname-directory directory))))
          (cond ((and (string= slug "_index") top)
                 (push (make-item :id (prefixed prefix "page/overview") :names (list "Overview")
                                  :kind :page :kind-name "Overview" :source (list :page lines) :weight -1)
                       items))
                ((string= slug "_index")
                 (let ((name (string-trim " " (if (search " dictionary" title)
                                                  (subseq title 0 (search " dictionary" title))
                                                  title))))
                   (push (make-item :id (prefixed prefix (format nil "page/~a" dir)) :names (list name)
                                    :kind :page :kind-name "Dictionary" :source (list :dictionary lines dir)
                                    :weight weight)
                         items)
                   (setf (gethash dir *page-links*) (item-id (first items)))))
                (title
                 (multiple-value-bind (kind kind-name names) (parse-title title)
                   (when names
                     (push (make-item :id (format nil "~(~a~)/~a" kind (prefixed prefix (first names)))
                                      :names names :kind kind :kind-name kind-name
                                      :source (list :page lines) :weight weight
                                      :dictionary (unless top dir))
                           items)
                     (unless (gethash slug *page-links*)
                       (setf (gethash slug *page-links*) (item-id (first items)))))))))))
    (sort items #'< :key #'item-weight)))

(defun symbol-items (package prefix covered &optional skip-packages)
  "The items of PACKAGE's external symbols whose names COVERED doesn't hold.
Symbols re-exported from COMMON-LISP or from SKIP-PACKAGES (the library's
other documented packages, which have their own entries) are left out."
  (let ((symbols '())
        (skip (cons (find-package '#:common-lisp) (mapcar #'find-package skip-packages))))
    (do-external-symbols (s package)
      (unless (member (symbol-package s) skip) (push s symbols)))
    (loop for symbol in (sort symbols #'string< :key #'symbol-name)
          for name = (string-downcase (symbol-name symbol))
          unless (gethash name covered)
            append (loop for definition in (symbol-definitions symbol)
                         collect (make-item :id (format nil "~(~a~)/~a" (getf definition :kind) (prefixed prefix name))
                                            :names (list name)
                                            :kind (getf definition :kind)
                                            :kind-name (getf definition :kind-name)
                                            :home (package-name (symbol-package symbol))
                                            :source (list :symbol symbol definition))))))

;;; Writing a package's entries

(defun item-entry (item package-name)
  (destructuring-bind (type &rest args) (item-source item)
    (ecase type
      (:symbol (definition-entry (first args) (second args) (item-id item) package-name))
      (:page (if (eq (item-kind item) :page)
                 (let ((*self-id* (item-id item)) (*params* '()))
                   (let ((blocks (markdown-blocks (first args))))
                     (list* :entry (item-id item) :names (item-names item) :kind :page
                            :kind-name (item-kind-name item)
                            (append (let ((s (summary blocks))) (when s (list :summary s)))
                                    (list :sections (list (cons "" blocks)))))))
                 (page-entry (item-id item) (item-kind item) (item-kind-name item) (item-names item)
                             package-name (first args))))
      (:dictionary
       (let ((*self-id* (item-id item)) (*params* '()))
         (list :entry (item-id item) :names (item-names item) :kind :page :kind-name "Dictionary"
               :summary (format nil "The ~a dictionary." (first (item-names item)))
               :sections (list (cons "" (markdown-blocks (first args))))))))))

(defun add-see-also (entry links label)
  "ENTRY with a See Also link to the entry LINKS names by its name, if any."
  (let ((id (gethash (first (getf (cddr entry) :names)) links)))
    (if (null id)
        entry
        (let* ((plist (copy-list (cddr entry)))
               (sections (getf plist :sections))
               (link (list :p (list :ref id (format nil "~a:~a" label (first (getf plist :names)))))))
          (setf (getf plist :sections)
                (if (assoc "See Also" sections :test #'string=)
                    (mapcar (lambda (s) (if (string= (car s) "See Also") (append s (list link)) s)) sections)
                    (append sections (list (list "See Also" link)))))
          (list* :entry (second entry) plist)))))

(defun plural (kind-name)
  (cond ((string= kind-name "Class") "Classes")
        ((char-equal (char kind-name (1- (length kind-name))) #\s) (format nil "~aes" kind-name))
        (t (format nil "~as" kind-name))))

(defun item-title (item) (format nil "~{~a~^, ~}" (item-names item)))

(defun kind-nodes (items &optional other)
  "Nodes for ITEMS by kind, in the order the kinds first appear. With OTHER,
titled as the entries not in a dictionary (\"Other functions\")."
  (let ((kinds '()))
    (dolist (i items) (pushnew (item-kind-name i) kinds :test #'string=))
    (loop for kind in (reverse kinds)
          collect (list :title (if other (format nil "Other ~(~a~)" (plural kind)) (plural kind))
                        :children (loop for i in items
                                        when (string= (item-kind-name i) kind)
                                          collect (list :title (item-title i) :entry (item-id i)))))))

(defun home-package-nodes (items)
  "Nodes for ITEMS by the package each symbol comes from, each by kind."
  (let ((homes '()))
    (dolist (i items) (pushnew (item-home i) homes :test #'equal))
    (loop for home in (sort homes #'string<)
          collect (list :title (string-downcase home)
                        :children (kind-nodes (remove home items :key #'item-home :test-not #'equal))))))

(defun package-toc (items &key group-by-home-package)
  "Contents of a package: its overview and dictionaries (each with its pages)
when it has pages, then its other entries by kind, or by the package they come
from and then by kind."
  (let ((dictionaries (remove-if-not (lambda (i) (eq (car (item-source i)) :dictionary)) items))
        (overview (find "Overview" items :key #'item-kind-name :test #'string=))
        (rest (remove-if (lambda (i) (or (item-dictionary i) (eq (item-kind i) :page))) items)))
    (append
     (when overview (list (list :title "Overview" :entry (item-id overview))))
     (loop for d in dictionaries
           for dir = (third (item-source d))
           collect (list :title (first (item-names d)) :entry (item-id d)
                         :children (loop for i in items
                                         when (equal (item-dictionary i) dir)
                                           collect (list :title (item-title i) :entry (item-id i)))))
     (if group-by-home-package
         (home-package-nodes rest)
         (kind-nodes rest dictionaries)))))

;;; Generating

(defun load-library-system (system)
  (if (find-package '#:ql)
      (uiop:symbol-call '#:ql '#:quickload system :silent t)
      (asdf:load-system system)))

(defun default-output-directory (id)
  (merge-pathnames (format nil "~a/" id) (docset:user-docset-directory)))

(defun generate (id &key (output-directory (default-output-directory id)))
  "Loads library ID's system and writes its docset to OUTPUT-DIRECTORY,
by default ~/.local/share/docsets/ID/. Returns the directory."
  (let* ((library (find-library id))
         (system (library-system library))
         (entries '()) (toc '()) (packages '()) (nicknames '())
         (package-links (make-hash-table :test 'equal)))   ; package name -> its *link-names*
    (load-library-system system)
    (let ((source-directory (asdf:system-source-directory system))
          (specs (library-packages library)))
      (dolist (spec specs)
        (let* ((name (package-spec-name spec))
               (package (or (find-package name) (error "~A doesn't define the package ~A." system name)))
               (prefix (package-option spec :prefix))
               (pages (package-option spec :pages))
               (*link-names* (make-hash-table :test 'equal))
               (*page-links* (make-hash-table :test 'equal))
               (covered (make-hash-table :test 'equal))
               (items (when pages (page-items (merge-pathnames pages source-directory) prefix))))
          ;; Records name the package as Lisp does; the others are nicknames
          (push (package-name package) packages)
          (let ((others (remove (package-name package)
                                (remove-duplicates (list* name (append (package-option spec :nicknames)
                                                                      (package-nicknames package)))
                                                   :test #'string=)
                                :test #'string=)))
            (when others (push (cons (package-name package) others) nicknames)))
          (dolist (i items)
            (unless (eq (item-kind i) :page)
              (dolist (n (item-names i)) (setf (gethash n covered) t))))
          (setf items (append items (symbol-items package prefix covered
                                                  (remove name (mapcar #'package-spec-name specs)
                                                          :test #'string=))))
          (dolist (i items)
            (unless (eq (item-kind i) :page)
              (dolist (n (item-names i))
                (unless (gethash n *link-names*) (setf (gethash n *link-names*) (item-id i))))))
          (let ((see-also (package-option spec :see-also)))
            (dolist (i items)
              (let ((entry (item-entry i (package-name package))))
                (when (and see-also (not (eq (item-kind i) :page)))
                  (let ((links (gethash see-also package-links)))
                    (when links
                      (setf entry (add-see-also entry links (string-downcase
                                                             (or (first (package-option (find see-also specs :key #'package-spec-name :test #'string=) :nicknames))
                                                                 see-also)))))))
                (push entry entries))))
          (setf (gethash name package-links) *link-names*)
          (let ((nodes (package-toc items :group-by-home-package
                                    (package-option spec :group-by-home-package))))
            (if (rest specs)
                (push (list :title (or (package-option spec :toc-title) name) :children nodes) toc)
                (setf toc nodes)))))
      (setf toc (if (rest specs) (reverse toc) toc))
      (let ((readme (library-readme library)))
        (when readme
          (let* ((*link-names* (or (gethash (package-spec-name (first specs)) package-links)
                                   (make-hash-table :test 'equal)))
                 (*page-links* (make-hash-table :test 'equal))
                 (*params* '())
                 (*self-id* "page/readme")
                 (blocks (readme-blocks (merge-pathnames readme source-directory))))
            ;; ENTRIES is reversed below: the overview goes first
            (setf entries (append entries
                                  (list (list* :entry *self-id* :names (list "Overview") :kind :page
                                               :kind-name "Overview"
                                               (append (let ((s (summary blocks))) (when s (list :summary s)))
                                                       (list :sections (list (cons "" blocks))))))))
            (push (list :title "Overview" :entry *self-id*) toc))))
      (docset:write-docset
       output-directory
       (list :id (library-id library)
             :title (library-title library)
             :subtitle (library-subtitle library)
             :version (asdf:component-version (asdf:find-system system))
             :system system
             :packages (reverse packages)
             :nicknames (reverse nicknames)
             :source (library-source library)
             :attribution (library-attribution library)
             :license (library-license library)
             :toc toc)
       (reverse entries)))))

(defun generate-all ()
  "Writes the docset of every library in libraries/. Returns their directories."
  (mapcar #'generate (library-names)))

;;;; symbols.lisp — what a symbol names, and its entries from docstrings
;;;;
;;;; SBCL's introspection gives each definition's kind and lambda list; the
;;;; docstring gives the description.

(in-package #:docsets)

;;; Definitions

(defun class-kind (class)
  (cond ((subtypep class (find-class 'condition)) (values :condition-type "Condition Type"))
        ((typep class 'structure-class) (values :structure "Structure"))
        ((typep class 'built-in-class) (values :system-class "System Class"))
        (t (values :class "Class"))))

(defun symbol-definitions (symbol)
  "What SYMBOL names, as plists (:kind :kind-name :doc :lambda-list :class),
in the order functions, variables, classes and types."
  (let ((out '()))
    (flet ((add (&rest plist) (push plist out)))
      (cond ((special-operator-p symbol))
            ((macro-function symbol)
             (add :kind :macro :kind-name "Macro" :doc (documentation symbol 'function)
                  :lambda-list (sb-introspect:function-lambda-list symbol)))
            ((fboundp symbol)
             (let ((f (fdefinition symbol)))
               (multiple-value-bind (kind kind-name)
                   (cond ((typep f 'standard-generic-function)
                          (values :standard-generic-function "Standard Generic Function"))
                         ((fboundp (list 'setf symbol)) (values :accessor "Accessor"))
                         (t (values :function "Function")))
                 (add :kind kind :kind-name kind-name :doc (documentation symbol 'function)
                      :lambda-list (if (typep f 'generic-function)
                                       (sb-mop:generic-function-lambda-list f)
                                       (sb-introspect:function-lambda-list symbol)))))))
      (case (sb-cltl2:variable-information symbol)
        ((:special :global)
         (add :kind :variable :kind-name "Variable" :doc (documentation symbol 'variable)))
        (:constant
         (add :kind :constant-variable :kind-name "Constant Variable" :doc (documentation symbol 'variable))))
      (let ((class (find-class symbol nil)))
        (cond (class
               (multiple-value-bind (kind kind-name) (class-kind class)
                 (add :kind kind :kind-name kind-name :doc (documentation symbol 'type) :class class)))
              ((eq (sb-int:info :type :kind symbol) :defined)
               (add :kind :type :kind-name "Type" :doc (documentation symbol 'type))))))
    (nreverse out)))

;;; Lambda lists

(defun form-text (form)
  "FORM as code, its symbols without package prefixes."
  (cond ((keywordp form) (format nil ":~(~a~)" (symbol-name form)))
        ((symbolp form) (string-downcase (symbol-name form)))
        ((consp form)
         (with-output-to-string (out)
           (write-char #\( out)
           (loop for (x . rest) on form
                 do (write-string (form-text x) out)
                    (cond ((consp rest) (write-char #\Space out))
                          (rest (format out " . ~a" (form-text rest)))))
           (write-char #\) out)))
        (t (let ((*print-case* :downcase)) (prin1-to-string form)))))

(defun flatten (tree)
  (cond ((null tree) '())
        ((atom tree) (list tree))
        (t (append (flatten (car tree)) (flatten (cdr tree))))))

(defun lambda-list-inlines (lambda-list)
  "LAMBDA-LIST as the :args of a :syntax block: lambda-list keywords as code."
  (let ((out '()))
    (loop for (x . rest) on lambda-list
          do (cond ((member x lambda-list-keywords) (push (list :code (form-text x)) out))
                   ;; A destructuring pattern, with lambda-list keywords of its own
                   ((and (consp x) (intersection (flatten x) lambda-list-keywords))
                    (push "(" out)
                    (dolist (y (lambda-list-inlines x)) (push y out))
                    (push ")" out))
                   (t (push (form-text x) out)))
             (cond ((consp rest) (push " " out))
                   (rest (push " . " out) (push (form-text rest) out))))
    ;; "name " rather than "name" " ", as the Common Lisp docset writes them
    (let ((merged '()))
      (dolist (x (nreverse out) (nreverse merged))
        (if (and (stringp x) (stringp (first merged)))
            (setf (first merged) (concatenate 'string (first merged) x))
            (push x merged))))))

(defun lambda-list-params (lambda-list)
  "The lowercase names of LAMBDA-LIST's parameters, nested ones included."
  (let ((out '()))
    (labels ((var (x)
               (when (and x (symbolp x) (not (member x lambda-list-keywords)))
                 (push (string-downcase (symbol-name x)) out)))
             (walk (list)
               (let ((required t))
                 (loop for (x . rest) on list
                       do (cond ((member x lambda-list-keywords)
                                 (setf required (member x '(&whole &environment &rest &body))))
                                ((symbolp x) (var x))
                                ;; A required parameter's list is a destructuring pattern
                                (required (walk x))
                                ;; (var default supplied-p), or ((:key var) default supplied-p)
                                ((consp (car x))
                                 (if (keywordp (caar x)) (var (cadar x)) (walk (car x)))
                                 (var (third x)))
                                (t (var (car x)) (var (third x))))
                       when (and rest (atom rest)) do (var rest)))))
      (walk lambda-list))
    (nreverse out)))

;;; Entries

(defun class-precedence-inlines (class)
  (ignore-errors (sb-mop:finalize-inheritance class))
  (let ((names (loop for c in (sb-mop:class-precedence-list class)
                     for name = (class-name c)
                     unless (member name '(sb-pcl::slot-object))
                       collect (string-downcase (symbol-name name)))))
    (loop for (name . rest) on names
          collect (let ((id (gethash name *link-names*)))
                    (if id (list :ref id name) (list :code name)))
          when rest collect ", ")))

(defun definition-entry (symbol definition id package-name)
  "The docset entry for DEFINITION, one of SYMBOL-DEFINITIONS' plists."
  (destructuring-bind (&key kind kind-name doc (lambda-list nil lambda-list-p) class) definition
    (let* ((name (string-downcase (symbol-name symbol)))
           (*params* (lambda-list-params lambda-list))
           (description (and doc (docstring-blocks doc)))
           (sections
             (append
              (when lambda-list-p
                (list (list "Syntax" (list :syntax :kind kind :name (list name)
                                                   :args (lambda-list-inlines lambda-list)))))
              (when class
                (list (list "Class Precedence List" (cons :p (class-precedence-inlines class)))))
              (when description
                (list (cons "Description" description))))))
      (list* :entry id :names (list name) :kind kind :kind-name kind-name :package package-name
             (append (let ((s (summary description))) (when s (list :summary s)))
                     (list :sections sections))))))

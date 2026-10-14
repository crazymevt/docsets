;;;; text.lisp — docstrings as docset blocks and inlines
;;;;
;;;; Docstrings are plain text by convention: paragraphs between blank lines,
;;;; "- " lists, indented code, and symbols written in uppercase. Uppercase
;;;; names become parameters, links to entries of the docset, or code.

(in-package #:docsets)

;;; What names link to, while an entry is written

(defvar *link-names* (make-hash-table :test 'equal)
  "Lowercase symbol name -> id of its entry, for the package being written.")

(defvar *params* '()
  "Lowercase parameter names of the entry being written.")

(defparameter *prose-words*
  '("a" "an" "and" "or" "not" "if" "do" "go" "is" "it" "in" "no" "of" "on" "to" "as" "at"
    "be" "by" "api" "apiv1" "apiv2" "smp" "os" "id" "ok" "ansi" "cl" "lisp" "mop" "io")
  "Uppercase words that are usually emphasis or acronyms, not symbols.")

(defun name-inline (word)
  "WORD, a symbol name as written in uppercase in prose, as an inline, or nil
when it is just a word."
  (let ((name (string-downcase word)))
    (cond ((member name *params* :test #'string=) (list :param name))
          ((gethash name *link-names*) (list :ref (gethash name *link-names*) name))
          ((member name '("nil" "t") :test #'string=) (list :code name))
          ((member name *prose-words* :test #'string=) nil)
          ((char= (char name 0) #\:) (list :code name))
          ((multiple-value-bind (symbol status) (find-symbol word "COMMON-LISP")
             (declare (ignore symbol))
             (and (eq status :external) (> (length name) 2)))
           (list :code name)))))

(defun symbol-char-p (c)
  (or (alphanumericp c) (find c "*+-%&<>=/!?$:")))

(defun uppercase-name-p (word)
  (and (some #'alpha-char-p word)
       (notany #'lower-case-p word)
       (alpha-char-p (find-if-not (lambda (c) (find c "*+%&:")) word))))

(defun emacs-quote-end (text start)
  "Where the quote closing `name' (Emacs's style) that opens at START is, if it is one."
  (let ((quote (position #\' text :start (1+ start)))
        (backquote (position #\` text :start (1+ start))))
    (and quote (> quote (1+ start))
         (or (null backquote) (< quote backquote))
         (notany (lambda (c) (member c '(#\Space #\Tab #\Newline))) (subseq text (1+ start) quote))
         quote)))

(defun text-inlines (text)
  "TEXT, a line of prose, as inlines: `code` (or `code' as Emacs writes it), and
uppercase symbol names."
  (let ((out '()) (plain (make-string-output-stream)) (i 0) (n (length text)))
    (flet ((flush ()
             (let ((s (get-output-stream-string plain)))
               (when (plusp (length s))
                 (if (stringp (first out))
                     (setf (first out) (concatenate 'string (first out) s))
                     (push s out))))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((and (char= c #\`) (or (emacs-quote-end text i) (position #\` text :start (1+ i))))
                        (let ((end (or (emacs-quote-end text i) (position #\` text :start (1+ i)))))
                          (flush)
                          (let* ((code (subseq text (1+ i) end))
                                 (param (find (string-downcase code) *params* :test #'string=)))
                            (push (if param (list :param param) (list :code code)) out))
                          (setf i (1+ end))))
                       ((and (symbol-char-p c) (or (zerop i) (not (symbol-char-p (char text (1- i))))))
                        (let* ((end (or (position-if-not #'symbol-char-p text :start i) n))
                               (word (string-right-trim "-:" (subseq text i end)))
                               (end (+ i (length word)))
                               (inline (and (plusp (length word)) (uppercase-name-p word) (name-inline word))))
                          (cond (inline (flush) (push inline out))
                                (t (write-string text plain :start i :end (max end (1+ i)))))
                          (setf i (max end (1+ i)))))
                       (t (write-char c plain) (incf i)))))
      (flush))
    (nreverse out)))

;;; Docstrings

(defun split-lines (string)
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline string :start start)
        collect (string-right-trim '(#\Space #\Tab #\Return) (subseq string start end))
        while end))

(defun blank-line-p (line) (zerop (length (string-trim " " line))))

(defun indentation (line) (or (position #\Space line :test-not #'char=) (length line)))

(defun dedent (lines)
  "LINES without the indentation they all share. A docstring's first line
starts right after the quote, so it is trimmed on its own."
  (let* ((rest (rest lines))
         (indent (loop for line in rest
                       unless (blank-line-p line) minimize (indentation line))))
    (cons (string-left-trim " " (first lines))
          (mapcar (lambda (line) (if (blank-line-p line) "" (subseq line (min (or indent 0) (length line)))))
                  rest))))

(defun bullet-line-p (line)
  (and (>= (length line) 2) (find (char line 0) "-*") (char= (char line 1) #\Space)))

(defun join-lines (lines)
  (format nil "~{~a~^ ~}" (mapcar (lambda (l) (string-trim " " l)) lines)))

(defun group-blocks (lines)
  "Blocks of one group of LINES (no blank lines among them)."
  (if (every (lambda (l) (plusp (indentation l))) lines)
      (list (list :code-block "lisp" (format nil "~{~a~^~%~}" (rest (dedent (cons "" lines))))))
      (let ((blocks '()) (paragraph '()) (items '()) (item nil))
        (flet ((end-paragraph ()
                 (when paragraph
                   (push (cons :p (text-inlines (join-lines (reverse paragraph)))) blocks)
                   (setf paragraph '())))
               (end-item ()
                 (when item
                   (push (list :item (cons :p (text-inlines (join-lines (reverse item))))) items)
                   (setf item nil)))
               (end-list ()
                 (when items
                   (push (cons :list (reverse items)) blocks)
                   (setf items '()))))
          (dolist (line lines)
            (cond ((bullet-line-p line)
                   (end-paragraph) (end-item)
                   (setf item (list (subseq line 2))))
                  ((and item (plusp (indentation line)))
                   (push line item))
                  (t (end-item) (end-list)
                     (push line paragraph))))
          (end-paragraph) (end-item) (end-list)
          (reverse blocks)))))

(defun merge-lists (blocks)
  "BLOCKS with lists that follow one another made one."
  (let ((out '()))
    (dolist (b blocks (nreverse out))
      (if (and (eq (car b) :list) (eq (car (first out)) :list))
          (setf (first out) (append (first out) (rest b)))
          (push b out)))))

(defun docstring-blocks (docstring)
  "DOCSTRING as docset blocks."
  (let ((groups '()) (group '()))
    (dolist (line (dedent (split-lines docstring)))
      (cond ((blank-line-p line) (when group (push (reverse group) groups) (setf group '())))
            (t (push line group))))
    (when group (push (reverse group) groups))
    (merge-lists (loop for g in (reverse groups) append (group-blocks g)))))

;;; Summaries

(defun plain-text (inlines)
  (with-output-to-string (out)
    (labels ((walk (x)
               (cond ((stringp x) (write-string x out))
                     ((member (car x) '(:ref :term :link :secref)) (mapc #'walk (cddr x)))
                     ((keywordp (car x)) (mapc #'walk (cdr x))))))
      (mapc #'walk inlines))))

(defun summary (blocks)
  "The first sentence of the first paragraph of BLOCKS, or nil."
  (let ((p (find :p blocks :key #'car)))
    (when p
      (let* ((text (plain-text (rest p)))
             (end (loop for i from 0 below (1- (length text))
                        when (and (char= (char text i) #\.) (char= (char text (1+ i)) #\Space)
                                  (> i 10))
                          return (1+ i))))
        (string-trim " " (subseq text 0 (min (length text) (or end (length text)) 300)))))))

;;;; markdown.lisp — HyperSpec-style Markdown pages as docset entries
;;;;
;;;; Some libraries document their API as a page per definition, laid out
;;;; like the standard: a title such as "Function ACQUIRE-LOCK, RELEASE-LOCK",
;;;; then "#### Syntax:", "#### Arguments and values:", "#### Description:" and
;;;; so on. bordeaux-threads' site is written this way (Hugo, with front
;;;; matter). This reads such pages; anything they don't cover comes from
;;;; docstrings instead.

(in-package #:docsets)

(defvar *page-links* (make-hash-table :test 'equal)
  "A page's file name without its type -> the id of its entry, for relative links.")

(defvar *self-id* nil
  "The id of the entry being written, for links to \".\".")

;;; Pages

(defun read-page (pathname)
  "The page in PATHNAME: its front matter's title and weight, and its body's lines."
  (let* ((lines (with-open-file (in pathname :external-format :utf-8)
                  (loop for line = (read-line in nil) while line
                        collect (string-right-trim '(#\Return) line))))
         (title nil) (weight 0))
    (when (equal (first lines) "---")
      (let ((end (position "---" lines :start 1 :test #'string=)))
        (dolist (line (subseq lines 1 end))
          (let ((colon (position #\: line)))
            (when colon
              (let ((key (string-trim " " (subseq line 0 colon)))
                    (value (string-trim " '\"" (subseq line (1+ colon)))))
                (cond ((string= key "title") (setf title value))
                      ((string= key "weight") (setf weight (or (parse-integer value :junk-allowed t) 0))))))))
        (setf lines (subseq lines (1+ end)))))
    (values title weight lines)))

(defun parse-title (title)
  "TITLE, such as \"Function: ACQUIRE-LOCK, RELEASE-LOCK\" or \"Host type NATIVE-LOCK\",
as the kind keyword, the kind's name, and the lowercase names documented. The
kind is the words with lowercase letters; the names are the uppercase rest."
  (let* ((words (loop for start = 0 then (1+ end)
                      for end = (position #\Space title :start start)
                      for word = (subseq title start end)
                      when (plusp (length word)) collect word
                      while end))
         (split (or (position-if-not (lambda (w) (some #'lower-case-p w)) words) (length words)))
         (kind-words (mapcar (lambda (w) (string-right-trim ":" w)) (subseq words 0 split)))
         (names (format nil "~{~a~^ ~}" (subseq words split))))
    (values (intern (string-upcase (format nil "~{~a~^-~}" kind-words)) :keyword)
            (format nil "~{~:(~a~)~^ ~}" kind-words)
            (loop for name in (loop for start = 0 then (1+ end)
                                    for end = (position #\, names :start start)
                                    collect (string-trim " " (subseq names start end))
                                    while end)
                  when (plusp (length name))
                    collect (string-downcase (remove #\\ name))))))

;;; Inlines

(defun link-target (url)
  "URL as (:ref id) or (:link url), or nil when it is a relative link to no page."
  (cond ((search "://" url) (list :link url))
        ((member url '("." "./") :test #'string=) (and *self-id* (list :ref *self-id*)))
        (t (let* ((path (string-right-trim "/" (subseq url 0 (position #\# url))))
                  (slug (subseq path (1+ (or (position #\/ path :from-end t) -1))))
                  ;; A page's file name, or failing that a symbol's name: page names have typos
                  (id (or (gethash slug *page-links*) (gethash slug *link-names*))))
             (and id (list :ref id))))))

(defun find-closing (string delimiter start)
  "Where DELIMITER next occurs in STRING after START, not escaped by a backslash."
  (loop for at = (search delimiter string :start2 start)
        while at
        do (if (and (plusp at) (char= (char string (1- at)) #\\))
               (setf start (1+ at))
               (return at))))

(defun markdown-inlines (string)
  "A line of Markdown as inlines: `code`, **bold**, *italic*, [links](url) and
backslash escapes. Code naming a parameter of the entry is a :param."
  (let ((out '()) (plain (make-string-output-stream)) (i 0) (n (length string)))
    (flet ((flush ()
             (let ((s (get-output-stream-string plain)))
               (when (plusp (length s)) (push s out)))))
      (loop while (< i n)
            do (let ((c (char string i)))
                 (cond ((and (char= c #\\) (< (1+ i) n))
                        (write-char (char string (1+ i)) plain) (incf i 2))
                       ((and (char= c #\`) (find-closing string "`" (1+ i)))
                        (let* ((end (find-closing string "`" (1+ i)))
                               (code (subseq string (1+ i) end))
                               (param (find (string-downcase code) *params* :test #'string=)))
                          (flush)
                          (push (if param (list :param param) (list :code code)) out)
                          (setf i (1+ end))))
                       ((and (char= c #\*) (< (1+ i) n) (char= (char string (1+ i)) #\*)
                             (find-closing string "**" (+ i 2)))
                        (let ((end (find-closing string "**" (+ i 2))))
                          (flush)
                          (push (cons :b (markdown-inlines (subseq string (+ i 2) end))) out)
                          (setf i (+ end 2))))
                       ((and (char= c #\*) (find-closing string "*" (1+ i)))
                        (let ((end (find-closing string "*" (1+ i))))
                          (flush)
                          (push (cons :i (markdown-inlines (subseq string (1+ i) end))) out)
                          (setf i (1+ end))))
                       ((and (char= c #\[) (find-closing string "](" (1+ i)))
                        (let* ((mid (find-closing string "](" (1+ i)))
                               (end (position #\) string :start mid)))
                          (if (null end)
                              (progn (write-char c plain) (incf i))
                              (let ((text (markdown-inlines (subseq string (1+ i) mid)))
                                    (target (link-target (subseq string (+ mid 2) end))))
                                (flush)
                                (if target
                                    (push (append target text) out)
                                    (setf out (append (reverse text) out)))
                                (setf i (1+ end))))))
                       (t (write-char c plain) (incf i)))))
      (flush))
    (nreverse out)))

;;; Blocks

(defun hard-break-groups (lines)
  "LINES split after each line ending in a backslash, Markdown's line break."
  (let ((groups '()) (group '()))
    (dolist (line lines)
      (let ((break (and (plusp (length line)) (char= (char line (1- (length line))) #\\))))
        (push (if break (subseq line 0 (1- (length line))) line) group)
        (when break (push (reverse group) groups) (setf group '()))))
    (when group (push (reverse group) groups))
    (reverse groups)))

(defun heading-level (line)
  (let ((n (or (position #\# line :test-not #'char=) 0)))
    (and (plusp n) (< n (length line)) (char= (char line n) #\Space) n)))

(defun markdown-blocks (lines)
  "LINES of Markdown as blocks: paragraphs (one per hard line break), \"- \"
lists, fenced code, and headings. Headings that are only a link, as the index
pages of a dictionary are written, become a list of links."
  (let ((blocks '()) (paragraph '()) (items '()) (item nil) (code nil) (in-code nil))
    (labels ((end-paragraph ()
               (when paragraph
                 (dolist (group (hard-break-groups (reverse paragraph)))
                   (push (cons :p (markdown-inlines (join-lines group))) blocks))
                 (setf paragraph '())))
             (end-item ()
               (when item
                 (push (list :item (cons :p (markdown-inlines (join-lines (reverse item))))) items)
                 (setf item nil)))
             (end-list ()
               (end-item)
               (when items
                 (push (cons :list (reverse items)) blocks)
                 (setf items '())))
             (end-all () (end-paragraph) (end-list)))
      (dolist (line lines)
        (cond (in-code
               (if (string= (string-trim " " line) "```")
                   (progn (push (list :code-block "lisp"
                                      (string-right-trim '(#\Newline #\Space)
                                                         (format nil "~{~a~^~%~}" (reverse code))))
                                blocks)
                          (setf in-code nil code '()))
                   (push line code)))
              ((and (>= (length line) 3) (string= "```" line :end2 3))
               (end-all) (setf in-code t code '()))
              ((heading-level line)
               (let* ((level (heading-level line))
                      (inlines (markdown-inlines (string-trim " " (subseq line level)))))
                 (cond ((and (= (length inlines) 1) (consp (first inlines))
                             (member (car (first inlines)) '(:ref :link)))
                        (end-paragraph) (end-item)
                        (push (list :item (cons :p inlines)) items))
                       (t (end-all) (push (list* :heading (min level 4) inlines) blocks)))))
              ((blank-line-p line) (end-paragraph) (end-item))
              ((bullet-line-p line)
               (end-paragraph) (end-item)
               (setf item (list (subseq line 2))))
              ((and item (plusp (indentation line))) (push line item))
              (t (end-list) (push line paragraph))))
      (end-all)
      (merge-lists (reverse blocks)))))

;;; Sections of a definition's page

(defun section-label (heading)
  "\"Arguments and values:\" as \"Arguments and Values\", the standard's casing."
  (let ((words (loop for start = 0 then (1+ end)
                     for end = (position #\Space heading :start start)
                     for word = (subseq heading start end)
                     when (plusp (length word)) collect word
                     while end)))
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (w) (if (member w '("and" "or" "of") :test #'string-equal) (string-downcase w) (string-capitalize w)))
                    (mapcar (lambda (w) (string-right-trim ":" w)) words)))))

(defun split-sections (lines)
  "LINES as (label . lines) by their \"#### \" headings; lines before the first are labelled \"\"."
  (let ((sections '()) (label "") (body '()))
    (dolist (line lines)
      (if (eql (heading-level line) 4)
          (progn (when (or body (plusp (length label))) (push (cons label (reverse body)) sections))
                 (setf label (section-label (subseq line 5)) body '()))
          (push line body)))
    (push (cons label (reverse body)) sections)
    (remove-if (lambda (s) (and (string= (car s) "") (every #'blank-line-p (cdr s))))
               (reverse sections))))

(defun syntax-line (line kind)
  "A line of a Syntax section, \"**acquire-lock** lock &key (wait t) => boolean\", as a :syntax block."
  (let* ((text (string-trim " " (remove #\\ line)))
         (start (search "**" text))
         (end (and start (search "**" text :start2 (+ start 2)))))
    (when end
      (let* ((name (subseq text (+ start 2) end))
             (rest (subseq text (+ end 2)))
             (arrow (search "=>" rest))
             (args (string-trim " " (subseq rest 0 arrow)))
             (values (and arrow (string-trim " " (subseq rest (+ arrow 2))))))
        (flet ((parts (string)
                 (let ((words (loop for s = 0 then (1+ e)
                                    for e = (position #\Space string :start s)
                                    for w = (string-trim "*" (subseq string s e))
                                    when (plusp (length w)) collect w
                                    while e)))
                   (loop for (w . more) on words
                         collect (if (char= (char w 0) #\&) (list :code w) (if more (format nil "~a " w) w))
                         when (and more (char= (char w 0) #\&)) collect " "))))
          (list* :syntax :kind kind :name (list name) :args (parts args)
                 (when values (list :values (parts values)))))))))

(defun syntax-params (lines)
  "The parameter names in a Syntax section's LINES."
  (let ((out '()))
    (dolist (line lines (remove-duplicates (nreverse out) :test #'string=))
      (let* ((text (remove #\\ line))
             (end (search "**" text :start2 (1+ (or (search "**" text) -1))))
             (rest (if end (subseq text (+ end 2) (search "=>" text)) "")))
        (loop for s = 0 then (1+ e)
              for e = (position #\Space rest :start s)
              for w = (string-trim "*()" (subseq rest s e))
              when (and (plusp (length w)) (alpha-char-p (char w 0))
                        (not (member w '("t" "nil") :test #'string-equal)))
                do (push (string-downcase w) out)
              while e)))))

(defun argument-paragraph (lines)
  "\"*lock* -> a lock object.\" as (:p (:param \"lock\") \"—a lock object.\")."
  (let* ((text (join-lines lines))
         (arrow (search "->" text)))
    (if (and arrow (char= (char text 0) #\*))
        (let ((name (string-trim "* " (subseq text 0 arrow))))
          (list* :p (list :param name)
                 (let ((rest (markdown-inlines (string-trim " " (subseq text (+ arrow 2))))))
                   (if (stringp (first rest))
                       (cons (format nil "—~a" (first rest)) (rest rest))
                       (cons "—" rest)))))
        (cons :p (markdown-inlines text)))))

(defun section-blocks (label lines kind)
  (cond ((string= label "Syntax")
         (or (loop for group in (hard-break-groups (remove-if #'blank-line-p lines))
                   for block = (syntax-line (join-lines group) kind)
                   when block collect block)
             (markdown-blocks lines)))
        ((string= label "Arguments and Values")
         (loop for group in (hard-break-groups (remove-if #'blank-line-p lines))
               collect (argument-paragraph group)))
        (t (markdown-blocks lines))))

(defun page-entry (id kind kind-name names package lines)
  "The entry of a definition's page: NAMES of KIND in PACKAGE, from the page's LINES."
  (let* ((sections (split-sections lines))
         (*self-id* id)
         (*params* (syntax-params (cdr (assoc "Syntax" sections :test #'string=))))
         (sections (loop for (label . body) in sections
                         for blocks = (section-blocks label body kind)
                         when blocks collect (cons label blocks)))
         (summary (summary (or (cdr (assoc "Description" sections :test #'string=))
                               (cdr (first sections))))))
    (list* :entry id :names names :kind kind :kind-name kind-name :package package
           (append (when summary (list :summary summary))
                   (list :sections sections)))))

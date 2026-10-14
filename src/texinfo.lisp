;;;; texinfo.lisp — a Texinfo manual as docset entries
;;;;
;;;; Some libraries document their API in a Texinfo manual: a node per
;;;; definition, laid out like the standard, with @subheading Syntax,
;;;; Arguments and Values, Description, Examples and See Also (CFFI's is
;;;; written this way), among chapters of tutorial and concepts. This reads
;;;; such a manual: a definition's node becomes its entry, as a Markdown page
;;;; would (see markdown.lisp), and every other node a page, nested in the
;;;; contents as the manual's chapters and sections are.

(in-package #:docsets)

(defvar *node-links* (make-hash-table :test 'equalp)
  "A node's or anchor's name -> the id of its entry, for @ref and its kin.")

(defvar *aliases* (make-hash-table :test 'equal)
  "Command names the manual's @alias lines define -> the commands they stand for.")

(defvar *verbatim* nil
  "Whether text is code: no quotes or dashes made typographic, spaces kept.")

(defvar *footnotes* :none
  "The footnotes of the entry being written, as lists of inlines, last first,
or :none when footnotes are not collected.")

;;; Reading: conditionals, comments, macros

(defun at-escaped-p (string i)
  "Whether the character at I in STRING follows an @ that escapes it: an odd
run of @s before it."
  (oddp (loop for j downfrom (1- i) to 0 while (char= (char string j) #\@) count t)))

(defun join-continued-lines (text)
  "TEXT with each @ that ends a line, Texinfo's continuation, and the
indentation after it made one space."
  (with-output-to-string (out)
    (let ((i 0) (n (length text)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((and (char= c #\@) (< (1+ i) n) (char= (char text (1+ i)) #\@))
                        (write-string "@@" out) (incf i 2))
                       ((and (char= c #\@) (< (1+ i) n) (char= (char text (1+ i)) #\Newline))
                        (write-char #\Space out)
                        (setf i (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text :start (+ i 2)) n)))
                       (t (write-char c out) (incf i))))))))

(defun strip-comment (line)
  "LINE without its @c or @comment and what follows it."
  (loop for i from 0 below (length line)
        when (and (char= (char line i) #\@) (not (at-escaped-p line i))
                  (loop for word in '("c" "comment")
                        for end = (+ i 1 (length word))
                        thereis (and (<= end (length line))
                                     (string= word line :start2 (1+ i) :end2 end)
                                     (or (= end (length line)) (member (char line end) '(#\Space #\Tab))))))
          return (string-right-trim " " (subseq line 0 i))
        finally (return line)))

(defun line-command (line)
  "For a line that starts with a command, such as \"@end table\", the
command's name (resolved through *ALIASES*) and the rest of the line. A
command with braces, \"@code{x} is\", is text, not a line's command."
  (let ((line (string-left-trim '(#\Space #\Tab) line)))
    (when (and (> (length line) 1) (char= (char line 0) #\@) (alpha-char-p (char line 1)))
      (let ((end (or (position-if-not #'alphanumericp line :start 1) (length line))))
        (unless (and (< end (length line)) (char= (char line end) #\{))
          (let ((name (subseq line 1 end)))
            (values (gethash name *aliases* name)
                    (string-trim '(#\Space #\Tab) (subseq line end)))))))))

(defparameter *dropped-environments*
  '("iftex" "ifinfo" "ifplaintext" "ifdocbook" "ifxml" "ifnothtml" "tex" "titlepage"
    "copying" "direntry" "detailmenu" "ignore" "documentdescription")
  "Environments whose contents a docset leaves out: other output formats'
text, the title page, and the manual's own front matter.")

(defparameter *unwrapped-environments*
  '("ifnottex" "ifnotinfo" "ifhtml" "ifnotplaintext" "ifnotdocbook" "ifnotxml" "group")
  "Environments a docset keeps the contents of, without the environment.")

(defun word-list (string)
  (loop for start = 0 then (1+ end)
        for end = (position #\Space string :start start)
        for word = (subseq string start end)
        when (plusp (length word)) collect word
        while end))

(defun macro-definitions (lines)
  "LINES without the environments in *DROPPED-ENVIRONMENTS* and those in
*UNWRAPPED-ENVIRONMENTS*, and without @macro definitions and @alias lines,
which are returned too: an alist of (name params . body) and a hash table."
  (let ((out '()) (macros '()) (aliases (make-hash-table :test 'equal)))
    (loop while lines
          do (let ((line (pop lines)))
               (multiple-value-bind (command rest) (let ((*aliases* aliases)) (line-command line))
                 (cond ((member command *dropped-environments* :test #'string=)
                        ;; Up to the matching @end, counting nested ones of the same name
                        (loop with depth = 1
                              for l = (pop lines)
                              while l
                              do (multiple-value-bind (c r) (line-command l)
                                   (cond ((string= c command) (incf depth))
                                         ((and (equal c "end") (string= r command))
                                          (when (zerop (decf depth)) (return)))))))
                       ((or (member command *unwrapped-environments* :test #'string=)
                            (and (equal command "end") (member rest *unwrapped-environments* :test #'string=))))
                       ((member command '("macro" "rmacro") :test #'string=)
                        (let* ((brace (position #\{ rest))
                               (name (string-trim " " (subseq rest 0 brace)))
                               (params (and brace (mapcar (lambda (p) (string-trim " " p))
                                                          (split-on #\, (subseq rest (1+ brace) (position #\} rest)))))))
                          (push (list* name (remove "" params :test #'string=)
                                       (format nil "~{~a~^~%~}"
                                               (loop for l = (pop lines)
                                                     until (or (null l) (multiple-value-bind (c r) (line-command l)
                                                                          (and (equal c "end") (member r '("macro" "rmacro") :test #'string=))))
                                                     collect l)))
                                macros)))
                       ((equal command "alias")
                        (let ((= (position #\= rest)))
                          (when =
                            (setf (gethash (string-trim " " (subseq rest 0 =)) aliases)
                                  (string-trim " " (subseq rest (1+ =)))))))
                       (t (push line out))))))
    (values (nreverse out) macros aliases)))

(defun split-on (char string)
  "STRING split at each CHAR."
  (loop for start = 0 then (1+ end)
        for end = (position char string :start start)
        collect (subseq string start end)
        while end))

(defun brace-end (string start)
  "For the { at START in STRING, the position of the } that closes it."
  (loop with depth = 0
        for i from start below (length string)
        for c = (char string i)
        do (cond ((char= c #\@) (incf i))
                 ((char= c #\{) (incf depth))
                 ((char= c #\}) (when (zerop (decf depth)) (return i))))))

(defun split-arguments (string)
  "STRING, a command's braces' contents, split at its commas outside braces."
  (let ((args '()) (start 0) (depth 0))
    (loop for i from 0 below (length string)
          for c = (char string i)
          do (cond ((char= c #\@) (incf i))
                   ((char= c #\{) (incf depth))
                   ((char= c #\}) (decf depth))
                   ((and (char= c #\,) (zerop depth))
                    (push (subseq string start i) args) (setf start (1+ i)))))
    (push (subseq string start) args)
    (mapcar (lambda (a) (string-trim '(#\Space #\Tab #\Newline) a)) (nreverse args))))

(defun expand-macros (text macros)
  "TEXT with each call of one of MACROS, as MACRO-DEFINITIONS returns them,
replaced by its body, its \\param\\s replaced by the call's arguments."
  (if (null macros)
      text
      (let ((i 0))
        (loop
          (let ((at (position #\@ text :start i)))
            (when (null at) (return text))
            (let* ((end (or (position-if-not #'alphanumericp text :start (1+ at)) (length text)))
                   (macro (and (not (at-escaped-p text at)) (> end (1+ at))
                               (assoc (subseq text (1+ at) end) macros :test #'string=))))
              (if (null macro)
                  (setf i (if (and (< (1+ at) (length text)) (char= (char text (1+ at)) #\@)) (+ at 2) (1+ at)))
                  (destructuring-bind (name params . body) macro
                    (declare (ignore name))
                    (let* ((braces (and (< end (length text)) (char= (char text end) #\{) (brace-end text end)))
                           (args (cond ((null braces) '())
                                       ((rest params) (split-arguments (subseq text (1+ end) braces)))
                                       (t (list (string-trim '(#\Space #\Tab #\Newline) (subseq text (1+ end) braces))))))
                           (expansion (with-output-to-string (out)
                                        (loop with j = 0
                                              while (< j (length body))
                                              do (let ((c (char body j)))
                                                   (if (char= c #\\)
                                                       (let ((close (position #\\ body :start (1+ j))))
                                                         (if (and close (= close (1+ j)))
                                                             (write-char #\\ out)
                                                             (let ((k (position (subseq body (1+ j) close) params :test #'string=)))
                                                               (write-string (or (and k (nth k args)) "") out)))
                                                         (setf j (1+ (or close j))))
                                                       (progn (write-char c out) (incf j))))))))
                      ;; Expanded again from where it starts, for macros calling macros
                      (setf text (concatenate 'string (subseq text 0 at) expansion
                                              (subseq text (if braces (1+ braces) end)))
                            i at))))))))))

(defun texinfo-source-lines (pathname)
  "The lines of the Texinfo manual in PATHNAME, with continuation lines joined,
macros expanded, and comments and other formats' text left out. Returns its
@alias table too."
  (let ((text (join-continued-lines (uiop:read-file-string pathname :external-format :utf-8))))
    (multiple-value-bind (lines macros aliases)
        (macro-definitions (mapcar (lambda (l) (string-right-trim '(#\Return) l)) (split-on #\Newline text)))
      (let ((lines (split-on #\Newline (expand-macros (format nil "~{~a~^~%~}" lines) macros))))
        (values (loop for line in lines
                      for stripped = (strip-comment line)
                      ;; A line that was only a comment goes, rather than ending a paragraph
                      unless (and (string/= line stripped) (blank-line-p stripped))
                        collect stripped)
                aliases)))))

;;; Nodes

(defstruct node
  name title (level 1) command lines
  defs                                  ; a definition's (kind kind-name names params)
  anchors)

(defparameter *sectioning-levels*
  '(("top" . 0) ("chapter" . 1) ("appendix" . 1) ("unnumbered" . 1) ("chapheading" . 1) ("majorheading" . 1)
    ("section" . 2) ("appendixsec" . 2) ("unnumberedsec" . 2) ("heading" . 2)
    ("subsection" . 3) ("appendixsubsec" . 3) ("unnumberedsubsec" . 3) ("subheading" . 3)
    ("subsubsection" . 4) ("appendixsubsubsec" . 4) ("unnumberedsubsubsec" . 4) ("subsubheading" . 4))
  "Sectioning and heading commands, and how deep each is.")

(defun texinfo-nodes (lines)
  "LINES split into nodes, each titled by its first sectioning command."
  (let ((nodes '()) (node nil))
    (dolist (line lines)
      (multiple-value-bind (command rest) (line-command line)
        (cond ((equal command "node")
               (when node (push node nodes))
               (setf node (make-node :name (string-trim " " (first (split-on #\, rest))))))
              ((null node))
              ((and (null (node-title node)) (assoc command *sectioning-levels* :test #'equal))
               (setf (node-title node) rest
                     (node-command node) command
                     (node-level node) (cdr (assoc command *sectioning-levels* :test #'string=))))
              (t (push line (node-lines node))))))
    (when node (push node nodes))
    (dolist (n nodes)
      (setf (node-lines n) (reverse (node-lines n))
            (node-anchors n) (loop for line in (node-lines n)
                                   for at = (search "@anchor{" line)
                                   when at collect (subseq line (+ at 8) (position #\} line :start at)))))
    (nreverse nodes)))

(defun reference-node-p (node)
  "Whether NODE documents definitions, laid out like the standard: it has a
Syntax subheading."
  (some (lambda (line) (multiple-value-bind (c r) (line-command line)
                         (and (equal c "subheading") (string-equal r "Syntax"))))
        (node-lines node)))

;;; Definition lines: @deffn and its kin

(defparameter *definition-commands*
  '(("deffn") ("defvr") ("deftp") ("defop") ("deftypefn") ("deftypevr")
    ("defun" . "Function") ("defmac" . "Macro") ("defspec" . "Special Form")
    ("defvar" . "Variable") ("defopt" . "User Option"))
  "Commands defining something, and the category of those that don't take one.")

(defparameter *category-kinds*
  '(("Special Variable" :variable "Variable")
    ("Generic Function" :standard-generic-function "Standard Generic Function")
    ("Lisp Type" :type "Type"))
  "Texinfo's categories named as the docstring reader names the same kinds.")

(defun definition-command-p (command)
  (and command
       (assoc (if (and (> (length command) 1) (char= (char command (1- (length command))) #\x)
                       (assoc (subseq command 0 (1- (length command))) *definition-commands* :test #'string=))
                  (subseq command 0 (1- (length command)))
                  command)
              *definition-commands* :test #'string=)))

(defun next-word (string)
  "The first word of STRING, or its first {group}, and the rest of STRING."
  (let ((string (string-left-trim " " string)))
    (if (and (plusp (length string)) (char= (char string 0) #\{))
        (let ((end (or (brace-end string 0) (1- (length string)))))
          (values (subseq string 1 end) (string-left-trim " " (subseq string (1+ end)))))
        (let ((end (or (position #\Space string) (length string))))
          (values (subseq string 0 end) (string-left-trim " " (subseq string end)))))))

(defun parse-definition (command rest)
  "A definition line's category, name and arguments: for \"@deffn {Generic
Function} frob x\", \"Generic Function\", \"frob\" and \"x\"."
  (let* ((entry (definition-command-p command))
         (fixed (cdr entry)))
    (multiple-value-bind (category rest) (if fixed (values fixed rest) (next-word rest))
      (when (member (car entry) '("defop" "deftypefn" "deftypevr") :test #'string=)
        (setf rest (nth-value 1 (next-word rest))))   ; a class or a type
      (multiple-value-bind (name args) (next-word rest)
        (values category name args)))))

(defun category-kind (category)
  "CATEGORY, such as \"Generic Function\", as a kind keyword and a kind name."
  (let ((known (assoc category *category-kinds* :test #'string-equal)))
    (if known
        (values-list (rest known))
        (values (intern (string-upcase (substitute #\- #\Space category)) :keyword)
                (format nil "~{~:(~a~)~^ ~}" (word-list category))))))

(defun texinfo-text (string)
  "STRING, Texinfo, as plain text, as it reads in code."
  (let ((*verbatim* t) (*params* '()) (*link-names* (make-hash-table)) (*footnotes* :none))
    (plain-text (texinfo-inlines string))))

(defun syntax-parts (string)
  "STRING, arguments or values, as inlines of a :syntax block: lambda-list
keywords as code, as syntax-line in markdown.lisp writes them."
  (let ((words (word-list (substitute #\Space #\Tab (texinfo-text string)))))
    (loop for (w . more) on words
          collect (if (char= (char w 0) #\&) (list :code w) (if more (format nil "~a " w) w))
          when (and more (char= (char w 0) #\&)) collect " ")))

(defun definition-arrow (args)
  "Where ARGS, a definition's arguments, give way to its values: their @result{}."
  (let ((arrows (list (search "@result{}" args))))
    (maphash (lambda (alias command)
               (when (string= command "result") (push (search (format nil "@~a{}" alias) args) arrows)))
             *aliases*)
    (let ((found (remove nil arrows)))
      (and found (reduce #'min found)))))

(defun definition-syntax (command rest)
  "A definition line as a :syntax block."
  (multiple-value-bind (category name args) (parse-definition command rest)
    (let ((arrow (definition-arrow args)))
      (list* :syntax :kind (category-kind category) :name (list (texinfo-text name))
             :args (syntax-parts (subseq args 0 arrow))
             (when arrow
               (list :values (syntax-parts (subseq args (1+ (position #\} args :start arrow))))))))))

(defun definition-params (args)
  "The parameter names in ARGS, a definition line's arguments."
  (let ((arrow (definition-arrow args)))
    (loop for w in (word-list (texinfo-text (subseq args 0 arrow)))
          for name = (string-downcase (string-trim "()[]{}*|" w))
          when (and (plusp (length name)) (alpha-char-p (char name 0))
                    (not (member name '("t" "nil") :test #'string=)))
            collect name)))

(defun node-definitions (node)
  "The kind, kind name, names and parameters of the definitions on the
Syntax lines of NODE, a reference node; nil when it has none."
  (let ((defs (loop for line in (node-lines node)
                    for (command rest) = (multiple-value-list (line-command line))
                    when (definition-command-p command)
                      collect (multiple-value-list (parse-definition command rest)))))
    (when defs
      (multiple-value-bind (kind kind-name) (category-kind (first (first defs)))
        (list kind kind-name
              (remove-duplicates (mapcar (lambda (d) (string-downcase (texinfo-text (second d)))) defs)
                                 :test #'string= :from-end t)
              (remove-duplicates (loop for d in defs append (definition-params (third d)))
                                 :test #'string= :from-end t))))))

;;; Inlines

(defparameter *glyphs*
  '(("result" . "⇒") ("expansion" . "→") ("print" . "▷") ("error" . "error→") ("equiv" . "≡")
    ("point" . "∗") ("dots" . "…") ("enddots" . "….") ("bullet" . "•") ("minus" . "−")
    ("copyright" . "©") ("registeredsymbol" . "®") ("tie" . " ") ("comma" . ",") ("atchar" . "@")
    ("lbracechar" . "{") ("rbracechar" . "}") ("backslashchar" . "\\") ("hashchar" . "#")
    ("TeX" . "TeX") ("LaTeX" . "LaTeX") ("geq" . "≥") ("leq" . "≤") ("arrow" . "→")
    ("click" . "→") ("today" . "") ("ss" . "ß") ("o" . "ø") ("O" . "Ø") ("ae" . "æ") ("AE" . "Æ")
    ("l" . "ł") ("L" . "Ł") ("exclamdown" . "¡") ("questiondown" . "¿") ("pounds" . "£")
    ("euro" . "€") ("dotless" . "") ("ordf" . "ª") ("ordm" . "º"))
  "Commands that stand for a character or a word.")

(defparameter *accents*
  '((#\' . #\COMBINING_ACUTE_ACCENT) (#\` . #\COMBINING_GRAVE_ACCENT)
    (#\" . #\COMBINING_DIAERESIS) (#\^ . #\COMBINING_CIRCUMFLEX_ACCENT)
    (#\~ . #\COMBINING_TILDE) (#\= . #\COMBINING_MACRON) (#\, . #\COMBINING_CEDILLA))
  "Accent commands, @'e, and the combining characters they add.")

(defparameter *code-commands*
  '("code" "samp" "file" "command" "env" "option" "kbd" "key" "t" "verb" "indicateurl" "var-code"))

(defun merge-strings (inlines)
  "INLINES with strings that follow one another made one."
  (let ((out '()))
    (dolist (x inlines (nreverse out))
      (if (and (stringp x) (stringp (first out)))
          (setf (first out) (concatenate 'string (first out) x))
          (push x out)))))

(defun reference-inlines (command args)
  "@ref, @xref, @pxref (and @seealso, CFFI's alias of @ref) to a node, as a
link to its entry when it has one."
  (destructuring-bind (node &optional name title file manual &rest more) args
    (declare (ignore more))
    (let* ((label (find-if (lambda (s) (and s (plusp (length s)))) (list title name node)))
           (text (texinfo-inlines label))
           (id (and (null (find-if (lambda (s) (and s (plusp (length s)))) (list file manual)))
                    (gethash node *node-links*)))
           (link (if id (list (list* :ref id text)) text)))
      (cond ((string= command "xref") (cons "See " link))
            ((string= command "pxref") (cons "see " link))
            (t link)))))

(defun code-inline (inlines)
  "INLINES, the contents of @code, as a parameter, a link to the entry they
name, or code."
  (let ((s (and (= (length inlines) 1) (stringp (first inlines)) (first inlines))))
    (cond ((null s) (list* :code inlines))
          ((find (string-downcase s) *params* :test #'string=) (list :param (string-downcase s)))
          ((let ((id (gethash (string-downcase s) *link-names*)))
             (and id (not (equal id *self-id*)) (list :ref id s))))
          (t (list :code s)))))

(defun brace-command-inlines (name arg)
  "The inlines of the command NAME with braces holding ARG."
  (flet ((inner () (texinfo-inlines arg)))
    (cond ((assoc name *glyphs* :test #'string=)
           (let ((glyph (cdr (assoc name *glyphs* :test #'string=))))
             ;; @dotless{i} is i; the accent around it adds the rest
             (if (string= name "dotless") (list arg) (list glyph))))
          ((member name *code-commands* :test #'string=)
           (list (code-inline (let ((*verbatim* t)) (inner)))))
          ((string= name "var") (list (list :param (string-downcase (texinfo-text arg)))))
          ((member name '("emph" "i" "dfn" "cite" "slanted") :test #'string=) (list (cons :i (inner))))
          ((member name '("strong" "b") :test #'string=) (list (cons :b (inner))))
          ((member name '("sup" "sub") :test #'string=) (list (cons (if (string= name "sup") :sup :sub) (inner))))
          ((member name '("acronym" "abbr") :test #'string=) (texinfo-inlines (first (split-arguments arg))))
          ((member name '("uref" "url") :test #'string=)
           (destructuring-bind (url &optional text replacement) (split-arguments arg)
             (let ((label (find-if (lambda (s) (and s (plusp (length s)))) (list replacement text url))))
               (list (list* :link (texinfo-text url) (texinfo-inlines label))))))
          ((string= name "email")
           (destructuring-bind (address &optional text) (split-arguments arg)
             (texinfo-inlines (if (and text (plusp (length text))) text address))))
          ((member name '("ref" "xref" "pxref") :test #'string=) (reference-inlines name (split-arguments arg)))
          ((string= name "inforef") (texinfo-inlines (first (split-arguments arg))))
          ((string= name "footnote")
           (if (listp *footnotes*)
               (progn (push (trim-inlines (inner)) *footnotes*)
                      (list (list :sup (format nil "~d" (length *footnotes*)))))
               '()))
          ((member name '("anchor" "image" "cindex" "findex" "vindex" "kindex" "tindex" "pindex") :test #'string=) '())
          (t (inner)))))

(defun texinfo-inlines (string)
  "A line of Texinfo as inlines: @code, @var (a parameter), @emph, @strong,
@ref and @uref, glyphs such as @result{}, and footnotes, as superscript
numbers with the footnote kept in *FOOTNOTES*."
  (let ((out '()) (plain (make-string-output-stream)) (i 0) (n (length string)))
    (flet ((flush ()
             (let ((s (get-output-stream-string plain)))
               (when (plusp (length s)) (push s out))))
           (emit (inlines)
             (dolist (x inlines) (push x out))))
      (loop while (< i n)
            do (let ((c (char string i)))
                 (cond ((and (char= c #\@) (< (1+ i) n))
                        (let ((next (char string (1+ i))))
                          (cond ((find next "@{}.!?") (write-char next plain) (incf i 2))
                                ((find next ":-/*&") (incf i 2))
                                ((member next '(#\Space #\Tab #\Newline)) (write-char #\Space plain) (incf i 2))
                                ((assoc next *accents*)
                                 ;; @'e or @'{e}, and @dotless{i} in the braces
                                 (let* ((braced (and (< (+ i 2) n) (char= (char string (+ i 2)) #\{)))
                                        (end (if braced (or (brace-end string (+ i 2)) (1- n)) (min n (+ i 2))))
                                        (base (if braced (texinfo-text (subseq string (+ i 3) end))
                                                  (if (< (+ i 2) n) (string (char string (+ i 2))) ""))))
                                   (write-string (sb-unicode:normalize-string
                                                  (format nil "~a~c" base (cdr (assoc next *accents*))) :nfc)
                                                 plain)
                                   (setf i (if braced (1+ end) (+ i 3)))))
                                ((alpha-char-p next)
                                 (let* ((end (or (position-if-not #'alphanumericp string :start (1+ i)) n))
                                        (name (subseq string (1+ i) end))
                                        (name (gethash name *aliases* name)))
                                   (if (and (< end n) (char= (char string end) #\{) (brace-end string end))
                                       (let ((close (brace-end string end)))
                                         (flush)
                                         (emit (brace-command-inlines name (subseq string (1+ end) close)))
                                         (setf i (1+ close)))
                                       ;; A command without braces: a glyph, or nothing a docset shows
                                       (let ((glyph (assoc name *glyphs* :test #'string=)))
                                         (when glyph (write-string (cdr glyph) plain))
                                         (setf i end)))))
                                (t (write-char next plain) (incf i 2)))))
                       ((and (not *verbatim*) (member c '(#\Space #\Tab #\Newline)))
                        (write-char #\Space plain)
                        (setf i (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline))) string :start i) n)))
                       ((and (not *verbatim*) (< (1+ i) n) (string= "``" string :start2 i :end2 (+ i 2)))
                        (write-char #\LEFT_DOUBLE_QUOTATION_MARK plain) (incf i 2))
                       ((and (not *verbatim*) (< (1+ i) n) (string= "''" string :start2 i :end2 (+ i 2)))
                        (write-char #\RIGHT_DOUBLE_QUOTATION_MARK plain) (incf i 2))
                       ((and (not *verbatim*) (< (+ i 2) n) (string= "---" string :start2 i :end2 (+ i 3)))
                        (write-char #\EM_DASH plain) (incf i 3))
                       ((and (not *verbatim*) (< (1+ i) n) (string= "--" string :start2 i :end2 (+ i 2)))
                        (write-char #\EN_DASH plain) (incf i 2))
                       (t (write-char c plain) (incf i)))))
      (flush))
    (merge-strings (nreverse out))))

;;; Blocks

(defun split-hard-breaks (text)
  "TEXT split at each @*, Texinfo's line break."
  (let ((parts '()) (start 0))
    (loop for i from 0 below (length text)
          do (when (char= (char text i) #\@)
               (when (and (< (1+ i) (length text)) (char= (char text (1+ i)) #\*))
                 (push (subseq text start i) parts)
                 (setf start (+ i 2)))
               (incf i)))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun paragraph-blocks (lines)
  "LINES of a paragraph as :p blocks, one per line break."
  (loop for part in (split-hard-breaks (format nil "~{~a~^~%~}" lines))
        for inlines = (trim-inlines (texinfo-inlines part))
        when inlines collect (cons :p inlines)))

(defun code-language (text)
  "\"lisp\" for TEXT that looks like Lisp, else nil."
  (let ((start (position-if-not (lambda (c) (member c '(#\Space #\Newline))) text)))
    (when (and start (or (find (char text start) "(;'#") (search "> (" text)))
      "lisp")))

(defun code-block (lines)
  (let* ((text (let ((*verbatim* t)) (plain-text (texinfo-inlines (format nil "~{~a~^~%~}" lines)))))
         (text (string-trim '(#\Newline) text)))
    (list :code-block (code-language text) (string-right-trim '(#\Space #\Newline) text))))

(defparameter *heading-levels*
  '(("chapter" . 1) ("appendix" . 1) ("unnumbered" . 1) ("chapheading" . 1) ("majorheading" . 1)
    ("section" . 2) ("appendixsec" . 2) ("unnumberedsec" . 2) ("heading" . 2)
    ("subsection" . 3) ("appendixsubsec" . 3) ("unnumberedsubsec" . 3) ("subheading" . 3)
    ("subsubsection" . 4) ("appendixsubsubsec" . 4) ("unnumberedsubsubsec" . 4) ("subsubheading" . 4))
  "Sectioning commands inside a node, and the levels of the headings they become.")

(defparameter *skipped-commands*
  '("noindent" "indent" "page" "need" "sp" "vskip" "printindex" "insertcopying" "contents"
    "shortcontents" "summarycontents" "bye" "setfilename" "settitle" "syncodeindex" "synindex"
    "exampleindent" "documentencoding" "documentlanguage" "paragraphindent" "firstparagraphindent"
    "dircategory" "node" "top" "cindex" "findex" "vindex" "kindex" "tindex" "pindex" "refill"
    "frenchspacing" "setchapternewpage" "headings" "footnotestyle" "set" "clear" "author" "title"
    "subtitle")
  "Commands on lines of their own that a docset has no use for.")

(defun texinfo-blocks (lines)
  "LINES of Texinfo as blocks."
  (values (parse-blocks (coerce lines 'vector) 0 nil)))

(defun parse-blocks (lines i end &key stop-at-item initial)
  "The blocks of LINES from I up to the @end END (or the end of LINES), and
where they stop. With STOP-AT-ITEM, an @item ends them too, unconsumed.
INITIAL is the start of a first paragraph."
  (let ((blocks '()) (paragraph (reverse initial)) (n (length lines)))
    (flet ((end-paragraph ()
             (when paragraph
               (dolist (b (paragraph-blocks (reverse paragraph))) (push b blocks))
               (setf paragraph '()))))
      (loop
        (when (>= i n) (end-paragraph) (return))
        (let ((line (aref lines i)))
          (multiple-value-bind (command rest) (line-command line)
            (cond ((blank-line-p line) (end-paragraph) (incf i))
                  ((equal command "end")
                   (end-paragraph)
                   (cond ((and stop-at-item (string= rest end)) (return))   ; the list's, for it to read
                         ((or (null end) (string= rest end)) (return (incf i)))
                         (t (incf i))))
                  ((and stop-at-item (member command '("item" "itemx") :test #'string=))
                   (end-paragraph) (return))
                  ((member command '("lisp" "smalllisp" "example" "smallexample" "verbatim" "display"
                                     "smalldisplay" "format" "smallformat")
                           :test #'string=)
                   (end-paragraph)
                   (let ((code '()))
                     (loop for j from (1+ i) below n
                           for l = (aref lines j)
                           do (multiple-value-bind (c r) (line-command l)
                                (cond ((and (equal c "end") (string= r command)) (setf i (1+ j)) (return))
                                      ((member c '("group" "noindent") :test #'string=))
                                      ((and (equal c "end") (string= r "group")))
                                      (t (push l code))))
                           finally (setf i n))
                     (push (code-block (reverse code)) blocks)))
                  ((member command '("itemize" "enumerate") :test #'string=)
                   (end-paragraph)
                   (multiple-value-bind (block next) (parse-list lines (1+ i) command nil)
                     (when (rest block) (push block blocks))
                     (setf i next)))
                  ((member command '("table" "ftable" "vtable") :test #'string=)
                   (end-paragraph)
                   (multiple-value-bind (block next)
                       (parse-list lines (1+ i) command (string-left-trim "@" (first (word-list rest))))
                     (when (rest block) (push block blocks))
                     (setf i next)))
                  ((member command '("quotation" "smallquotation" "indentedblock" "smallindentedblock")
                           :test #'string=)
                   (end-paragraph)
                   (multiple-value-bind (inner next) (parse-blocks lines (1+ i) command)
                     (when inner (push (cons :quote inner) blocks))
                     (setf i next)))
                  ((equal command "cartouche")
                   (end-paragraph)
                   (multiple-value-bind (inner next) (parse-blocks lines (1+ i) command)
                     (dolist (b inner) (push b blocks))
                     (setf i next)))
                  ((equal command "menu")
                   (end-paragraph)
                   (multiple-value-bind (menu next) (parse-menu lines (1+ i))
                     (dolist (b menu) (push b blocks))
                     (setf i next)))
                  ((definition-command-p command)
                   (end-paragraph)
                   (push (definition-syntax command rest) blocks)
                   (incf i)
                   ;; @deffnx adds a line to its @deffn; the body follows up to @end deffn
                   (unless (char= (char command (1- (length command))) #\x)
                     (multiple-value-bind (inner next) (parse-blocks lines i command)
                       (dolist (b inner) (push b blocks))
                       (setf i next))))
                  ((assoc command *heading-levels* :test #'equal)
                   (end-paragraph)
                   (let ((inlines (trim-inlines (texinfo-inlines rest))))
                     (when inlines
                       (push (list* :heading (cdr (assoc command *heading-levels* :test #'string=)) inlines)
                             blocks)))
                   (incf i))
                  ((member command *skipped-commands* :test #'equal) (end-paragraph) (incf i))
                  (t (push line paragraph) (incf i)))))))
    (values (nreverse blocks) i)))

(defun parse-list (lines i command format)
  "The @itemize, @enumerate or @table (with FORMAT, the command its items'
labels are written in) starting at I, as a :list, and where it ends."
  (let ((items '()) (n (length lines)))
    (loop
      (when (>= i n) (return))
      (multiple-value-bind (c rest) (line-command (aref lines i))
        (cond ((and (equal c "end") (string= rest command)) (incf i) (return))
              ((equal c "item")
               (if format
                   ;; A table item: its labels (@item, then any @itemx), then its body
                   (let ((labels (list rest)))
                     (loop while (and (< (1+ i) n) (equal (line-command (aref lines (1+ i))) "itemx"))
                           do (incf i) (push (nth-value 1 (line-command (aref lines i))) labels))
                     (multiple-value-bind (body next) (parse-blocks lines (1+ i) command :stop-at-item t)
                       (push (list* :item
                                    (cons :label (loop for (label . more) on (reverse labels)
                                                       append (texinfo-inlines
                                                               (if (member format '("asis" "") :test #'string=)
                                                                   label
                                                                   (format nil "@~a{~a}" format label)))
                                                       when more collect ", "))
                                    body)
                             items)
                       (setf i next)))
                   (multiple-value-bind (body next)
                       (parse-blocks lines (1+ i) command :stop-at-item t
                                                          :initial (when (plusp (length rest)) (list rest)))
                     (push (cons :item body) items)
                     (setf i next))))
              ;; What comes before the first @item, such as blank lines
              (t (incf i)))))
    (values (cons :list (nreverse items)) i)))

(defun parse-menu (lines i)
  "The @menu starting at I as blocks: a list of links to its entries' nodes,
headed by the lines between them (\"Dictionary\"). Returns where it ends."
  (let ((blocks '()) (items '()) (n (length lines)))
    (flet ((end-list ()
             (when items (push (cons :list (nreverse items)) blocks) (setf items '()))))
      (loop
        (when (>= i n) (return))
        (let* ((line (aref lines i)) (command (line-command line)))
          (incf i)
          (cond ((and (equal command "end")) (return))
                ((and (> (length line) 2) (string= "* " line :end2 2))
                 (let* ((entry (subseq line 2))
                        (colons (search "::" entry))
                        (colon (position #\: entry))
                        ;; "* Node::  description" or "* Title: Node.  description"
                        (node (if colons (subseq entry 0 colons)
                                  (string-trim " " (subseq entry (1+ colon) (position-if (lambda (c) (find c ".,	")) entry :start (1+ colon))))))
                        (title (if colons node (subseq entry 0 colon)))
                        (after (if colons (+ colons 2)
                                   (1+ (or (position-if (lambda (c) (find c ".,	")) entry :start (1+ colon)) (1- (length entry))))))
                        (description (string-trim " " (subseq entry (min after (length entry))))))
                   ;; A description goes on over indented lines
                   (loop while (and (< i n) (plusp (length (aref lines i))) (member (char (aref lines i) 0) '(#\Space #\Tab)))
                         do (setf description (format nil "~a ~a" description (string-trim " " (aref lines i))))
                            (incf i))
                   (push (list :item (cons :p (trim-inlines
                                               (texinfo-inlines
                                                (format nil "@ref{~a,,~a}~:[ — ~a~;~]" node title
                                                        (zerop (length description)) description)))))
                         items)))
                ((blank-line-p line))
                (t (end-list)
                   (push (list* :heading 3 (texinfo-inlines (string-trim " " line))) blocks)))))
      (end-list))
    (values (nreverse blocks) i)))

;;; Entries

(defun section-lines (lines)
  "LINES as (label . lines) by their @subheading lines; lines before the first are labelled \"\"."
  (let ((sections '()) (label "") (body '()))
    (dolist (line lines)
      (multiple-value-bind (command rest) (line-command line)
        (if (equal command "subheading")
            (progn (when (or (plusp (length label)) (notevery #'blank-line-p body))
                     (push (cons label (reverse body)) sections))
                   (setf label (section-label (texinfo-text rest)) body '()))
            (push line body))))
    (push (cons label (reverse body)) sections)
    (reverse sections)))

(defun argument-blocks (blocks)
  "BLOCKS of an Arguments and Values section, its @table of parameters made
paragraphs as the Markdown reader writes them: \"ptr—a foreign pointer.\""
  (loop for b in blocks
        if (and (eq (car b) :list) (every (lambda (item) (eq (car (second item)) :label)) (rest b)))
          append (loop for item in (rest b)
                       for label = (rest (second item))
                       for body = (cddr item)
                       append (if (eq (car (first body)) :p)
                                  (let ((text (rest (first body))))
                                    (list* (append (list :p) label
                                                   (if (stringp (first text))
                                                       (cons (format nil "—~a" (lowercase-initial (first text))) (rest text))
                                                       (cons "—" text)))
                                           (rest body)))
                                  (cons (cons :p label) body)))
        else collect b))

(defun lowercase-initial (string)
  "STRING with its first letter lowercase when it starts a sentence, \"A
foreign type\", and not a name, \"NULL\"."
  (if (and (> (length string) 1) (upper-case-p (char string 0))
           (or (lower-case-p (char string 1)) (char= (char string 1) #\Space)))
      (format nil "~(~c~)~a" (char string 0) (subseq string 1))
      string))

(defun footnote-blocks ()
  (loop for note in (reverse *footnotes*)
        for k from 1
        collect (list* :p (list :sup (format nil "~d" k)) " " note)))

(defun with-docstring-description (sections names package)
  "SECTIONS with a Description from the docstring of one of NAMES in PACKAGE,
for a definition the manual describes no more than its syntax: before its
Examples, or whatever else follows the description."
  (let* ((doc (loop for name in names
                    for symbol = (find-symbol (string-upcase name) package)
                    thereis (and symbol (some (lambda (d) (getf d :doc)) (symbol-definitions symbol)))))
         (blocks (and doc (docstring-blocks doc))))
    (if (null blocks)
        sections
        (let ((at (or (position-if (lambda (s) (not (member (car s) '("" "Syntax" "Arguments and Values" "Value Type"
                                                                      "Initial Value" "Class Precedence List")
                                                            :test #'string=)))
                                   sections)
                      (length sections))))
          (append (subseq sections 0 at) (list (cons "Description" blocks)) (subseq sections at))))))

(defun texinfo-definition-entry (id node kind kind-name names package)
  "The entry of NODE, a reference node documenting NAMES of KIND in PACKAGE."
  (let* ((*self-id* id)
         (*params* (fourth (node-defs node)))
         (*footnotes* '())
         (sections (loop for (label . lines) in (section-lines (node-lines node))
                         for blocks = (texinfo-blocks lines)
                         for blocks* = (if (string= label "Arguments and Values") (argument-blocks blocks) blocks)
                         when blocks* collect (cons label blocks*)))
         (sections (if (assoc "Description" sections :test #'string=)
                       sections
                       (with-docstring-description sections names package)))
         (sections (if *footnotes* (append sections (list (cons "Notes" (footnote-blocks)))) sections))
         (summary (summary (or (cdr (assoc "Description" sections :test #'string=))
                               (cdr (first sections))))))
    (list* :entry id :names names :kind kind :kind-name kind-name :package package
           (append (when summary (list :summary summary))
                   (list :sections sections)))))

(defun texinfo-page-entry (id node kind-name)
  "The entry of NODE, a chapter or section of the manual, as a page."
  (let* ((*self-id* id)
         (*params* '())
         (*footnotes* '())
         (blocks (texinfo-blocks (node-lines node)))
         (blocks (if *footnotes*
                     (append blocks (list (list :heading 2 "Notes")) (footnote-blocks))
                     blocks)))
    (list* :entry id :names (list (texinfo-text (node-title node))) :kind :page :kind-name kind-name
           (append (let ((s (summary blocks))) (when s (list :summary s)))
                   (list :sections (list (cons "" blocks)))))))

;;; Planning

(defun node-slug (name)
  (string-trim "-" (with-output-to-string (out)
                     (loop for c across (string-downcase name)
                           with dash = nil
                           do (if (alphanumericp c)
                                  (progn (when dash (write-char #\- out)) (setf dash nil) (write-char c out))
                                  (setf dash t))))))

(defun page-definition-nodes (node package)
  "For each definition of an external symbol of PACKAGE in NODE, a page
(\"@deftp {Lisp Type} foreign-pointer\" and the text up to its @end), a node
of its own, laid out as a reference node is."
  (loop for (line . more) on (node-lines node)
        for (command rest) = (multiple-value-list (line-command line))
        when (and (definition-command-p command)
                  (multiple-value-bind (symbol status)
                      (find-symbol (string-upcase (texinfo-text (nth-value 1 (parse-definition command rest)))) package)
                    (and symbol (eq status :external))))
          collect (let ((definition (make-node :name (node-name node)
                                               :lines (list* "@subheading Syntax" line "@subheading Description"
                                                             (loop for l in more
                                                                   until (multiple-value-bind (c r) (line-command l)
                                                                           (and (equal c "end") (string= r command)))
                                                                   collect l)))))
                    (setf (node-defs definition) (node-definitions definition))
                    definition)))

(defun texinfo-items (pathname prefix package)
  "The items of the Texinfo manual in PATHNAME: an entry for each node laid
out like the standard (see REFERENCE-NODE-P), and a page for each other
node, each under the node it belongs to. A definition of one of PACKAGE's
symbols in a page has an entry too, under it, unless a node of its own
documents it. The manual's top node and its index are left out."
  (multiple-value-bind (lines aliases) (texinfo-source-lines pathname)
    (let* ((*aliases* aliases)
           (nodes (remove-if (lambda (node)
                               (or (member (node-command node) '(nil "top") :test #'equal)
                                   (some (lambda (l) (equal (line-command l) "printindex")) (node-lines node))))
                             (texinfo-nodes lines)))
           (links (make-hash-table :test 'equalp))
           (stack (make-array 6 :initial-element nil))
           (items '()))
      (dolist (node nodes)
        (let* ((defs (and (reference-node-p node) (node-definitions node)))
               (title (texinfo-text (node-title node)))
               (level (node-level node))
               (item (if defs
                         (destructuring-bind (kind kind-name names params) defs
                           (declare (ignore params))
                           (setf (node-defs node) defs)
                           (make-item :id (format nil "~(~a~)/~a" kind (prefixed prefix (first names)))
                                      :names names :kind kind :kind-name kind-name))
                         (make-item :id (prefixed prefix (format nil "page/~a" (node-slug (node-name node))))
                                    :names (list title) :kind :page
                                    :kind-name (cond ((string= (node-command node) "appendix") "Appendix")
                                                     ((= level 1) "Chapter")
                                                     (t "Section"))))))
          (setf (item-source item) (list :texinfo node links aliases)
                (item-parent item) (let ((parent (find-if #'identity stack :end (min level (length stack)) :from-end t)))
                                     (and parent (item-id parent))))
          ;; A definition is no parent: what follows it belongs to its chapter
          (unless defs
            (setf (aref stack (min level 5)) item)
            (fill stack nil :start (min (1+ level) 6)))
          (dolist (name (cons (node-name node) (node-anchors node)))
            (setf (gethash name links) (item-id item)))
          (push item items)
          (unless defs
            (dolist (definition (page-definition-nodes node package))
              (destructuring-bind (kind kind-name names params) (node-defs definition)
                (declare (ignore params))
                (push (make-item :id (format nil "~(~a~)/~a" kind (prefixed prefix (first names)))
                                 :names names :kind kind :kind-name kind-name :parent (item-id item)
                                 :source (list :texinfo definition links aliases))
                      items))))))
      ;; Without the definitions in pages that have nodes of their own
      (let ((documented (make-hash-table :test 'equal)))
        (dolist (i items)
          (unless (or (eq (item-kind i) :page) (item-page-definition-p i))
            (dolist (n (item-names i)) (setf (gethash n documented) t))))
        (remove-if (lambda (i) (and (item-page-definition-p i) (some (lambda (n) (gethash n documented)) (item-names i))))
                   (nreverse items))))))

(defun item-page-definition-p (item)
  "Whether ITEM is a definition in a page, from PAGE-DEFINITION-NODES."
  (and (not (eq (item-kind item) :page))
       (null (node-title (second (item-source item))))))

(defun texinfo-entry (item package-name)
  "ITEM's entry, an item of TEXINFO-ITEMS."
  (destructuring-bind (node links aliases) (rest (item-source item))
    (let ((*node-links* links) (*aliases* aliases))
      (if (eq (item-kind item) :page)
          (texinfo-page-entry (item-id item) node (item-kind-name item))
          (texinfo-definition-entry (item-id item) node (item-kind item) (item-kind-name item)
                                    (item-names item) package-name)))))

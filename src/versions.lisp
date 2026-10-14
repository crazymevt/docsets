;;;; versions.lisp — what a docset was made from, and whether that has changed
;;;;
;;;; Each docset's manifest records the library release it was made from
;;;; (:release) and a fingerprint of the generator that made it (:generator).
;;;; A docset is out of date when either differs from what is installed now:
;;;; a newer release of the library (after ql:update-dist, say), or a change
;;;; to this generator, the docset library or the library's definition here.
;;;; Nothing here reaches the network; updating the libraries is up to you.

(in-package #:docsets)

;;; The library's release

(defun quicklisp-release (directory)
  "For a directory in a Quicklisp dist, its release: the directory under
software/, such as \"dexador-20260101-git\"."
  (let ((parts (pathname-directory directory)))
    (loop for (part next) on parts
          when (and (equal part "software") next (member "dists" parts :test #'equal))
            return next)))

(defun git-commit (directory)
  "The commit checked out in the git work tree holding DIRECTORY, or nil."
  (let ((commit (ignore-errors
                 (uiop:run-program (list "git" "-C" (uiop:native-namestring directory)
                                         "rev-parse" "--short=12" "HEAD")
                                   :output '(:string :stripped t) :error-output nil))))
    (and commit (plusp (length commit)) commit)))

(defun library-release (system)
  "The release of SYSTEM installed now: its Quicklisp release, else its git
commit, else its directory's name. A system built into the Lisp (UIOP in
SBCL) has the Lisp's version. Nil when SYSTEM isn't installed."
  (let ((found (ignore-errors (asdf:find-system system nil))))
    (when found
      (let ((directory (ignore-errors (truename (asdf:system-source-directory found)))))
        (cond ((null directory)
               (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version)))
              ((quicklisp-release directory))
              ((let ((commit (git-commit directory))) (and commit (format nil "git ~a" commit))))
              (t (car (last (pathname-directory directory)))))))))

;;; The generator's fingerprint

(defun fnv-1a (octets &optional (hash #xcbf29ce484222325))
  "64-bit FNV-1a of OCTETS, continuing from HASH."
  (loop for octet across octets
        do (setf hash (ldb (byte 64 0) (* (logxor hash octet) #x100000001b3))))
  hash)

(defun file-octets (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun generator-files (library)
  "The files whose changes change LIBRARY's docset: this generator's source,
the docset library's, and LIBRARY's definition in libraries/."
  (append (sort (directory (asdf:system-relative-pathname "docsets" "src/*.lisp")) #'string< :key #'namestring)
          (sort (directory (asdf:system-relative-pathname "docset" "src/*.lisp")) #'string< :key #'namestring)
          (let ((file (library-file library))) (when (and file (probe-file file)) (list file)))))

(defun generator-fingerprint (library)
  "A fingerprint of what generates LIBRARY's docset, as 16 hex digits."
  (let ((hash #xcbf29ce484222325))
    (dolist (file (generator-files library))
      (setf hash (fnv-1a (file-octets file) hash)))
    (format nil "~(~16,'0x~)" hash)))

;;; Checking

(defun docset-status (directory)
  "Whether the docset in DIRECTORY is up to date, as a plist: :id, :status
(:current, :library-changed, :generator-changed, :unrecorded, or :missing when
its library isn't installed), and the recorded and installed :release."
  (let* ((manifest (docset:docset-manifest (docset:open-docset directory)))
         (id (getf manifest :id))
         (library (find-library id))
         (recorded (getf manifest :release))
         (installed (library-release (library-system library))))
    (list :id id :directory directory :recorded recorded :installed installed
          :status (cond ((null installed) :missing)
                        ((null recorded) :unrecorded)
                        ((not (equal recorded installed)) :library-changed)
                        ((not (equal (getf manifest :generator) (generator-fingerprint library)))
                         :generator-changed)
                        (t :current)))))

(defun docset-statuses (&key (directory (docset:user-docset-directory)))
  "The status of every docset in DIRECTORY, each in its own subdirectory."
  (loop for manifest in (sort (directory (merge-pathnames "*/manifest.sexp" directory))
                              #'string< :key #'namestring)
        collect (docset-status (make-pathname :name nil :type nil :defaults manifest))))

(defun outdated-p (status) (member (getf status :status) '(:library-changed :generator-changed :unrecorded)))

(defun report-outdated (&key (directory (docset:user-docset-directory)) (stream *standard-output*))
  "Prints whether each docset in DIRECTORY is up to date. Returns how many aren't."
  (let ((statuses (docset-statuses :directory directory)))
    (dolist (s statuses)
      (format stream "~&~20a ~a~%" (getf s :id)
              (ecase (getf s :status)
                (:current (format nil "up to date (~a)" (getf s :installed)))
                (:library-changed (format nil "out of date: ~a is installed, made from ~a"
                                          (getf s :installed) (getf s :recorded)))
                (:generator-changed "out of date: the generator has changed")
                (:unrecorded "out of date: made before docsets recorded versions")
                (:missing "not installed, so it can't be checked or updated"))))
    (count-if #'outdated-p statuses)))

(defun update (&key (directory (docset:user-docset-directory)))
  "Regenerates every docset in DIRECTORY that is out of date. Returns their directories."
  (loop for s in (docset-statuses :directory directory)
        when (outdated-p s)
          collect (generate (getf s :id) :output-directory (getf s :directory))))

;;; Code to read from python process over a stream

(in-package :py4cl2)

(defstruct python-object
  "A handle for a python object
which couldn't be translated into a Lisp value.
TYPE slot is the python type string
HANDLE slot is a unique key used to refer to a value in python."
  (type "" :type string)
  handle)

(defvar *print-python-object* t
  "If non-NIL, python's 'str' is called on the python-object before printing.")

(defmethod print-object ((o python-object) s)
  (print-unreadable-object (o s :type t :identity t)
    (if *print-python-object*
        (progn
          (terpri s)
          (pprint-logical-block (s nil :per-line-prefix "  ")
            ;; Doing this prevents infinite error loops if someone tries
            ;; to print out an object that causes an error which contains
            ;; the object which...
            (let ((*print-python-object* nil))
              (format s "~A" (pyeval "str(" o ")"))))
          (terpri s))
        (with-slots (type handle) o
          (format s ":HANDLE ~A :TYPE ~A" handle type)))))

(defvar *freed-python-objects* nil
  "A list of handles to be freed. This is used because garbage collection may occur in parallel with the main thread.")

(defun free-python-object (python-id handle)
  (push (list python-id handle) *freed-python-objects*))

(defun delete-freed-python-objects ()
  ;; Remove (python-id handle) pairs from the list and process
  (loop for id-handle = (pop *freed-python-objects*)
     while id-handle
     do (let ((python-id (first id-handle))
              (handle (second id-handle)))
          (if (and
               (python-alive-p) ; If not alive, pyexec will start python
               (= *current-python-process-id* python-id))  ; Python might have restarted
              ;; Call the internal function, to avoid infinite recursion or deadlock
              (raw-pyexec "
try:
  del _py4cl_objects[" (%pythonize handle) "]
except:
  pass")))))

(defvar *numpy-pickle-index*)

(defun delete-numpy-pickle-arrays ()
  "Delete pickled arrays, to free space."
  (iter (while (> *numpy-pickle-index* 0))
    (decf *numpy-pickle-index*)
    (for filename =
         (concatenate 'string
                      (config-var 'numpy-pickle-location)
                      ".to." (write-to-string *numpy-pickle-index*)))
    (uiop:delete-file-if-exists filename)))

(defun make-python-object-finalize (&key (type "") handle)
    "Make a PYTHON-OBJECT struct with a finalizer.
This deletes the object from the dict store in python.

Uses trivial-garbage (public domain)
"
    (tg:finalize
     (make-python-object :type type
                         :handle handle)
     (let ((python-id *current-python-process-id*))
       (lambda () ; This function is called when the python-object is garbage collected
         (ignore-errors
           ;; Put on a list to free later. Garbage collection may happen
           ;; in parallel with the main thread, which may be executing other commands.
           (free-python-object python-id handle))))))

(defun stream-read-string (stream)
  "Reads a string from a stream
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (let ((nchars (parse-integer (read-line stream))))
    (with-output-to-string (str)
      (iter (for i from 1 to nchars)
        (for char = (read-char stream))
        (write-char char str)))))

(defun stream-read-value (stream)
  "Get a value from a stream
Currently works by reading a string then using read-from-string
"
  (let ((str (stream-read-string stream)))
    (multiple-value-bind (value count)
        (read-from-string str)
      ;; Check if all characters were used
      (unless (eql count (length str))
        (error (concatenate 'string "unread characters in reading string \"" str "\"")))
      value)))

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

(defun free-python-object (python-id handle &optional (python *python*))
  (push (list python-id handle) (python-freed-python-objects python)))

(defun delete-freed-python-objects (&optional (python *python*))
  ;; Remove (python-id handle) pairs from the list and process
  (loop for id-handle = (pop (python-freed-python-objects python))
     while id-handle
     do (let ((python-id (first id-handle))
              (handle (second id-handle)))
          ;; Don't bother if python dead or we restarted it
          (if (and
                (python-alive-p python) ; If not alive, pyexec will start python
                (= python-id (uiop:process-info-pid python)))  ; Python might have restarted
              ;; Call the internal function, to avoid infinite recursion or deadlock
              (raw-pyexec "
try:
  del _py4cl_objects[" (%pythonize handle) "]
except:
  pass")))))

(defun delete-numpy-pickle-arrays (&optional (python *python*))
  "Delete pickled arrays, to free space."
  (loop while (> (python-numpy-pickle-index python) 0)
     do (decf (python-numpy-pickle-index python))
        (let ((filename
                (concatenate 'string
                             (config-var 'numpy-pickle-location)
                             "-" (write-to-string (uiop:process-info-pid python))
                             ".to." (write-to-string (python-numpy-pickle-index python)))))
          (uiop:delete-file-if-exists filename))))

(defun make-python-object-finalize (&key (type "") handle (python *python*))
    "Make a PYTHON-OBJECT struct with a finalizer.
 This deletes the object from the dict store in python."
  (tg:finalize
     (make-python-object :type type :handle handle)
     (let ((python-id (uiop:process-info-pid python)))
       (lambda () ; This function is called when the python-object is garbage collected
         (ignore-errors
           ;; Put on a list to free later. Garbage collection may happen
           ;; in parallel with the main thread, which may be executing other commands.
          (free-python-object python-id handle python))))))

(defun stream-read-string (stream)
  "Reads a string from a stream
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (declare (optimize speed safety))
  (let* ((nchars (parse-integer (read-line stream)))
         (seq (make-array nchars :element-type 'character)))
    (read-sequence seq stream)
    seq))

(defun stream-read-value (stream)
  "Get a value from a stream
Currently works by reading a string then using read-from-string
"
  (declare (optimize speed safety))
  (let ((str (stream-read-string stream)))
    (multiple-value-bind (value count)
        (read-from-string str)
      ;; Check if all characters were used
      (unless (eql count (length str))
        (error (concatenate 'string "unread characters in reading string \"" str "\"")))
      value)))

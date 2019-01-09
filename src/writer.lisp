;;; Write data to python over a stream

(in-package :py4cl)

(defgeneric pythonize (obj)
  (:documentation
   "Convert an object into a string which can be written to stream.
Default implementation returns an empty string")
  (:method (obj) 
    "None"))

(defmethod pythonize ((obj real))
  "Write a real number. 
   Note that python doesn't handle 'd','f', 's' or 'L' exponent markers"
  (substitute-if #\e (lambda (ch)
                       (member ch '(#\d #\D #\f #\F #\s #\S #\l #\L)))
                 (write-to-string obj)))

(defmethod pythonize ((obj array))
  (with-output-to-string (stream)
    (write-char #\[ stream)
    (princ (row-major-aref obj 0) stream)
    (do ((indx 1 (1+ indx)))
        ((>= indx (array-total-size obj)))
      (write-char #\, stream)
      (princ (row-major-aref obj indx) stream))
    (write-char #\] stream)))

(defmethod pythonize ((obj cons))
  "Convert a list. This leaves a trailing comma so that python
evals a list with a single element as a tuple
"
  (with-output-to-string (stream)
    (write-char #\( stream)
    (dolist (val obj)
      (write-string (pythonize val) stream)
      (write-char #\, stream))
    (write-char #\) stream)))

(defmethod pythonize ((obj string))
  (write-to-string obj :escape t :readably t))

(defmethod pythonize ((obj symbol))
  "Handle symbols. Need to handle NIL,
converting it to Python None"
  (if obj
      (concatenate 'string
                   "_py4cl_Symbol(':" (string-downcase (string obj)) "')")
      "None"))

(defun stream-write-string (str stream)
  "Write a string to a stream, putting the length first"
  ;; Convert the value to a string
  (princ (length str) stream)  ; Header, so length of string is known to reader
  (terpri stream)
  (write-string str stream))
    
(defun stream-write-value (value stream)
  "Write a value to a stream, in a format which can be read
by the python subprocess as the corresponding python type"
  (stream-write-string (pythonize value) stream))

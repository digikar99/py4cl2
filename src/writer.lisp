;;; Write data to python over a stream

(in-package :py4cl2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object Handles

(defvar *handle-counter* 0)

(defvar *lisp-objects* (make-hash-table :test #'eql))

(defun clear-lisp-objects ()
  "Clear the *lisp-objects* object store, allowing them to be GC'd"
  (setf *lisp-objects* (make-hash-table :test #'eql)
        *handle-counter* 0))

(defun free-handle (handle)
  "Remove an object with HANDLE from the hash table"
  (remhash handle *lisp-objects*))

(defun lisp-object (handle)
  "Get the lisp object corresponding to HANDLE"
  (or (gethash handle *lisp-objects*)
      (error "Invalid Handle.")))

(defun object-handle (object)
  "Store OBJECT and return a handle"
  (let ((handle (incf *handle-counter*)))
    (setf (gethash handle *lisp-objects*) object)
    handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert objects to a form which python can eval

(defgeneric pythonize (obj)
  (:documentation
   "Convert an object into a string which can be written to stream.
Default implementation creates a handle to an unknown Lisp object.")
  (:method (obj)
    (concatenate 'string
                 "_py4cl_UnknownLispObject(\""
                 (write-to-string
                 (type-of obj))
                 "\", "
                 (write-to-string
                  (object-handle obj))
                 ")")))

(defmethod pythonize ((obj real))
  "Write a real number. 
   Note that python doesn't handle 'd','f', 's' or 'L' exponent markers"
  (substitute-if #\e (lambda (ch)
                       (member ch '(#\d #\D #\f #\F #\s #\S #\l #\L)))
                 (write-to-string obj)))

(defmethod pythonize ((obj complex))
  "Create string of the form \"(1+2j\". 
If imaginary part is negative the output is of form \"(1+-2j\"
which is interpreted correctly by python (3.7.2)."
  (concatenate 'string
               "("
               (write-to-string (realpart obj))
               "+"
               (write-to-string (imagpart obj))
               "j)"))

(defvar *numpy-pickle-index* 0
  "Used for transferring multiple numpy-pickled arrays in one pyeval/exec/etc")
;; this is incremented by pythonize and reset to 0 at the beginning of
;; every pyeval*/pycall from delete-numpy-pickle-arrays in reader.lisp
(defmethod pythonize ((obj array))
  (when (and (config-var 'numpy-pickle-lower-bound)
             (config-var 'numpy-pickle-location)
             (> (array-total-size obj)
                (config-var 'numpy-pickle-lower-bound)))
    (let ((filename (concatenate 'string
				 (config-var 'numpy-pickle-location)
				 "." (write-to-string (incf *numpy-pickle-index*)))))
      (numpy-file-format:store-array obj filename)
      (return-from pythonize
	(concatenate 'string "_py4cl_load_pickled_ndarray_and_delete('"
		     filename"')"))))
  
  ;; Handle case of empty array
  (if (= (array-total-size obj) 0)
      (return-from pythonize "[]"))
  
  ;; First convert the array to 1D [0,1,2,3,...]
  (let ((array1d (with-output-to-string (stream)
                   (write-char #\[ stream)
                   (princ (pythonize (row-major-aref obj 0)) stream)
                   (do ((indx 1 (1+ indx)))
                       ((>= indx (array-total-size obj)))
                     (write-char #\, stream)
                     (princ (pythonize (row-major-aref obj indx)) stream))
                   (write-char #\] stream))))
    (if (= (array-rank obj) 1)
        ;; 1D array return as-is
        array1d
        ;; Multi-dimensional array. Call NumPy to resize
        (concatenate 'string
                     "_py4cl_numpy.resize(" array1d ", "
                     (pythonize (array-dimensions obj)) ")"))))

(defmethod pythonize ((obj cons))
  "Convert a list. This leaves a trailing comma so that python
evals a list with a single element as a tuple
"
  (if obj
      (with-output-to-string (stream)
        (write-char #\( stream)
        (dolist (val obj)
          (write-string (pythonize val) stream)
          (write-char #\, stream))
        (write-char #\) stream))
      "None"))

(defmethod pythonize ((obj string))
  (if (find-if (lambda (ch) (char= #\newline ch)) obj)
      (with-output-to-string (return-string)
        (write-string "\"\"\"" return-string)
        (write-string
         (let ((escaped-string (write-to-string (coerce obj
                                                        '(vector character))
                                                :escape t :readably t)))
           (subseq escaped-string 1 (1- (length escaped-string))))
         return-string)
        (write-string "\"\"\"" return-string))
      (write-to-string (coerce obj '(vector character))
                       :escape t :readably t)))

(defvar *lisp-to-python-types-alist*
  '((t "True")
    (nil "None")
    (float "float")
    (boolean "bool")
    (null "type(None)")
    (integer "int")
    (complex "complex")
    (vector "list")
    (hash-table "dict")
    (string "str")))
;; leaves out inspect._empty    

(defmethod pythonize ((obj symbol))
  "One-to-one mapping between python name and lisp symbol names:
     symbol  :  symbol-name  : python-name
  'foo-bar   :  \"FOO-BAR\"  : foo_bar
  '|Foo-Bar| :  \"Foo-Bar\"  : Foo_Bar
  '*foo-bar* : \"*FOO-BAR*\" : FOO_BAR
      t      :     \"T\"     :  True
     nil     :    \"NIL\"    :  None"
  (let* ((symbol-name (symbol-name obj))
         (name (cond ((and (char= (char symbol-name 0) #\*) ; *global-variable* == PYTHON_CONSTANT
                           (char= (char symbol-name (1- (length symbol-name)))))
                      (subseq symbol-name 1 (1- (length symbol-name))))
                     ((string= "NIL" symbol-name) "None")
                     ((string= "T" symbol-name) "True")
                     ((every #'(lambda (char) ; = every character is either upper-case 
                                 (not (lower-case-p char))) ; or is not an alphabet
                             symbol-name)
                      (format nil "~(~a~)" symbol-name))
                     (t symbol-name))))
    
    (iter (for char in-string name)
          (collect (if (char= char #\-)
                       #\_
                       char)
            into python-name
            result-type string)
          (finally (return
                     (if (equalp (symbol-package obj) (find-package :keyword))
                         (format nil "~A = " python-name)
                         python-name))))))

(defun depythonize (string &optional (can-be-global nil))
  (let ((hyphenated (iter (for c in-string string)
                          (collect (if (char= c #\_) #\- c)
                            result-type 'string))))
    (cond ((string= hyphenated "None") "NIL")
          ((string= hyphenated "True") "T")
          ((and can-be-global
                (every #'(lambda (char) ; = every character is either upper-case 
                           (not (lower-case-p char))) ; or is not an alphabet
                       hyphenated))
           (format nil "*~A*" hyphenated))
          ((some #'upper-case-p hyphenated) hyphenated)
          (t (string-upcase hyphenated)))))

(defmethod pythonize ((obj hash-table))
  "Convert hash-table to python map.
Produces a string {key1:value1, key2:value2,}"
  (concatenate 'string
               "{"
               (apply #'concatenate 'string
                      (loop for key being the hash-keys of obj
                         using (hash-value value)
                         appending (list (pythonize key) ":" (pythonize value) ",")))
               "}"))

(defmethod pythonize ((obj function))
  "Handle a function by converting to a callback object
The lisp function is stored in the same object store as other objects."
  (concatenate 'string
                 "_py4cl_LispCallbackObject("
                 (write-to-string
                  (object-handle obj))
                 ")"))

(defmethod pythonize ((obj python-object))
  "A handle for a python object, stored in a dict in Python"
  (concatenate 'string
               "_py4cl_objects["
               (write-to-string (python-object-handle obj))
               "]"))

(defmethod pythonize ((obj ratio))
  "Handle ratios, using Python's Fraction if available"
  (concatenate 'string
               "_py4cl_fraction("
               (pythonize (numerator obj))
               ","
               (pythonize (denominator obj))
               ")"))

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

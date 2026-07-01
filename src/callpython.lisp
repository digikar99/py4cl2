;; This file is divided into:
;; - Preparations for calling
;;   - dispatch-reply: Used for calling and passing lisp functions to python
;;     Note that this calling can be nested: probably, "return_values"
;;     py4cl.py file keeps track of this nesting (see LispCallbackObject class
;;     in python
;; - Raw Functions
;; - Utility Functions
;;   - eval, exec, call, method, async, monitor
;;   - chain
;;   - remote objects

(in-package :py4cl2)

;; ============================ PREPARATIONS FOR CALLING ======================

(define-condition pyerror (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python error: ~a" (text condition)))))

(define-condition python-eof-but-alive (end-of-file)
  ;; TODO: How will this condition arise?
  ((python-process :initarg :python-process :reader python-process))
  (:report (lambda (condition stream)
             (format stream
                     "Unable to read (or write) from python process~%  ~S~%but the process is alive"
                     (python-process condition)))))

(define-condition python-eof-and-dead (end-of-file)
  ((python-process :initarg :python-process :reader python-process))
  (:report (lambda (condition stream)
             (format stream
                     "The python process~%  ~S~%has died. Unable to read (or write) from it"
                     (python-process condition)))))

(defun dispatch-reply (stream value)
  (write-char #\r stream)
  (stream-write-value value stream)
  (force-output stream))

(defmacro cp-debug-print (&rest rest)
  (declare (ignorable rest))
  #+debug `(format *standard-output* ,@rest))

(defun dispatch (python python-message)
  (loop
    for message-char = (python-message-cmd-char python-message)
    for string = (python-message-string python-message)
    :do
       (cp-debug-print "DP: dispatch on '~A'~%" message-char)
       (return
         (case message-char
           (#\r (return (values (read-value string) t)))
           (#\e (return (values (make-python-error :thunk (lambda () (error 'pyerror :text string))) t)))
           (#\d ;; Delete object. This is called when an UnknownLispObject is deleted
	    (free-handle (read-value string))
            (values))
           (#\s ;; Slot read
	    (destructuring-bind (handle slot-name)
                (read-value string)
              (let ((object (lisp-object handle)))
                ;; User must register a function to handle slot access
                (dispatch-reply
                 (python-input python)
                 (with-sldb-default-restart return-nil
                   (restart-case
                       (python-getattr object slot-name)
                     ;; Provide some restarts for missing handler or missing slot
		     (return-nil () (values nil t))
                     (return-zero () 0)
                     (enter-value (return-value)
                       :report "Provide a value to return"
                       :interactive (lambda ()
                                      (format t "Enter a value to return: ")
                                      (list (read)))
		       (values return-value t))))))))
           (#\S ;; Slot write
	    (destructuring-bind (handle slot-name slot-value)
                (read-value string)
              (let ((object (lisp-object handle)))
                ;; User must register a function to handle slot write
                (python-setattr object slot-name slot-value)
                (dispatch-reply (python-input python) nil))
              (values)))
           (#\c ;; Callback. Return a list, containing function ID, then the args
            (let ((*lispifiers* (python-lispifiers python))
                  (*pythonizers* (python-pythonizers python)))
              (let* ((call-value (read-value string))
                     (return-value
                       (with-sldb-default-restart ignore
                         (restart-case
                             (apply (lisp-object (first call-value))
                                    (if (and (stringp (second call-value))
                                             (string= "()" (second call-value)))
                                        ()
                                        (second call-value)))
                           (ignore () nil)))))
	        (dispatch-reply (python-input python) return-value)
                (values))))
           (#\p				; Print stdout
            (let ((print-string (read-value string)))
	      (lambda () (princ print-string))))
           (otherwise
            (values (make-python-error :thunk (lambda () (error "Unhandled message type '~d'" message-char))) t))))))

(defmacro with-timing (&rest body)
  (let ((g (gensym)))
    `(let ((,g (/ (get-internal-real-time) internal-time-units-per-second 1f0)))
       (labels ((current-time ()
                  (- (/ (get-internal-real-time) internal-time-units-per-second 1f0) ,g)))
         ,@body))))

(defstruct timing-info
  (calls 0 :type (unsigned-byte 32))
  (total-time 0f0 :type single-float))

(defparameter *timing* (make-timing-info))

(defun add-to-timing (time &optional (timing *timing*))
  (incf (timing-info-calls timing))
  (incf (timing-info-total-time timing) time)
  (values))

(defun print-timing (&optional (timing *timing*))
  (format t "~A calls in ~,3f seconds: ~,1f calls/second~%"
          (timing-info-calls timing)
          (timing-info-total-time timing)
          (/ (timing-info-calls timing)
             (timing-info-total-time timing))))

;; ============================== RAW FUNCTIONS ================================
(defun raw-py (cmd-char &rest strings)
  "Intended as an abstraction to RAW-PYEVAL and RAW_PYEXEC.
Passes strings as they are, without any 'pythonize'ation."
  (python-start-if-not-alive)
  (with-timing
      (prog1
          (let ((r (make-request *python* cmd-char (apply #'concatenate 'string strings))))
            (if (python-error-p r)
                (restart-case
                    (funcall (python-error-thunk r))
                  (IGNORE ()))
                r))
        (add-to-timing (current-time)))))

(defun raw-py-exec/no-return (&rest strings)
  "Execute strings without expecting any return, used to pass
control permanently to, say, a GUI main loop in the python process.
Passes strings as they are, without any 'pythonize'ation."
  (python-start-if-not-alive)
  (let* ((python *python*)
	 (stream (python-input python))
         (str (apply #'concatenate 'string strings)))
    (bt:with-recursive-lock-held ((python-interaction-lock python))
      (write-char #\x stream)
      (stream-write-string str stream)
      (force-output stream))))

(defun raw-pyeval (&rest strings)
  "Calls python eval on the concatenation of strings, as they are, without any
pythonization or modification."
  (restart-case (apply #'raw-py #\e strings)
    (raw-pyexec ()
      :report "If the error is 'invalid syntax', using 'exec' instead of 'eval' might work."
      (apply #'raw-pyexec strings)
      (values))))

(defun raw-pyexec (&rest strings)
  "Calls python exec on the concatenation of strings, as they are, without any
pythonization or modification.
NOTE: Like usual, there are peculiarities to exec commands.
For instance,
  import sys
  def foo:
    sys.stdout.write('hello')
  foo()
will result in 'sys' name not defined PYERROR."
  (python-start-if-not-alive)
  (apply #'raw-py #\x strings)
  (values))

(defun (setf raw-pyeval) (value &rest args)
  (apply #'raw-pyexec (append args
                              (list "=" (pythonize value))))
  value)

;; =========================== UTILITY FUNCTIONS ===============================

(labels ((pythonizep (value)
           "Determines if VALUE should be pythonized."
           (or (not (stringp value)) ; do not pythonize if
               (realp (ignore-errors (parse-number:parse-number value)))))
         (pythonize-if-needed (value)
           (if (pythonizep value) (%pythonize value) value)))

  (defun pyeval (&rest args)
    "Calls python eval on args; PYTHONIZEs arg if it satisfies PYTHONIZEP.
Eg.
  > (let ((a 5)) (pyeval a \"*\" a))
  25"
    (python-start-if-not-alive)
    (delete-freed-python-objects) ; delete before pythonizing
    (apply #'raw-pyeval (mapcar #'pythonize-if-needed args)))

  (defun pyexec (&rest args)
    "Calls python exec on args; PYTHONIZEs arg if it satisfies PYTHONIZEP."
    (python-start-if-not-alive)
    (delete-freed-python-objects) ; delete before pythonizing
    (apply #'raw-pyexec (mapcar #'pythonize-if-needed args)))

  ;; One argument for the name (setf pyeval) is that it sets the "place" returned
  ;; by pyeval.
  (defun (setf pyeval) (value &rest args)
    "Set an expression to a value. Just adds \"=\" and the value
to the end of the expression. Note that the result is evaluated
with exec rather than eval.
Example:
    (setf (pyeval \"a\") 2)  ; python \"a=2\"
Can be useful for modifying a value directly in python.
"
    (python-start-if-not-alive)
    (apply #'pyexec (append args (list "=" value))) ; would nconc be better?
    value))


(defun %pycall-args (&rest args)
  (apply #'concatenate
         'string
         "("
         `(,@(iter (for arg in args)
                   (for pythonized-arg = (%pythonize arg))
                   (if (and (symbolp arg)
                            (eq (find-package :keyword)
                                (symbol-package arg)))
                       (collect pythonized-arg)
                       (progn (collect pythonized-arg)
                              (collect ","))))
             ")")))

(flet ((pythonize-if-needed (name)
         (if (stringp name)
             name
             (%pythonize name))))

  (defun pycall (fun-name &rest args)
    "Calls FUN-NAME with ARGS as arguments. Arguments can be keyword based, or
 otherwise."
    (raw-pyeval "("
                (pythonize-if-needed fun-name)
                ")"
                (apply #'%pycall-args args)))

  (defun pymethod (object method &rest args)
    "PYCALLs METHOD of OBJECT with ARGS
Examples:
  > (pymethod \"'hello {0}'\" 'format \"world\")
  \"hello world\"
  > (pymethod '(1 2 3) '--len--)
  3
Note: FUN-NAME is NOT PYTHONIZEd if it is a string.
"
    (python-start-if-not-alive)
    (apply #'pycall
           (concatenate 'string
                        (%pythonize object)
                        "."
                        (pythonize-if-needed method))
           args))

  (defun pyslot-value (object slot-name)
    (pyeval object "." (pythonize-if-needed slot-name)))

  (defun (setf pyslot-value) (new-value object slot-name)
    (pyexec object "." (pythonize-if-needed slot-name) " = " new-value)))

(defun pygenerator (function stop-value)
  (pycall "_py4cl_generator" function stop-value))

(defun pyversion-info ()
  "Return a list, using the result of python's sys.version_info."
  (pyexec "import sys")
  (pyeval "tuple(sys.version_info)"))

(defun pyhelp (object)
  (pyeval "help(" object ")"))

;; Chain -----------------------------------------------------------------------

;;; If someone wants to handle multidimensional array slicing and stuff, they should take
;;; a look at: https://github.com/hylang/hy/issues/541
(defun %chain* (&rest chain)
  (if (= 1 (length chain))
      (let ((chain (first chain)))
        (typecase chain
          (cons (cond ((and (symbolp (car chain))
                            (member (symbol-name (car chain)) '("@" "CHAIN")
                                    :test 'string=))
                       (format nil "~{~a~^.~}" (mapcar #'%chain* (cdr chain))))
                      ((eq 'aref (car chain))
                       (apply #'concatenate
                              'string
                              (%chain* (cadr chain))
                              (mapcar (lambda (link) (format nil "[~A]"
                                                             (%chain* link)))
                                      (cddr chain))))
                      (t
                       (apply #'concatenate
                              'string
                              (if (stringp (car chain))
                                  (car chain)
                                  (%chain* (car chain)))
                              "("
                              `(,@(iter (for link in (cdr chain))
                                        (for pythonized-link = (%chain* link))
                                        (collect pythonized-link)
                                        (unless (and (symbolp link)
                                                     (eq (find-package :keyword)
                                                         (symbol-package link)))
                                          (collect ",")))
                                  ")")))))
          (t (%pythonize chain))))
      (format nil "~{~a~^.~}" (mapcar #'%chain* chain))))

(defun chain* (&rest chain) (raw-pyeval (apply #'%chain* chain)))
(defmacro chain (&rest chain) `(raw-pyeval ,(apply #'%chain* chain)))

(defun (setf chain*) (value &rest args)
  (apply #'raw-pyexec (list (apply #'%chain* args)
                            "="
                            (%pythonize value)))
  value)

(defmacro with-remote-objects (&body body)
  "Ensures that all values returned by python functions
 and methods are kept in python, and only handles returned to lisp.
 This is useful if performing operations on large datasets."
  `(progn
     (python-start-if-not-alive)
     (let ((stream (python-input *python*)))
       (bt:with-recursive-lock-held ((python-interaction-lock python))
         (write-char #\O stream)        ;; Turn on remote objects
         (force-output stream))
       (unwind-protect
            (progn ,@body)
         (bt:with-recursive-lock-held ((python-interaction-lock python))
           (write-char #\o stream)          ;; Turn off remote objects
           (force-output stream))))))

(defmacro with-remote-objects* (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets. Unlike
with-remote-objects, evaluates the last result and returns not just a handle."
  `(pyeval (with-remote-objects ,@body)))


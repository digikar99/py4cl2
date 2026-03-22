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

(defun dispatch-messages (process)
  "Read response from python, loop to handle any callbacks"
  (let ((*python-process-busy-p* t))
    (handler-case
        (let* ((read-stream (uiop:process-info-output process))
               (write-stream (uiop:process-info-input process))
               (return-value
                 (loop
                   ;; First character is type of message
                   :for message-char := (read-char read-stream)
                   :do
                      (case message-char
                        (#\r (return (stream-read-value read-stream))) ; Returned value

                        (#\e (error 'pyerror
                                    :text (stream-read-string read-stream)))
                        ;; Delete object. This is called when an UnknownLispObject is deleted
                        (#\d (free-handle (stream-read-value read-stream)))

                        ;; Slot read
                        (#\s (destructuring-bind (handle slot-name)
                                 (stream-read-value read-stream)
                               (let ((object (lisp-object handle)))
                                 ;; User must register a function to handle slot access
                                 (dispatch-reply
                                  write-stream
                                  (restart-case
                                      (python-getattr object slot-name)
                                    ;; Provide some restarts for missing handler or missing slot
                                    (return-nil () nil)
                                    (return-zero () 0)
                                    (enter-value (return-value)
                                      :report "Provide a value to return"
                                      :interactive (lambda ()
                                                     (format t "Enter a value to return: ")
                                                     (list (read)))
                                      return-value))))))

                        ;; Slot write
                        (#\S (destructuring-bind (handle slot-name slot-value)
                                 (stream-read-value read-stream)
                               (let ((object (lisp-object handle)))
                                 ;; User must register a function to handle slot write
                                 (python-setattr object slot-name slot-value)
                                 (dispatch-reply write-stream nil))))

                        (#\c ;; Callback. Return a list, containing function ID, then the args
                         (let* ((call-value (stream-read-value read-stream))
                                (return-value (apply (lisp-object (first call-value))
                                                     (if (and (stringp (second call-value))
                                                              (string= "()" (second call-value)))
                                                         ()
                                                         (second call-value)))))
                           (dispatch-reply write-stream return-value)))

                        (#\p                ; Print stdout
                         (let ((print-string (stream-read-value read-stream)))
                           (princ print-string)))
                        (otherwise (error "Unhandled message type '~d'" message-char))))))
          return-value)
      (end-of-file (condition)
        (declare (ignore condition))
        (sleep 0.00001)
        (error (if (python-alive-p process)
                   'python-eof-but-alive
                   'python-eof-and-dead)
               :python-process process)))))

;; ============================== RAW FUNCTIONS ================================
(defvar *python-lock* (bt:make-recursive-lock "py4cl2"))

(declaim (ftype (function (character &rest string)) raw-py))
(defun raw-py (cmd-char &rest strings)
  "Intended as an abstraction to RAW-PYEVAL and RAW_PYEXEC.
Passes strings as they are, without any 'pythonize'ation."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*))
        (str (apply #'concatenate 'string strings)))
    (bt:with-recursive-lock-held (*python-lock*) ; wait for previous processing to be done
      (write-char cmd-char stream)
      (stream-write-string str stream)
      (force-output stream)
      ;; wait for python output
      (dispatch-messages *python*))))

(defun raw-py-exec/no-return (&rest strings)
  "Execute strings without expecting any return, used to pass
control permanently to, say, a GUI main loop in the python process.
Passes strings as they are, without any 'pythonize'ation."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*))
        (str (apply #'concatenate 'string strings)))
    (bt:with-recursive-lock-held (*python-lock*) ; wait for previous processing to be done
      (write-char #\x stream)
      (stream-write-string str stream)
      (force-output stream))))

(declaim (ftype (function (&rest string)) raw-pyeval))
(defun raw-pyeval (&rest strings)
  "Calls python eval on the concatenation of strings, as they are, without any
pythonization or modification."
  (restart-case (apply #'raw-py #\e strings)
    (raw-pyexec ()
      :report "If the error is 'invalid syntax', using 'exec' instead of 'eval' might work."
      (apply #'raw-pyexec strings)
      (values))))

(declaim (ftype (function (&rest string)) raw-pyexec))
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
    (delete-numpy-pickle-arrays)
    (apply #'raw-pyeval (mapcar #'pythonize-if-needed args)))

  (defun pyexec (&rest args)
    "Calls python exec on args; PYTHONIZEs arg if it satisfies PYTHONIZEP."
    (python-start-if-not-alive)
    (delete-freed-python-objects) ; delete before pythonizing
    (delete-numpy-pickle-arrays)
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
  `(let ()
     (python-start-if-not-alive)
     (let ((stream (uiop:process-info-input *python*)))
       (write-char #\O stream)        ;; Turn on remote objects
       (force-output stream)
       (unwind-protect
            (progn ,@body)
         (write-char #\o stream)          ;; Turn off remote objects
         (force-output stream)))))

(defmacro with-remote-objects* (&body body)
  "Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets. Unlike
with-remote-objects, evaluates the last result and returns not just a handle."
  `(pyeval (with-remote-objects ,@body)))


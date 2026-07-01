;;; Functions to start and stop python process

(in-package :py4cl2)

(defvar *python-id* 0
  "Unique id for each python process.  Used so we can keep track of
 pickled arrays and also to track objects that we are keeping alive
 in the python process, so we can free them later (if the python
 process is restarted, we don't care about them anymore)")

;; State of a running python process
;; We use stdin / stdout to communicate with our dispatch loop
;; stderr is what python thinks stdout is, so on that we capture
;; printed output and misc error output.

;; Request / return-value pairs occur in a stack like format, the most
;; response return-value response from python is always associated with
;; the most recently sent request.  We use python-interactions as the
;; equivalent stack to keep request/return-value responses in order.
;; That doesn't work either because python may get an EVAL request, be
;; working on it, the interaction thread goes to sleep as does the caller
;; and another thread comes in and asks for a request, thus getting
;; matched.

(defstruct queue-elt
  (elt nil)
  (next nil :type (or null queue-elt))
  (previous nil :type (or null queue-elt)))

(defstruct queue
  (head nil :type (or null queue-elt))
  (tail nil :type (or null queue-elt))
  (waitqueue (bt:make-condition-variable))
  (lock (bt:make-lock)))

(defun enqueue (queue elt)
  "push to tail"
  (bt:with-lock-held ((queue-lock queue))
    (let* ((tail (queue-tail queue))
           (new-queue-elt (make-queue-elt :elt elt :next nil :previous tail)))
      (when (not (queue-head queue)) (setf (queue-head queue) new-queue-elt))
      (when tail
        (setf (queue-elt-next (queue-tail queue)) new-queue-elt))
      (setf (queue-tail queue) new-queue-elt)
      (bt2:condition-broadcast (queue-waitqueue queue)))))

(defun dequeue (queue)
  "pop from head"
  (bt:with-lock-held ((queue-lock queue))
    (let ((elt (queue-head queue)))
      (cond
        (elt
         (let ((next (queue-elt-next elt)))
           (setf (queue-head queue) next)
           (if (eq elt (queue-tail queue))
               (setf (queue-tail queue) nil)
               (setf (queue-elt-previous next) nil)))
         (values (queue-elt-elt elt) t))
        (t (values nil nil))))))

(defun wait (queue)
  (bt:with-lock-held ((queue-lock queue))
    (unless (queue-head queue)
      (bt:condition-wait (queue-waitqueue queue) (queue-lock queue)))))

(defun peek (queue)
  (let ((lock (queue-lock queue)))
    (bt:with-lock-held (lock)
      (tagbody
       :try
         (let ((elt (queue-head queue)))
           (when elt
             (return-from peek (values (queue-elt-elt elt) t)))
           (bt:condition-wait (queue-waitqueue queue) lock)
           (go :try))))))

(defstruct python
  (subprocess nil)
  ;; This is for reading stuff coming back on stderr (printed stuff from python)
  (output-lock (bt:make-recursive-lock) :type bt:lock)
  (output-thread nil :type (or null bt:thread))
  ;; gets written to when in #'with-python-output
  (output-result (make-array 0 :element-type 'character :adjustable t :fill-pointer t) :type vector)
  (interaction-lock (bt:make-recursive-lock "interaction-lock") :type bt:lock) ;; single threads lisp clients
  (async-callback-thread nil :type (or null bt:thread))
  (read-thread nil :type (or null bt:thread))
  (read-queue (make-queue) :type queue)
  (in-with-python-output nil :type boolean)
  (id (incf *python-id*) :type fixnum) ;; unique id
  (freed-python-objects nil :type list) ;; lisp objects that have been gc'ed, free them from python
  (numpy-pickle-index 0 :type fixnum)
  ;; "Used for transferring multiple numpy-pickled arrays in one pyeval/exec/etc")
  ;; this is incremented by pythonize and reset to 0 at the beginning of
  (lisp-objects nil :type list) ;; lisp objects that python might know about
  (numpy-installed nil :type boolean)
  (thread-end-signal nil :type boolean)  ;; t to gently stop threads
  (lispifiers *lispifiers*)
  (pythonizers *pythonizers*))

(defun subprocess (python/subprocess)
  (if (python-p python/subprocess)
      (python-subprocess python/subprocess)
      python/subprocess))

(defun python-error-output (python/subprocess)
  "Works on a `python' object or a python-subprocess.  Returns
 the error output stream, things printed by the python process"
  (uiop:process-info-error-output (subprocess python/subprocess)))

(defun python-output (python/subprocess)
  "Works on a `python' object or a python-subprocess.  This is the output
 from the python process dispatch loop."
  (uiop:process-info-output (subprocess python/subprocess)))

(defun python-input (python/subprocess)
  "To use this, one must hold the python-lock.  This sends
 data to the python process"
  (uiop:process-info-input (subprocess python/subprocess)))

(defun python-alive-p (&optional (python *python*))
  "Returns non-NIL if the python process is alive
 (e.g. SBCL -> T, CCL -> RUNNING). Works on a `python'
 or a python-subprocess thereof"
  (and python (subprocess python) (uiop:process-alive-p (subprocess python))))

(defstruct python-error
  thunk)

(defmacro pp-debug-print (&rest rest)
  (declare (ignorable rest))
  #+debug `(notify-user ,@rest))

(declaim (type (or null python) *python*))

(defvar *py4cl-tests* nil "Not needed, but here for backwards compatibility")
  ;; We are loading the whole file into a variable, because we want users to be able
  ;; to use py4cl2 even in a dumped lisp image, without any additional configuration.
(defparameter *python-code*
  (alexandria:read-file-into-string
   (asdf:component-pathname
    (asdf:find-component :py4cl2 "python-code"))))

(declaim (type list *additional-init-codes*))
(defvar *additional-init-codes* nil
  "A list of strings each of which should be python code. All the code
will be executed by PYSTART. The code should not contain single-quotation marks.")

(define-condition python-process-startup-error (error)
  ((command :initarg :command :initform "" :reader command)
   (error-string :initarg :error :initform "" :reader error-string))
  (:report (lambda (condition stream)
             (format stream "Unable to start python process \"~a\"~%~% Error: ~%~%~a"
                     (command condition)
		     (error-string condition)))))

#+unix
(defun bash-escape-string (string)
  (declare (type string string))
           ;; We want strings such as
           ;; "/user/ram-disk/test (hello''/miniconda(3')/bin/"
           ;; to be escaped correctly.
  (with-output-to-string (str)
    (labels ((escape-char (ch)
               (case ch
		 (#\' (write-string "\\'" str))
		 (#\( (write-string "\\(" str))
		 (#\) (write-string "\\)" str))
		 (#\space (write-string "\\ " str))
		 (t (write-char ch str)))))
      (map nil #'escape-char string))))

(defun try-start-python (command)
  (let* ((pathname
	  (asdf:component-pathname
	   (asdf:find-component
	    :py4cl2 "python-code")))
	 (program-name
          #+(or os-windows windows)
          (concatenate 'string
                       "set OLDPYTHONIOENCODING=PYTHONIOENCODING && set PYTHONIOENCODING=utf8 && "
                       command
                       " -u "
		       (namestring pathname)
                       " "
		       (directory-namestring pathname)
                       " & set PYTHONIOENCODING=OLDPYTHONIOENCODING")
          #+unix
          (concatenate 'string
                       "bash -c \""
                       (bash-escape-string command)
                       " -u "
                       ;; Unbuffered is important if flush=True
                       ;; should not be required for asynchronous output.
                       ;; TODO: Add test for unflushed async output; been unable to
                       ;; The closest thing is the INTERRUPT test.
                       "\"' <(cat <<\"EOF\""
                       (string #\newline)
                       *python-code*;; (alexandria:read-file-into-string
                       ;;  (asdf:component-pathname
                       ;;   (asdf:find-component :py4cl2 "python-code")))
                       (string #\newline)
                       "EOF"
                       (string #\newline)
                       ")'\" "
                       (bash-escape-string
                        (directory-namestring
			 pathname))
		       "\"")))
    (uiop:launch-program program-name
                         :sharing :lock :input :stream :output :stream :error-output :stream)))

(defvar *get-results* 'get-results&)

(defun get-results (python)
  (funcall *get-results* python))

(defmacro with-sldb-default-restart (restart-name &body body)
  `(let (#+swank (swank:*sldb-quit-restart* ',restart-name))
     ,@body))

(declaim (inline make-response-request))
(defstruct response-request
  "A box for the response, and any temporary lispifiers and pythonizers to use
 during the context of the request/response"
  (response 'NOT-SATISFIED))

(defstruct write-request
  (cmd-char #\! :type character)
  (string "" :type string))

(defun is-valid (response)
  (not (eq (response-request-response response) 'NOT-SATISFIED)))

(defvar *am-read-thread* nil)

(defun read-from-python-queue (python)
  (let ((r (dequeue (python-read-queue python))))
    (if (python-error-p r)
        (restart-case
            (funcall (python-error-thunk r))
          (ignore () nil))
        r)))

(defvar *request-id* (list 0))

(defstruct python-message
  (cmd-char #\! :type character)
  (string "" :type string)
  (response-id -1 :type fixnum))

(defun make-request (python cmd-char request-string)
  ;; For use by Lisp client threads to call into Python
  ;; Outer lock keeps out other lisp clients
  (bt:with-recursive-lock-held ((python-interaction-lock python))
    (let ((stream (python-input python))
          (request-id (when (eql cmd-char #\e) (sb-ext:atomic-incf (car *request-id*)))))
      (write-char cmd-char stream)
      (when (eql cmd-char #\e)
        (write-string (format nil "~A~%" request-id) stream))
      (stream-write-string request-string stream)
      (force-output stream)
      ;; (notify-user "Requested ~A ID:~A ~A" cmd-char request-id request-string)
      ;; We will process everything, including random callbacks
      ;; until we get an answer to our request.  dispatch may
      ;; recurse inside of itself and end up calling make-request,
      ;; thus the need for a recursive-lock
      (loop
        with response = nil
        for raw-response = (read-from-python-queue python)
        do (when raw-response
             (if (or (not request-id)
                     (= (python-message-response-id raw-response) -1)
                     (= (python-message-response-id raw-response) request-id))
                 (multiple-value-bind (response response-returned)
                     (dispatch python raw-response)
                   (when (functionp response)
                     (funcall response))
                   (when response-returned
                     (return-from make-request response)))
                 (progn
                   ;; a response from some other request
                   ;; don't busy wait
                   (enqueue (python-read-queue python) raw-response)
                   (sleep 0.001))))))))

(defun notify-user (format-string &rest format-args)
  (apply 'format *standard-output* format-string format-args))

(defun read-loop (python)
  "Read messages from python, push them to read-queue"
  (loop
    with stream = (python-output python)
    with queue = (python-read-queue python)
    until (or (python-thread-end-signal python) (not (python-alive-p python)))
    :for message-char := (read-char stream)
    :do
       (case message-char
         ((#\e #\r)
          (let ((response-id (parse-integer (read-line stream))))
            (let ((msg (make-python-message :cmd-char message-char :string (stream-read-string stream)
                                           :response-id response-id)))
              ;;(notify-user "Got ~A" msg)
              (enqueue queue msg))))
         ((#\d #\s #\S #\c #\p)
          (let ((msg (make-python-message :cmd-char message-char :string (stream-read-string stream))))
            ;;(notify-user "Got ~A" msg)
            (enqueue queue msg)))
         (otherwise
          (enqueue queue (make-python-error :thunk (lambda () (error "Unhandled message type '~d'" message-char))))))))

(defun async-callback-loop (python)
  "Our job is to wait and see if there are answers"
  (loop
    until (or (python-thread-end-signal python) (not (python-alive-p python)))
    do
       (let ((p (peek (python-read-queue python))))
         (when (and p (member (python-message-cmd-char p) '(#\c #\d)))
           (bt:with-recursive-lock-held ((python-interaction-lock python))
             ;;(notify-user "async callback processing ~A" p)
             (let ((raw-response (read-from-python-queue python)))
               (when raw-response
                 (multiple-value-bind (response response-returned)
                     (dispatch python raw-response)
                   (when response-returned
                     (if (python-error-p response)
                         (restart-case
                             (funcall (python-error-thunk response))
                           (ignore ()))
                         (if (functionp response)
                             (funcall response)
                             (restart-case
                                 (error "Unexpected response ~A" response)
                               (ignore ())))))))))))))

(defparameter *pystart-lock* (bt:make-recursive-lock))

(defparameter *spurious-info* (make-array 0 :fill-pointer t :element-type 'base-char))

(defun pystart (&optional (command (config-var 'pycmd)) (python *python*))
  "Start a new python subprocess if PYTHON is not currently alive.  If an
 existing (and potentially dead) python is passed in, nothing will
 occur if it is alive, otherwise it will be restarted.  Returns a
 running python process.  If there is no global *python*, then it will
 be updated to this running python process.

 COMMAND is a string with the python executable to launch e.g. \"python\"
 By default this is is set to (CONFIG-VAR 'PYCMD)"
  (when (python-alive-p python)
    (format t "Python is already alive~%")
    (return-from pystart python))
  (bt:with-recursive-lock-held (*pystart-lock*)
    (when (python-alive-p python)
      (format t "Python is already alive~%")
      (return-from pystart python))
    (when python
      (pystop python)
      (setf (python-subprocess python) nil))
    (format t "Restarting python process~%")
    (unless python (setf python (make-python)))
    (let ((subprocess nil))
      (setf (python-id python) (incf *python-id*))
      (loop
	:until (python-alive-p python)
	:do (setf subprocess (try-start-python command))
            (sleep 0.1)
	    (unless (python-alive-p subprocess)
              (cerror "Provide another path (setf (config-var 'pycmd) ...)"
                      'python-process-startup-error :command command
		      :error-string (or (ignore-errors
					 (read-stream-content-into-string
					  (python-error-output subprocess)))
					"Unable to fetch more error details on ECL"))
              (format t "~&Provide the path to python binary to use (eg python): ")
              (let ((cmd (read-line)))
		(setf (config-var 'pycmd) cmd)
		(setf command cmd)))
	    (setf (python-subprocess python) subprocess))
      (unless (not subprocess)
	(setf (python-read-thread python)
	      (bt:make-thread
	       (lambda () (read-loop python))
	       :name "reader-thread"))
        (setf (python-async-callback-thread python)
	      (bt:make-thread
	       (lambda () (async-callback-loop python))
	       :name "async-callback-loop"))
	(setf (python-output-thread python)
              (bt:make-thread
               (lambda ()
		 (handler-case
                     (loop
		       with output-lock = (python-output-lock python)
		       with py-out = (python-error-output python)
                       while (and (python-alive-p python) (not (python-thread-end-signal python)))
		       with last-notified-of-spurious-output = (get-universal-time)
		       ;; Someone may be waiting to grab with-python-output
		       do ;; ideally we would not block, but we do
			  (let ((char (read-char py-out)))
			    (when char
			      (if (python-in-with-python-output python)
				  (progn
				    (bt:with-lock-held (output-lock)
				      (assert (< (length (python-output-result python)) 10000000))
				      (vector-push-extend char (python-output-result python))))
				  (progn
				    (when (> (- (get-universal-time) last-notified-of-spurious-output) 1)
				      (notify-user "Spurious output from python #~A:~%" (python-id python))
				      (setf last-notified-of-spurious-output (get-universal-time)))
                                    (vector-push-extend char *spurious-info*)
				    (write-char char))))))
                   (simple-error (condition)
		     (cerror "ACCEPT" "~S~%  ~A~%occured while inside python-output-thread" condition condition))
		   (end-of-file (condition)
		     ;; User will get a debugger from the interaction thread, better to only have one
		     ;; Messages will probably be garbled as we write from two threads, but better than nothing
		     (notify-user
			     "Python #~A: EOF on STDERR ~A received ~@[last message was ~A~]~%"
			     (python-id python)
			     condition (unless (emptyp (python-output-result python))
					 (python-output-result python))))
                   (stream-error (condition)
                     (unless (member :abcl *features*)
                       (cerror "ACCEPT" "~S~%  ~A~%occured while inside python-output-thread" condition condition)))))
	       :name "stderr thread")))
      (assert (python-alive-p python))
      (setf (python-numpy-installed python) (numpy-installed-p python))
      (when (numpy-installed-p python)
	(pushnew :arrays *internal-features*))
      (unless *python* (setf *python* python))
      (apply #'raw-pyexec *additional-init-codes*)
      python)))

(defmacro with-python-output (&body forms-decl)
  "Gets the output of the python program executed in FORMS-DECL in the form a string.
 We need to synchronize ourselves with the python output, there is no way to guarantee
 timing between flushing and when we get the data, so we need to use a marker in the stream
 ... we use a random value."
  `(progn
     (when (and *warn-on-unavailable-feature-usage*
                (not (member :with-python-output *internal-features*)))
       (warn "WITH-PYTHON-OUTPUT may not work on your system."))
     (let ((end-string (format nil "~A" (random (expt 2 61)))))
       (unwind-protect
	    (progn
	      (setf (python-in-with-python-output *python*) t)
                       ,@forms-decl
	      (pycall "print" end-string :end "" :flush t)
	      (loop until (bt:with-lock-held ((python-output-lock *python*))
			    (string= (python-output-result *python*)
				     end-string
				     :start1 (max 0 (- (length (python-output-result *python*))
						       (length end-string)))
				     :start2 0))
		    do (sleep 0.00001))
	      (decf (fill-pointer (python-output-result *python*)) (length end-string))
	      (prog1
		  (copy-seq (python-output-result *python*))
		(setf (fill-pointer (python-output-result *python*)) 0)))
	 (setf (python-in-with-python-output *python*) nil)))))

(defun python-start-if-not-alive (&optional (python *python*))
  "If no python process is running, tries to start it.
If still not alive, raises a condition."
  (unless (and python (python-alive-p python))
    (pystart (config-var 'pycmd) python)))

(defun pystop (&optional (python *python*))
  "Stop (Quit) the python process PROCESS"
  (when python
    (setf (python-thread-end-signal python) t)
    (when (python-alive-p python)
      (sleep 0.01)                                         ;; wait for threads to die
      (ignore-errors (write-char #\q (python-input python))) ;; can't be sure they aren't still waiting for output
      (ignore-errors (finish-output (python-input python)))
      (loop repeat 1000
	    until (not (python-alive-p python)))
      (sleep 0.01)) ;; wait for python to die before axing it
    (when (python-alive-p python)
      (ignore-errors (uiop:terminate-process (subprocess python)))
      (ignore-errors (uiop:terminate-process (subprocess python) :urgent t)))
    ;; We no longer care about any objects that needed to be freed in python
    (setf (python-freed-python-objects python) nil)
    (if (bt:thread-alive-p (python-output-thread python)) (bt:destroy-thread (python-output-thread python)))
    (if (bt:thread-alive-p (python-read-thread python)) (bt:destroy-thread (python-read-thread python)))
    (if (bt:thread-alive-p (python-async-callback-thread python))
        (bt:destroy-thread (python-async-callback-thread python)))
    (setf (python-thread-end-signal python) nil)
    (clear-lisp-objects python)
    (setf (fill-pointer (python-output-result python)) 0)
    (setf (python-read-queue python) (make-queue))
    (setf (python-output-lock python) (bt:make-recursive-lock))
    (setf (python-interaction-lock python) (bt:make-lock))
    (setf (python-subprocess python) nil)))

(defun pyinterrupt (&optional (python *python*))
  "Issues a SIGINT command to the python process"
  (when (and *warn-on-unavailable-feature-usage*
             (not (member :with-python-output *internal-features*)))
    (warn "Might not be able to issue a SIGINT to the python process on your system."))
  ;; Interrupt and grab any results
  (when (python-alive-p python)
    (uiop:run-program
     (concatenate 'string "/bin/kill -SIGINT "
                  (write-to-string (uiop:process-info-pid (subprocess python))))
     :force-shell t)))

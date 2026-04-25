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


(defstruct python
  (subprocess nil)
  ;; This is for reading stuff coming back on stderr (printed stuff from python)
  (output-lock (bt:make-recursive-lock) :type bt:lock)
  (output-thread nil :type (or null bt:thread))
  ;; gets written to when in #'with-python-output
  (output-result (make-array 0 :element-type 'character :adjustable t :fill-pointer t) :type vector)
  ;; This lock deals with reading data back from python
  (interaction-thread nil :type (or null bt:thread))
  (interaction-lock (bt:make-recursive-lock) :type bt:lock)
  (interaction-wait (bt:make-condition-variable :name "stdin stream waitqueue"))
  (interaction-results nil :type list) ;; for simplicity we lock
  ;; This is the user facing lock through raw-py.  Having two locks is dangerous
  ;; one must make sure the interaction-thread NEVER grabs the raw-py lock
  ;; as the order of locks is RAW-PY then INTERACTION-LOCK.  While the interaction-thread
  ;; grabs the INTERACTION-LOCK without the RAW-PY lock.  This is the PYTHON-DURING-CALLBACK
  ;; test
  (raw-py-lock (bt:make-recursive-lock) :type bt:lock)
  (in-with-python-output nil :type boolean)
  (id (incf *python-id*) :type fixnum) ;; unique id
  (freed-python-objects nil :type list) ;; lisp objects that have been gc'ed, free them from python
  (numpy-pickle-index 0 :type fixnum)
  ;; "Used for transferring multiple numpy-pickled arrays in one pyeval/exec/etc")
  ;; this is incremented by pythonize and reset to 0 at the beginning of
  ;; every pyeval*/pycall from delete-numpy-pickle-arrays in reader.lisp
  (lisp-objects nil :type list) ;; lisp objects that python might know about
  (numpy-installed nil :type boolean)
  (thread-end-signal nil :type boolean) ;; t to gently stop threads
  (lispifiers nil :type list)
  (pythonizers nil :type list))

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

(defmacro pp-debug-print (&rest rest)
  (declare (ignorable rest))
  #+debug `(format *standard-output* ,@rest))

(defun get-results& (python)
  "Block until some results from python.  But cannot block if the
 interaction thread is calling us."
  (declare (optimize speed safety))
  (let ((lock (python-interaction-lock python)))
    (pp-debug-print "GR: Grabbing interaction lock~%")
    (bt:with-recursive-lock-held (lock)
      (pp-debug-print "GR: Got interaction lock~%")
      (loop
	for is-result = (python-interaction-results python)
	for result = (pop (python-interaction-results python))
	until is-result ;; result may be nil!
	do
	   (pp-debug-print "GR: waiting on interaction-wait~%")
           ;; Here we have a race, because we want to give control
           ;; back to the interaction thread, but someone else may
           ;; be in raw-py waiting to grab the interaction lock and
           ;; if they get it, they will get our results.  But this
           ;; is prevented by the raw-py lock.
	   (unless (python-alive-p python)
	     (error 'python-eof-and-dead :python-process (python-subprocess python) :stream nil))
	   (bt:condition-wait (python-interaction-wait python) lock :timeout 1)
	finally
	   (pp-debug-print "GR: Got result ~S~%" result)
           ;; It's a delayed error
           (when (functionp result)
             (funcall result))
           (when (python-interaction-results python)
             (error (format nil "More results than expected: ~A" (pop (python-interaction-results python)))))
	   (return result)))))

(declaim (type (or null python) *python*))
(defvar *python* nil "Current `python' instance")
(defvar *py4cl-tests* nil "Not needed, but here for backwards compatibility")
  ;; We are loading the whole file into a variable, because we want users to be able
  ;; to use py4cl2 even in a dumped lisp image, without any additional configuration.
  (defvar *python-code*
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
                       *python-code*
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
  
(defun interaction-loop
    (python)
  "This is the loop that handles all communication back from the
 python process.  We need to be re-entrant because we may call
 callbacks from this thread which are allowed to call back into python
 and can block on get-results.  That includes being able to print
 python objects.  Re-entrance is handled by having calls into
 get-result be difference when in this loop.  We need to handle
 *lispifiers* and *pythonizers*."
  (let ((*get-results*
	 (lambda (python)
	   (dispatch-messages (python-output python) (python-input python))))
        (*print-python-object* nil)) ;; avoid deadlocks
    (declare (special *get-results* *print-python-object*))
    (loop
       until (or (python-thread-end-signal python) (not (python-alive-p python)))
       with output-stream = (python-output python)
       with input-stream = (python-input python)
       do
	 (handler-case
	     (progn
	       (pp-debug-print "IT: Waiting for python to say something~%")
	       (peek-char nil output-stream t)
	       (pp-debug-print "IT: Got something, grabbing interaction lock~%")
	       (bt:with-recursive-lock-held ((python-interaction-lock python))
		 (pp-debug-print "IT: got lock, calling dispatch-message~%")
                 (let ((*holding-interaction-lock-already* t)
                       (*lispifiers* (python-lispifiers python))
                       (*pythonizers* (python-pythonizers python)))
                   (declare (special *holding-interaction-lock-already*
                                     *lispifiers* *pythonizers*))
		   (multiple-value-bind (result result-occurred)
		       (dispatch-messages output-stream input-stream)
		     (pp-debug-print "IT: result-occurred ~A~%" result-occurred)
		     (when (or result result-occurred)
		       (pp-debug-print "IT: Got result ~S~%" result)
		       (unless (null (python-interaction-results python))
		         (format *standard-output* "Unexpected results from python!~%")
		         (map nil (lambda (x)
				    (if (functionp x) (funcall x) (format *standard-output* "~A~%" x)))
			      (python-interaction-results python))
		         (setf (python-interaction-results python) nil))
		       (push result (python-interaction-results python))
		       (pp-debug-print "IT: Results is now ~S, notifying waiters~%" (python-interaction-results python))
		       (bt:condition-notify (python-interaction-wait python)))))))
	   (end-of-file (condition)
	     ;; We end up signalling errors from both this thread and the stderr thread
	     (format *standard-output* "Python #~A STDOUT got ~A~%" (python-id python) condition)
	     (labels ((signal-error ()
			(error (if (python-alive-p python)
			           'python-eof-but-alive
			           'python-eof-and-dead)
			       :python-process (python-subprocess python))))
	       (bt:with-lock-held ((python-interaction-lock python))
		 (push
		  #'signal-error
		  (python-interaction-results python))
		 (bt:condition-notify (python-interaction-wait python)))))))))

(defparameter *pystart-lock* (bt:make-recursive-lock))

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
	(setf (python-interaction-results python) nil) ;; clear any old stuff if restarting
	(setf (python-interaction-thread python)
	      (bt:make-thread
	       (lambda () (interaction-loop python))
	       :name "interaction-thread"))
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
				      (format *standard-output* "Spurious output from python #~A:~%"
					      (python-id python))
				      (setf last-notified-of-spurious-output (get-universal-time)))
				    (write-char char))))))
                   (simple-error (condition)
		     (cerror "ACCEPT" "~S~%  ~A~%occured while inside python-output-thread" condition condition))
		   (end-of-file (condition)
		     ;; User will get a debugger from the interaction thread, better to only have one
		     ;; Messages will probably be garbled as we write from two threads, but better than nothing
		     (format *standard-output*
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
  (setf (python-thread-end-signal python) t)
  (when (python-alive-p python)
    (sleep 0.01) ;; wait for threads to die
    (write-char #\q (python-input python)) ;; can't be sure they aren't still waiting for output
    (finish-output (python-input python))
    (loop repeat 1000
	  until (not (python-alive-p python)))
    (sleep 0.01)) ;; wait for python to die before axing it
  (when (python-alive-p python)
    (ignore-errors (uiop:terminate-process (subprocess python)))
    (ignore-errors (uiop:terminate-process (subprocess python) :urgent t)))
  ;; We no longer care about any objects that needed to be freed in python
  (setf (python-freed-python-objects python) nil)
  (delete-numpy-pickle-arrays python)
  (if (bt:thread-alive-p (python-output-thread python)) (bt:destroy-thread (python-output-thread python)))
  (if (bt:thread-alive-p (python-interaction-thread python)) (bt:destroy-thread (python-interaction-thread python)))
  (setf (python-thread-end-signal python) nil)
  (clear-lisp-objects python)
  (setf (fill-pointer (python-output-result python)) 0)
  (setf (python-interaction-results python) nil)
  (setf (python-output-lock python) (bt:make-recursive-lock))
  (setf (python-interaction-lock python) (bt:make-recursive-lock))
  (setf (python-raw-py-lock python) (bt:make-recursive-lock))
  (setf (python-interaction-wait python) (bt:make-condition-variable :name "stdin stream waitqueue"))
  (setf (python-subprocess python) nil))

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

;;; Functions to start and stop python process

(in-package :py4cl2)

(eval-when (:compile-toplevel :load-toplevel :execute)

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
will be executed by PYSTART. The code should not contain single-quotation marks."))

(define-condition python-already-alive (condition)
  ((subprocess :initarg :subprocess :reader python-subprocess))
  (:report (lambda (c s)
             (format s "Python is already alive:~%~%  ~S~%"
                     (python-subprocess c)))))

(define-condition python-process-startup-error (error)
  ((command :initarg :command :initform "" :reader command)
   (error-string :initarg :error-string :initform "" :reader error-string))
  (:report (lambda (condition stream)
             (format stream "Unable to start python process \"~a\"~%~% Error: ~%~%~a"
                     (command condition)
                     (error-string condition)))))

(defvar *python-id* 0
  "Unique id for each python process.  Used so we can keep track of
 pickled arrays and also to track objects that we are keeping alive
 in the python process, so we can free them later (if the python
 process is restarted, we don't care about them anymore)")

(defun pystart (&optional (command (config-var 'pycmd)) (python *python*))
  "Start a new python subprocess if PYTHON is not currently alive.  If an
 existing (and potentially dead) python is passed in, nothing will
 occur if it is alive, otherwise it will be restarted.  Returns a
 running python process.  If there is no global *python*, then it will
 be updated to this running python process.

 COMMAND is a string with the python executable to launch e.g. \"python\"
 By default this is is set to (CONFIG-VAR 'PYCMD)"
  (when (and python (python-alive-p python))
    (signal 'python-already-alive :subprocess python)
    (return-from pystart python))
  (when (or (null python)
            (and python (uiop:process-alive-p python)))
    (setf python (make-python-process-info)))
  ;; (format t "Restarting python process~%")
  ;; (setf id (incf *python-id*))
  (loop
    :with process-info := nil
    :until (and process-info (uiop:process-alive-p process-info))
    :do (setf process-info (launch-python-command command))
        (sleep 0.1)
        (unless (uiop:process-alive-p process-info)
          (setf command (query-new-python-command command process-info)))
    :finally
    (copy-bare-process-info-to-python process-info python))

  ;; clear any old stuff if restarting
  (setf (slot-value python 'output-thread) (make-python-output-thread python))

  (assert (uiop:process-alive-p python))

  (setf (python-numpy-installed-p python) (numpy-installed-p python))
  (when (python-numpy-installed-p python)
    ;; FIXME: The features might need to be made subprocess specific
    (pushnew :arrays *internal-features*))

  (unless *python* (setf *python* python))
  (apply #'raw-pyexec *additional-init-codes*)
  python)

(defun python-alive-p (&optional (python *python*))
  "Returns non-NIL if the python process is alive
 (e.g. SBCL -> T, CCL -> RUNNING). Works on a `python'
 or a python-process-info thereof"
  (uiop:process-alive-p python))

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

(defun launch-python-command (command)
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
                         :sharing :lock
                         :input :stream
                         :output :stream
                         :error-output :stream)))

(defun query-new-python-command (old-command process-info)
  (cerror "Provide another path (setf (config-var 'pycmd) ...)"
          'python-process-startup-error :command old-command
          :error-string (or (ignore-errors
                             (read-stream-content-into-string
                              (uiop:process-info-error-output process-info)))
                            "Unable to fetch more error details on ECL"))
  (format t "~&Provide the path to python binary to use (eg python): ")
  (let ((cmd (read-line)))
    (setf (config-var 'pycmd) cmd)
    cmd))

(defun make-python-output-thread (python-process-info)
  (bt:make-thread
   (lambda ()
     (let* ((pyinfo python-process-info)
            (py-out (uiop:process-info-error-output pyinfo)))
       (iter outer
         (while (and pyinfo (uiop:process-alive-p pyinfo)))
         (handler-case
             (for char =
               (progn
                 ;; PEEK-CHAR waits for input
                 (peek-char nil py-out nil)
                 (when (python-in-with-python-output pyinfo)
                   (iter (while (python-in-with-python-output pyinfo))
                     (bt:wait-on-semaphore (python-output-semaphore pyinfo)))
                   (in outer (next-iteration)))
                 (read-char py-out nil)))
           (simple-error (condition)
             (error "~S~%  ~A~%occured while inside python-output-thread ~A"
                    condition condition (bt:current-thread)))
           (stream-error (condition)
             (unless (member :abcl *features*)
               (error "~S~%  ~A~%occured while inside python-output-thread ~A"
                      condition condition (bt:current-thread)))))
         (when char (write-char char)))))
   :name "python-output-thread (sys.stderr)"))

(defmacro with-python-output (&body forms-decl)
  "Gets the output of the python program executed in FORMS-DECL in the form a string.
 We need to synchronize ourselves with the python output, there is no way to guarantee
 timing between flushing and when we get the data, so we need to use a marker in the stream
 ... we use a random value."
  (with-gensyms (python py-out output-stream)
    `(with-output-to-string (,output-stream)
       (let ((,python *python*))
         (when (and *warn-on-unavailable-feature-usage*
                    (not (member :with-python-output *internal-features*)))
           (warn "WITH-PYTHON-OUTPUT may not work on your system."))
         (unwind-protect (progn
                           (setf (python-in-with-python-output ,python) t)
                           ,@forms-decl
                           (sleep 0.00002)
                           (let ((,py-out (uiop:process-info-error-output ,python)))
                             (iter (while (listen ,py-out))
                               (for char = (read-char ,py-out nil))
                               (when char (write-char char ,output-stream)))))
           (setf (python-in-with-python-output ,python) nil)
           (bt:signal-semaphore (python-output-semaphore ,python)))))))

(defun python-start-if-not-alive (&optional (python *python*))
  "If no python process is running, tries to start it.
If still not alive, raises a condition."
  (unless (and python (python-alive-p python))
    (pystart (config-var 'pycmd) python)))

(defun python-subprocess-quit-or-kill (python)
  ;; can't be sure they aren't still waiting for output
  (write-char #\q (uiop:process-info-input python))
  (finish-output (uiop:process-info-input python))
  (sleep 0.01) ;; wait for python to die before axing it
  (when (python-alive-p python)
    (handler-case (uiop:terminate-process python)
      (error (c) (cerror "Continue" c))))
  (sleep 0.01)
  (when (python-alive-p python)
    (handler-case (uiop:terminate-process python :urgent t)
      (error (c) (cerror "Continue" c))))
  (sleep 0.01))

(defun pystop (&optional (python *python*))
  "Stop (Quit) the python process"
  (unless (python-alive-p python)
    (return-from pystop))
  (python-subprocess-quit-or-kill python)
  ;; We no longer care about any objects that needed to be freed in python
  (setf (python-freed-python-objects python) nil)
  (delete-numpy-pickle-arrays python)
  (clear-lisp-objects python)
  (when (bt:thread-alive-p (python-output-thread python))
    (bt:destroy-thread (python-output-thread python))))

(defun pyinterrupt (&optional (python *python*))
  "Issues a SIGINT command to the python process"
  (when (and *warn-on-unavailable-feature-usage*
             (not (member :with-python-output *internal-features*)))
    (warn "Might not be able to issue a SIGINT to the python process on your system."))
  ;; Interrupt and grab any results
  (when (python-alive-p python)
    (uiop:run-program
     (concatenate 'string "/bin/kill -SIGINT "
                  (write-to-string (uiop:process-info-pid python)))
     :force-shell t)))

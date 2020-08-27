;;; Functions to start and stop python process

(in-package :py4cl)

(defvar *python-command* "python"
  "String, the Python executable to launch
e.g. \"python\" or \"python3\"")

(defvar *python* nil
  "Most recently started python subprocess")

(defvar *current-python-process-id* 0
  "A number which changes when python is started. This
is used to prevent garbage collection from deleting objects in the wrong
python session")

(defparameter *python-code*
  (let ((python-code (alexandria:read-file-into-string
                      (namestring (merge-pathnames #p"py4cl.py"
                                                   py4cl/config:*base-directory*)))))
    (when (find #\' python-code)
      (cerror "Yes"
              "There is a #\' in the code. Are you sure you want to proceed?"))
    python-code))

(defun python-start (&optional (command *python-command*))
  "Start a new python subprocess
This sets the global variable *python* to the process handle,
in addition to returning it.
COMMAND is a string with the python executable to launch e.g. \"python\"
By default this is is set to *PYTHON-COMMAND*
"
  (setf *python*
        (uiop:launch-program
         (concatenate 'string
                      "bash -c '"
                      command        ; Run python executable
                      " -u "
                      " <(cat <<\"EOF\""
                      (string #\newline)
                      *python-code*
                      (string #\newline)
                      "EOF"
                      (string #\newline)
                      ")'")
                :input :stream
                :output :stream))
  (incf *current-python-process-id*))

(defun python-alive-p (&optional (process *python*))
  "Returns non-NIL if the python process is alive
(e.g. SBCL -> T, CCL -> RUNNING).
Optionally pass the process object returned by PYTHON-START"
  (and process
       (uiop:process-alive-p process)))

(defun python-start-if-not-alive ()
  "If no python process is running, tries to start it.
If still not alive, raises a condition."
  (unless (python-alive-p)
    (python-start))
  (unless (python-alive-p)
    (error "Could not start python process")))

;; Function defined in writer.lisp, which clears an object store
(declaim (ftype (function () t) clear-lisp-objects))

(defun python-stop (&optional (process *python*))
  ;; If python is not running then return
  (unless (python-alive-p process)
    (return-from python-stop))

  ;; First ask python subprocess to quit
  ;; Could give it a few seconds to close nicely
  (let ((stream (uiop:process-info-input process)))
    (write-char #\q stream))
  ;; Close input, output streams
  (uiop:close-streams process)
  ;; Terminate
  (uiop:terminate-process process)
  ;; Mark as not alive
  (setf *python* nil)

  ;; Clear lisp objects
  (clear-lisp-objects))

(defvar *py4cl-tests* nil "Set nil for something like py4cl/tests::interrupt test,
for unknown reasons.")
(defun python-interrupt (&optional (process-info *python*))
  (when (python-alive-p process-info)
    (uiop:run-program
     (concatenate 'string "/bin/kill -SIGINT -"
		  (write-to-string (uiop:process-info-pid process-info)))
     :force-shell t)
    ;; something to do with running in separate threads! "deftest interrupt"
    (unless *py4cl-tests* (dispatch-messages process-info))))

(defun python-version-info ()
  "Return a list, using the result of python's sys.version_info."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*)))
    (write-char #\v stream)
    (force-output stream))
  (dispatch-messages *python*))

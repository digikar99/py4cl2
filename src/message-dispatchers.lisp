(in-package :py4cl2)

(defgeneric dispatch-message (cmd-char read-stream write-stream)
  (:method (cmd-char read-stream write-stream)
    (error "Unhandled message type '~d'" cmd-char)))

(defmethod dispatch-message ((cmd-char (eql #\r)) read-stream write-stream)
  "Return value from python"
  (values (stream-read-value read-stream) t))

(defmethod dispatch-message ((cmd-char (eql #\e)) read-stream write-stream)
  (error 'pyerror :text (stream-read-string read-stream)))

(defmethod dispatch-message ((cmd-char (eql #\E)) read-stream write-stream)
  (make-instance 'pyerror :text (stream-read-string read-stream)))

(defmethod dispatch-message ((cmd-char (eql #\d)) read-stream write-stream)
  "Delete object. This is called when an UnknownLispObject is deleted"
  (free-handle (stream-read-value read-stream)))

(defmethod dispatch-message ((cmd-char (eql #\s)) read-stream write-stream)
  "Slot read"
  (destructuring-bind (handle slot-name)
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

(defmethod dispatch-message ((cmd-char (eql #\S)) read-stream write-stream)
  "Slot write"
  (destructuring-bind (handle slot-name slot-value)
      (stream-read-value read-stream)
    (let ((object (lisp-object handle)))
      ;; User must register a function to handle slot write
      (python-setattr object slot-name slot-value)
      (dispatch-reply write-stream nil))))

(defmethod dispatch-message ((cmd-char (eql #\c)) read-stream write-stream)
  "Callback. Return a list, containing function ID, then the args"
  (let* ((call-value (stream-read-value read-stream))
         (return-value (apply (lisp-object (first call-value))
                              (if (and (stringp (second call-value))
                                       (string= "()" (second call-value)))
                                  ()
                                  (second call-value)))))
    (dispatch-reply write-stream return-value)))

(defmethod dispatch-message ((cmd-char (eql #\p)) read-stream write-stream)
  "Print stdout"
  (let ((print-string (stream-read-value read-stream)))
    (princ print-string)))

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
                   :do (multiple-value-bind (response return-now)
                           (dispatch-message message-char read-stream write-stream)
                         (when return-now (return response))))))
          return-value)
      (end-of-file (condition)
        (declare (ignore condition))
        (sleep 0.00001)
        (error (if (python-alive-p process)
                   'python-eof-but-alive
                   'python-eof-and-dead)
               :python-process process)))))

(defvar *python-debug-print* nil
  "When non-NIL, various py4cl2 functions print additional info for debug.")

(defun python-debug-print (&rest args)
  (when *python-debug-print*
    (apply #'format *standard-output* args)))

(defun dispatch-messages-loop (python &optional main-thread)
  "This is the loop that handles all communication back from the
 python process.  We need to be re-entrant because we may call
 callbacks from this thread which are allowed to call back into python
 and can block on get-results.  That includes being able to print
 python objects.  Re-entrance is handled by having calls into
 get-result be difference when in this loop.  We need to handle
 *lispifiers* and *pythonizers*."
  (let ((*python-results-function* 'dispatch-messages)
        (*print-python-object* nil)  ; avoid deadlocks
        (output-stream (uiop:process-info-output python)))
    (declare (special *python-results-function* *print-python-object*))
    (loop
      while (python-alive-p python)
      do (handler-case
             (progn
               (python-debug-print "IT: Waiting for python to say something~%")
               (peek-char nil output-stream t)
               (python-debug-print "IT: Got something, grabbing interaction lock~%")
               (let ((*lispifiers* (python-lispifiers python))
                     (*pythonizers* (python-pythonizers python)))
                 (declare (special *lispifiers* *pythonizers*))
                 (with-python-lock (python)
                   (handler-case (push (dispatch-messages python)
                                       (python-results-queue python))
                     (error (c)
                       (when main-thread
                         (bt:interrupt-thread main-thread (lambda () (error c)))))))
                 (bt:condition-notify (python-results-condition python))))
           (end-of-file (condition)
             ;; We end up signalling errors from both this thread and the stderr thread
             (format *standard-output* "Python #~A STDOUT got ~A~%"
                     (uiop:process-info-pid python)
                     condition)
             (with-python-lock (python)
               (push
                (make-instance (if (python-alive-p python)
                                   'python-eof-but-alive
                                   'python-eof-and-dead)
                               :python-process python)
                (python-results-queue python)))
             (bt:condition-notify (python-results-condition python)))))))

(defun make-python-message-dispatch-thread (python)
  (let ((main-thread (bt:current-thread)))
    (bt:make-thread (lambda () (dispatch-messages-loop python main-thread))
                    :name "python-message-dispatch-thread (sys.stdout)")))

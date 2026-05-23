(in-package :py4cl2)

(defgeneric dispatch-message (cmd-char read-stream write-stream)
  (:method (cmd-char read-stream write-stream)
    (error "Unhandled message type '~d'" cmd-char)))

(defmethod dispatch-message ((cmd-char (eql #\r)) read-stream write-stream)
  "Return value from python"
  (values (stream-read-value read-stream) t))

(defmethod dispatch-message ((cmd-char (eql #\e)) read-stream write-stream)
  (error 'pyerror :text (stream-read-string read-stream)))

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

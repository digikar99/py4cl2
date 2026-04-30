(in-package :py4cl2)

(declaim (type (or null python-process-info) *python*))
(defvar *python* nil "Current `python' instance")

(defclass python-process-info (process-info)
  ((start-lock
    :initform (bt:make-recursive-lock "python-start-lock")
    :type bt:recursive-lock
    :documentation
    "This lock needs to be recursive, because PYSTART calls RAW-PY,
which in turn can call PYSTART")
   (output-lock
    :initform (bt:make-recursive-lock)
    :type bt:lock
    :reader python-output-lock)
   (output-thread
    :initform nil
    :type (or null bt:thread)
    :reader python-output-thread
    :documentation "This is for reading stuff coming back on stderr (printed stuff from python)")
   (numpy-installed-p
    :initform nil
    :type boolean
    :accessor python-numpy-installed-p)
   (raw-py-lock
    :initform (bt:make-recursive-lock)
    :type bt:lock
    :reader python-raw-py-lock
    :documentation "This is the user facing lock through raw-py.")
   (thread-end-signal
    :initform nil
    :type boolean
    :accessor python-thread-end-signal
    :documentation "Set to T to gently stop threads")
   (freed-python-objects
    :initform nil
    :type list
    :accessor python-freed-python-objects
    :documentation "lisp objects that have been gc'ed, free them from python")
   (numpy-pickle-index
    :initform 0
    :type fixnum
    :accessor python-numpy-pickle-index
    :documentation
    "Used for transferring multiple numpy-pickled arrays in one pyeval/exec/etc.
This is incremented by pythonize and reset to 0 at the beginning of
every pyeval*/pycall from delete-numpy-pickle-arrays in reader.lisp")
   (lisp-objects
    :initform nil
    :type list
    :accessor python-lisp-objects
    :documentation "lisp objects that python might know about")
   (lispifiers :initform nil :type list)
   (pythonizers :initform nil :type list))
  (:documentation "State of a running python process
We use stdin / stdout to communicate with our dispatch loop
stderr is what python thinks stdout is, so on that we capture
printed output and misc error output."))

(defun make-python-process-info (&rest args &key &allow-other-keys)
  (apply #'make-instance 'python-process-info args))

(defun copy-bare-process-info-to-python (process-info python-process-info)
  (let ((slot-defns (closer-mop:class-slots (find-class 'process-info))))
    (dolist (slot-defn slot-defns)
      (setf (slot-value python-process-info (closer-mop:slot-definition-name slot-defn))
            (slot-value process-info (closer-mop:slot-definition-name slot-defn))))
    python-process-info))

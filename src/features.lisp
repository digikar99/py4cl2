(in-package :py4cl2)

;; This variable is modified in do-after-load.lisp.
;; It is also modified from PYSTART function for determining the availability of numpy.

(defvar *internal-features* nil
  "A list of PY4CL2 features available on the system. (Support for :ARRAYS
requires numpy and is only determinable after python process has started.)

The list can include one or more of:

  :WITH-PYTHON-OUTPUT
  :TYPED-ARRAYS
  :FAST-LARGE-ARRAY-TRANSFER
  :INTERRUPT
")

(defun numpy-installed-p (&optional (python *python*))
  (handler-case (let ((*python* python))
                  (declare (special *python*)) ;; compilation order, may not be known
                  (pyexec "import numpy")
                  t)
    (pyerror (condition)
      (declare (ignore condition))
      nil)))

(defparameter *feature-exclusion-alist*
  `((:with-python-output        :ecl)
    (:interrupt                 :ecl :abcl :windows :os-windows)
    (:fast-large-array-transfer :ecl :abcl)
    (:typed-arrays              :abcl)))

(defvar *warn-on-unavailable-feature-usage* t
  "If non-NIL, then issues a WARNING on use of a feature unavailable on the platform.
See PY4CL2:*INTERNAL-FEATURES* for the list of available features.")

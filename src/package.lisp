
;;;; package.lisp

(defpackage #:py4cl2
  (:use #:cl #:iterate #:alexandria)
  (:import-from #:uiop/launch-program
                #:process-info
                #:process)
  (:shadowing-import-from #:iterate #:as #:for)
  (:export ; python-process
   #:python-process-startup-error
   #:*additional-init-codes*
   #:pystart
   #:with-python-output
   #:pystop
   #:python-alive-p
   #:python-start-if-not-alive
   #:pyinterrupt)
  (:export ; writer
   #:pythonize)
  (:export ; reader
   #:python-object
   #:*print-python-object*
   #:python-object-type)
  (:export ; callpython
   #:pyerror
   #:python-eof-but-alive
   #:python-eof-and-dead
   #:raw-pyeval
   #:raw-pyexec
   #:pyeval
   #:pyexec
   #:pycall
   #:pymethod
   #:pygenerator
   #:pyslot-value
   #:pyversion-info
   #:pyhelp
   #:chain
   #:chain*
   #:@
   #:with-remote-objects
   #:with-remote-objects*)
  (:export ; import-export
   #:pymethod-list
   #:pyslot-list
   #:defpyfun
   #:defpymodule
   #:*defpymodule-silent-p*
   #:defpyfuns
   #:export-function)
  (:export ; lisp-classes
   #:python-getattr
   #:python-setattr)
  (:export ; config
   #:*config*
   #:*lispifiers*
   #:with-lispifiers
   #:*pythonizers*
   #:with-pythonizers
   #:initialize
   #:save-config
   #:load-config
   #:config-var
   #:pycmd
   #:numpy-pickle-location
   #:numpy-pickle-lower-bound
   #:print-python-traceback
   #:py-cd)
  (:export
   #:*internal-features*
   #:*warn-on-unavailable-feature-usage*))

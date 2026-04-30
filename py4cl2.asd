(asdf:defsystem "py4cl2"
  :serial t
  :description "Some improvements over py4cl. py4cl is a library for interfacing with python libraries from common lisp, using streams to communicate with the python process.
Report the issues at https://github.com/digikar99/py4cl2/issues
 (More) Documentation is available at https://digikar99.github.io/py4cl2/"
  :author #.(concatenate 'string
                         "py4cl author: Ben Dudson <benjamin.dudson@york.ac.uk>"
                         (string #\newline)
                         "py4cl2 maintainer: Shubhamkar Ayare <shubhamayare@yahoo.co.in>")
  :license "MIT"
  :version "2.10.0"                  ; py4cl is assumed to be version 1
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-json"
               "closer-mop"
               "float-features"
               "iterate"
               "numpy-file-format"
               "parse-number"
               "trivial-garbage"
               "uiop")
  :pathname #P"src/"
  :components ((:static-file "python-code" :pathname #P"../py4cl.py")
               (:file "package")
               (:file "python-process-info" :depends-on ("package"))
               (:file "config"         :depends-on ("package"))
               (:file "features"       :depends-on ("package"))
               (:file "python-process" :depends-on ("package" "features"))
               (:file "reader"         :depends-on ("package" "python-process"))
               (:file "writer"         :depends-on ("package" "features"))
               (:file "lisp-classes"   :depends-on ("package"))
               (:file "callpython"     :depends-on ("reader"
                                                    "writer"
                                                    "python-process"))
               (:file "arg-list"       :depends-on ("package"
                                                    "callpython"))
               (:file "import-export"  :depends-on ("python-process"
                                                    "callpython"
                                                    "arg-list"))
               (:file "do-after-load"  :depends-on ("import-export"))
               (:static-file ".config" :pathname #P"../.config"))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (handler-case (let ((system (asdf:find-system "py4cl2-tests")))
                                    (asdf:test-system system))
                      (asdf:missing-component (condition)
                        (declare (ignore condition))
                        (format *error-output* "Please find the tests at ~A~%"
                                "https://github.com/digikar99/py4cl2-tests")
                        (format *error-output*
                                "If you have already set up the tests, then something is wrong,
as asdf was unable to find \"py4cl2-tests\".")))))

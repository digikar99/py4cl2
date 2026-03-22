>This is a fork of bendudson/py4cl.

# Github Pages

Detailed documentation is available on [github-pages](https://digikar99.github.io/py4cl2/) (could be a bit outdated).

## For MacOS Users

If you have bash 3.2 please upgrade it to a [more recent version](https://www.shell-tips.com/mac/upgrade-bash/#gsc.tab=0).

# Releases

Check the [Releases](https://github.com/digikar99/py4cl2/releases) section. That said, if you are looking for stability, look at py4cl and not py4cl2, at least not in 2021. You may use py4cl2 on use-and-throw projects, or use it without using "edgy" features (not yet classified).

# Contribution and PRs

Please test using [py4cl2-tests](https://github.com/digikar99/py4cl2-tests).

# Quick Demonstration

<img margin="auto" width="75%" src="./docs/readme_slime.png"></img>

```lisp
(ql:quickload :py4cl2 :silent t)

(py4cl2:defpymodule "numpy" nil :lisp-package "NP")
(py4cl2:defpymodule "scipy.integrate" nil :lisp-package "INTEGRATE")

;; Integrate some ODEs
(defparameter *data*
  (integrate:odeint 
   :func (lambda (y time) 
           (list (aref y 1)       ; dy[0]/dt = y[1]
                 (- (aref y 0)))) ; dy[1]/dt = -y[0]
   :y-0 #(1.0 0.0)   ; Initial state
   :t (np:linspace :start 0.0 :stop (* 2 pi) :num 20)))
                                        ; Vector of times

; (array-dimensions *data*) => (20 2)

;; Make a plot, save and show it in a window
(py4cl2:defpymodule "matplotlib.pyplot" nil :lisp-package "PLT")

(plt:plot *data*)
(plt:xlabel :xlabel "Time")
(plt:savefig "result.pdf")
(plt:show)
```

<img margin="auto" width="75%" src="./docs/readme_matplotlib.png"></img>

# Running py4cl2 in a main GUI loop

The example above with matplotlib was a static plot (no interactive zooming, no GUI).  To enable interactivity, the main thread of the Python process needs to be running a GUI main loop.  To do this, we can change the py4cl2 loop to not block and to be called regularly by the gui main loop.  To do so, see the example src/PyQt6_example.py where we create a matplotlib plot on the Qt6 backend.  To run it, you would do the following:
```lisp
  ;; so python can find the example module
  (py4cl2:pyexec (format nil "import sys; sys.path.insert(0, '~a')"
			 (directory-namestring
			  (asdf:component-pathname
			   (asdf:find-component :py4cl2 "python-code")))))
  ;; start the gui loop and a simple plot.
  (py4cl2::raw-py-exec/no-return "import PyQt6_example; PyQt6_example.start_app(try_process_message);")
  (py4cl2:pyeval "1 + 1") ;; still works despite GUI refreshing as needed
```

Great thanks to [Ben Dudson](https://github.com/bendudson) for starting this project, and documenting it enough to make it more-buildable!
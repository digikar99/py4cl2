# py4cl2

[Last update: v2.9.3]

## Introduction

[py4cl][bendudson/py4cl] is a package by Ben Dudson, aimed at making python libraries availble in Common Lisp using streams to communicate with a separate python process - this is the approach taken by [cl4py](https://github.com/marcoheisig/cl4py) and is
different to the CFFI approach used by [burgled-batteries](https://github.com/pinterface/burgled-batteries),
but has the same goal.

[py4cl2](https://github.com/digikar99/py4cl2) is intended to be an improvement over the original py4cl - importing of arguments posed a critical problem with backwards-compatibility. (See [Highlights and Limitations](#highlights-and-limitations-of-py4cl).)

Please report the issues on github: [py4cl2](https://github.com/digikar99/py4cl2/issues) or [py4cl](https://github.com/bendudson/py4cl/issues)).


## Highlights and Limitations of `py4cl`

- Speed: About 6500 `(pycall "int" "5")` instructions per second @ 1GHz intel 8750H.
This shouldn't be a bottleneck if you're planning to run "long" processes in python. (For example, deep learning :). )
- Virtual environments: [`pycmd`](#pycmd) (`*python-command*` in `py4cl`): Choose which python binary to use. Works with miniconda.
- Multiple python processes (not documented here) - parallel execution?
- CI for SBCL, CCL, and ECL
- v2.6.0 worked with ABCL 1.7.1 (Java 1.8) despite several limitations (see the documentation included in [that release](https://github.com/digikar99/py4cl2/releases/tag/v2.6.0)).
- While CI has not been set up for windows (a PR is welcome!), from v2.7.0, things will be tested locally before releasing.
- No support for inheriting python classes - should require MOP
- Mac users should update to a later version of bash. See [this stackoverflow question](https://stackoverflow.com/questions/32481734/shell-error-with-bash-bad-substitution-no-closing). Thanks to [byulparen](https://github.com/byulparan) for pointing out!
- Embeddable into lisp-image - the code from py4cl.py is copied into `*python-code*` and heredocs are used. This is made to happen for unix and not for windows; until someone gets into Windows heredocs. (Maintainer note: This entails not using single-quote character `'` in py4cl.py.) Also note that this still requires the developer (= py4cl2 user) to supply the python libraries and binaries along with the lisp image to the (very-)end-user.
- Improvements in large array transfer speed numpy-file-format (see [initialize](#initialize)); while this does not beat `remote-objects` in existence since `py4cl`, it does provide a faster way to send array data from lisp to python and can be beneficial while offloading the work to python process, eg. deep learning.
- Ability to interrupt the python process using [(pyinterrupt)](#pyinterrupt)

<div><img src="readme_slime.png" width="80%" style="margin:auto; display:block;"/></div>
<!-- ![slime-demo-image](readme_slime.png) -->

<br/>

## Upcoming Sources of Possible Code Breakage

- `*lispifiers*` and `with-lispifiers` symbols are currently unstable introduced in this very version. Avoid using in production code until stability.

### Changes over py4cl

<u>**Backwards Incompatible Changes**</u>

**Unavoidable**

- Arguments are imported; submodules can be imported with an option to [defpymodule](#defpymodule). However, this is only possible for python3. In fact, with python2's end-of-life, no support for python2 is provided even.
- Asynchronous Printing: use `(with-python-output &body body)` to capture python output; in py4cl, `(with-output-to-stream (*standard-output*) &body body)` works. The separate macro [with-python-output](#with-python-output) was necessitated due to the asyncronous printing in py4cl2.
- Semantics of `nil`: see [Type Mapping and Pythonize](#type-mapping-and-pythonize)

**Avoidable**

- Several (but not all) names have been shorted from `python-` to `py`; `remote-objects` have been changed to `with-remote-object(s)`. Personal preference for these names stems from:
    - `defpyfun/module` reminds of the equivalent in `burgled-batteries` and `cffi`
    - `py`names are shorter
    - `with-remote` seems more appropriate
    - `chain` and `chain*` with more "uniformity"

<u>**IMO Backwards Portable Changes**</u>

- `defpymodule` (previously `import-module`) works "as-expected" with asdf / `defpackage`
- Embedding py4cl.py into lisp image
- importing submodules
- process startup errors

<u>**Other Notes**</u>

- Argument ordering can be wrong with ABCL, CCL or  ECL. I've not used it extensively at anywhere other than SBCL. Basic tests concerning argument orders do work on ABCL, CCL and ECL; since, in most cases, you are good with keyword args. Early adopters are welcome :D!

<table id="feature-comparison">
<tr>
<th>Feature / Implementation (default: linux)</th>
<th>SBCL</th>
<th>SBCL (Windows)</th>
<th>CCL</th>
<th>ECL</th>
<th>ABCL</th>
</tr>
<tr>
<td>Basic Functionality</td>
<td>✓</td>
<td>✓</td>
<td>?</td>
<td>?</td>
<td>?</td>
</tr>
<tr>
<td>Interrupt</td>
<td>✓</td>
<td>✗</td>
<td>✓</td>
<td>✗</td>
<td>✗</td>
</tr>
<tr>
<tr>
<td>with-python-output</td>
<td>✓</td>
<td>✓</td>
<td>✓</td>
<td>✗</td>
<td>✓</td>
</tr>
<tr>
<td>array-element-type preservation</td>
<td>✓</td>
<td>✓</td>
<td>✓</td>
<td>✓</td>
<td>✗</td>
</tr>
<tr>
<td>Fast Large Array Transfer</td>
<td>✓</td>
<td>✓</td>
<td>✓</td>
<td>✗</td>
<td>✗</td>
</tr>
</table>

- See [Future Work](#future-work).

## Installation

### Dependencies

This fork is possible due to the following (and therefore, depends on):

On the CL side:

- alexandria
- bordeaux-threads
- cl-json
- trivial-garbage
- iterate
- numpy-file-format
- parse-number
- uiop (some implementations have an older version of uiop; support for `launch-program` is needed for asynchronous processes)

On python side:

- numpy (recommended for arrays)

(other packages should be available in a standard python distribution - tested with CPython.)

### Installation

Besides installing directly from [quicklisp](https://www.quicklisp.org/beta/), download the (latest) release from the [Releases](https://github.com/digikar99/py4cl2/releases) and untar/unzip into `~/quicklisp/local-projects/` or any other
location where it can be discovered by `quicklisp`:

```sh
wget -qO- https://github.com/digikar99/py4cl2/archive/v2.6.0.tar.gz | tar xvz - -C ~/quicklisp/local-projects
```

Load into REPL with
```lisp
(ql:quickload :py4cl2)
```

### Tests

See [py4cl2-tests](https://github.com/digikar99/py4cl2-tests)

### Setting up

#### initialize

On loading this library for the first time, run `initialize` and provide the necessary details.

```lisp
(py4cl2:initialize)
```

(You may want to note the printed information, about the location of config-file. Of course, you can call this function again, but be sure to refill the values.)

The library uses (temporary) pickled .npy files for transferring large numpy arrays efficiently
between lisp and python. This process is IO intensive, writing as much as 100MB or even a GB each time, and therefore, using a ram-disk is recommended for this purpose would be recommended. ([How to create a ram disk on Linux?](https://unix.stackexchange.com/questions/66329/creating-a-ram-disk-on-linux))

#### \*config\* / config-var

These values can also be accessed using `*config*` and `config-var`:

```lisp
CL-USER> py4cl2:*config*
((PY4CL2:PRINT-PYTHON-TRACEBACK . T)
 (PY4CL2:PYCMD . "/home/user/miniconda3/bin/python")
 (PY4CL2:NUMPY-PICKLE-LOCATION . "/home/user/ram-disk/_numpy_pickle.npy")
 (PY4CL2:NUMPY-PICKLE-LOWER-BOUND . 100000))
CL-USER> (py4cl2:config-var 'py4cl2:numpy-pickle-location)
"/home/user/ram-disk/_numpy_pickle.npy"
CL-USER> (setf (config-var 'py4cl2:pycmd) "python")
"python"
```

Complementary to `config-var` are `save-config` and `load-config`. The latter is called on startup, the config-file exists. `(setf config-var)` calls the former unless it is `pycmd`, as well as asks the python process to load the config, from the config file. (The exception for `pycmd` exists so as to let the users set up project-local environments.)

#### \*internal-features\* / \*warn-on-unavailable-feature-usage\*

This lists the features available on your system/implementation. Values in the list may include:

- `:ARRAYS`: requires numpy, and can be seen only after a call to `(pystart)`
- `:TYPED-ARRAYS`: requires support for specialized-arrays on lisp side; ABCL 1.7.1 does not provide this
- `:FAST-LARGE-ARRAY-TRANSFER`: requires support for [numpy-file-format](https://github.com/marcoheisig/numpy-file-format)
- `:INTERRUPT`: requires system support for SIGINT (and some more things not working on ECL)
- `:WITH-PYTHON-OUTPUT`

See also: the [feature comparison table](#feature-comparison).

Attempt to use a feature that is not available will signal a warning and has undefined consequences. To avoid the warning, set the value of `*warn-on-unavailable-feature-usage*` to `nil`.


## Examples and Documentation

```lisp
CL-USER> (use-package :py4cl2)
```

### Python Processes

It all starts with a python process (actually, more than one as well - however, this use hasn't been documented here.).

#### pycmd

```lisp
CL-USER> (config-var 'pycmd)
"python"
```

Also see [config-var](#config--config-var).

#### pyversion-info
```lisp
CL-USER> (pyversion-info)
(3 7 3 "final" 0)
```

#### pyinterrupt
`(pyinterrupt &optional process)`

A simple `C-c C-c` only interrupts the lisp process from slime - the python process keeps running. `(pyinterrupt)` can be used in these cases to send a SIGINT (2) to the python process.

Also note that if `pyinterrupt` is not called before sending the next form to `eval` or `exec`, the input-output would go out of sync. A known way to get out is to `(pystop)` the python-process.

Therefore, you may want to have `(pyinterrupt)` called on the reception of SIGINT in SLIME:

```lisp
(when (find-package :swank)
  (defvar swank-simple-break)
  (setf (fdefinition 'swank-simple-break)
        (fdefinition (find-symbol "SIMPLE-BREAK" :swank)))
  (defun swank:simple-break
      (&optional (datum "Interrupt from Emacs") &rest args)
    (py4cl2:pyinterrupt)
    (apply (fdefinition 'swank-simple-break) datum args)))
```

However, I have been unable to get the code to work (by adding to `do-after-load` with as well as without SLIME. Further, people may not like a library to fiddle with their environments - so it might be better to leave it up to the user to set it.

#### py-cd
`(py-cd path)`

Equivalent of `slime-cd`, since python is a separate process.

#### Other useful functions and variables

##### pystart
##### pystop
##### python-alive-p
##### python-start-if-not-alive
##### \*defpymodule-silent-p\*
##### \*print-python-object\*


### Doing arbitrary things in python

Unlike lisp, python (and most other languages) make a distinction between *statements* and *expressions*: see [Quora](https://www.quora.com/Whats-the-difference-between-a-statement-and-an-expression-in-Python-Why-is-print-%E2%80%98hi%E2%80%99-a-statement-while-other-functions-are-expressions) or [stackoverflow](https://stackoverflow.com/questions/4728073/what-is-the-difference-between-an-expression-and-a-statement-in-python).

A general rule of thumb from there is: if you can print it, or assign it to a variable, then it's an expression, otherwise it's a statement.

Both `pyeval` and `pyexec` take any type of arguments. The `arg` is `pythonize`d if the `arg` is not a `string`, or it is a `string` that can be read into a `real`.

#### raw-pyeval
`(raw-pyeval &rest strings)`

Concatenates the strings and sends them to the python process for `eval`uation. The concatenation should be a valid python expression. Returns the result of evaluating the expression.

#### raw-pyexec
`(raw-pyexec &rest strings)`

Concatenates the strings and sends them to the python process for `exec`uation. The concatenation should be a valid python statement. Returns nil.

Note that [one limitation of `pyexec` is that modules imported on the top-level (of python) are not available inside some things](https://stackoverflow.com/questions/12505047/in-python-why-doesnt-an-import-in-an-exec-in-a-function-work). These "some things" include functions.

The following should illustrate this point:
```lisp
CL-USER> (pyexec "import time")
NIL
CL-USER> (pyeval "time.time()")
1.5623434e9
CL-USER> (pyexec "
def foo():
  return time.time()")
NIL
CL-USER> (pyeval "foo()")
; Evaluation aborted on #<PYERROR {100C24DF03}> ;; says 'time' is not defined
CL-USER> (pyeval "time.time()")
1.5623434e9
```
THe workaround in this case is to `import` inside the `def`.

Often times, the two commands above would be tedious - since you'd need to convert objects into their string representations every time. To avoid this hassle, there are the following useful functions.

#### pyeval
`(pyeval &rest args)`

For python expressions
```lisp
CL-USER> (pyeval 4 "+" 3)
7
```

There's also `(setf pyeval)`, which unlike `(pyexec)`, can return non-`nil` values.

```lisp
CL-USER> (setf (pyeval "a") "5")
"5"
CL-USER> (pyeval "a")
"5"
```

`pyeval` (and `pyexec`) treats the string as a python string, if it can be parsed into a number.
In fact, in accordance with an internal function `pythonizep`.

```lisp
CL-USER> (pyeval "1.0")
"1.0"
CL-USER> (pyeval "hello")
; Evaluation aborted on #<PYERROR {1003AC0183}>.
```

See also [Doing arbitrary things in python](#doing-arbitrary-things-in-python).

#### pyexec
`(pyexec &rest args)`

For python statements
```lisp
CL-USER> (pyexec "
if True:
  print(5)
else:
  print(10)")
; 5
NIL
```
`pyexec` (and `pyeval`) treats the string as a python string, if it can be parsed into a number).
In fact, in accordance with an internal function `pythonizep`. (See [pyeval](#pyeval).)

See also [Doing arbitrary things in python](#doing-arbitrary-things-in-python) to learn about `pyeval` and `pyexec`.

### Defining python functions and modules

Rather, we define functions that call python functions.

Names are lispified by converting underscores hyphens, and converting CamelCase to camel-case. Also see [Name Mapping](#name-mapping).

#### defpyfun
```lisp
(defpyfun fun-name &optional pymodule-name &key
  (as fun-name) (lisp-fun-name (lispify-name as))
  (lisp-package *package*)
  (safety t))
```
`lisp-fun-name` is the name of the symbol that would be `fboundp`ed to the function [that calls the python function].

Example Usage:
```lisp
CL-USER> (defpyfun "Input" "keras.layers" :lisp-fun-name "INP")
INP

CL-USER> (inp :shape '(1 2))
##S(PY4CL2::PYTHON-OBJECT
   :TYPE "<class 'tensorflow.python.framework.ops.Tensor'>"
   :HANDLE 1849)
```

`safety` takes care to import the required function from the required module after python process restarts for some reason. However, this affects speed.

Refer `(describe 'defpyfun)`.

#### defpymodule
```lisp
(defpymodule (pymodule-name
              &optional (import-submodules nil)
              &key (cache t)
                (continue-ignoring-errors t)
                (lisp-package (lispify-name pymodule-name) lisp-package-supplied-p)
                (reload t)
                (recompile-on-change nil)
                (safety t)
                (silent *defpymodule-silent-p*)))
```

`lisp-package` is the name of the symbol that the package would be bound to.

Example Usage:
```lisp
CL-USER> (defpymodule "keras.layers" t :lisp-package "KL")
Defining KL for accessing python package kl...
Defining KL.ADVANCED-ACTIVATIONS for accessing python package kl.advanced_activations...
Defining KL.CONVOLUTIONAL for accessing python package kl.convolutional...
Defining KL.CONVOLUTIONAL-RECURRENT for accessing python package kl.convolutional_recurrent...
Defining KL.CORE for accessing python package kl.core...
Defining KL.CUDNN-RECURRENT for accessing python package kl.cudnn_recurrent...
Defining KL.EMBEDDINGS for accessing python package kl.embeddings...
Defining KL.LOCAL for accessing python package kl.local...
Defining KL.MERGE for accessing python package kl.merge...
Defining KL.NOISE for accessing python package kl.noise...
Defining KL.NORMALIZATION for accessing python package kl.normalization...
Defining KL.POOLING for accessing python package kl.pooling...
Defining KL.RECURRENT for accessing python package kl.recurrent...
Defining KL.WRAPPERS for accessing python package kl.wrappers...
T

CL-USER> (kl:input/1 :shape '(1 2))
#S(PY4CL2::PYTHON-OBJECT
   :TYPE "<class 'tensorflow.python.framework.ops.Tensor'>"
   :HANDLE 816)

CL-USER> (pycall (kl.advanced-activations:softmax/class :input-shape '(1 2))
                 (kl:input/1 :shape '(1 2)))
#S(PY4CL2::PYTHON-OBJECT
   :TYPE "<class 'tensorflow.python.framework.ops.Tensor'>"
   :HANDLE 144)
```

Note that unlike Common Lisp, python has a single namespace. Therefore, currently,
to call a callable (in Python) object, but not defined as a function in Common Lisp,
you'd need to use something like [pycall].

#### defpyfuns

(Undocumented here.)

### Customizing Type Mapping

#### \*lispifiers\*

> NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

Each entry in the alist `*lispifiers*` maps from a lisp-type to a single-argument lisp function. This function takes as input the "default" lisp objects and is expected to appropriately parse it to the corresponding lisp object.

#### with-lispifiers
```lisp
(with-lispifiers ((&rest overriding-lispifiers) &body body))
```

> NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

Each entry of `overriding-lispifiers` is a two-element list of the form

```
  (type lispifier)
```

Here, `type` is unevaluated, while `lispifier` will be evaluated; the `lispifier` is expected
to take a default-lispified object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

```lisp
  (pyeval "[1, 2, 3]") ;=> #(1 2 3) ; the default lispified object
  (with-lispifiers ((vector (lambda (x) (coerce (print x) 'list))))
    (print (pyeval "[1,2,3]"))
    (print (pyeval 5)))
  ; #(1 2 3) ; default lispified object
  ; (1 2 3)  ; coerced to LIST by the lispifier
  ; 5        ; lispifier uncalled for non-VECTOR
  5
```

#### \*pythonizers\*

> NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

Each entry in the alist `*pythonizers*` maps from a lisp-type to
a single-argument PYTHON-FUNCTION-DESIGNATOR. This python function takes as input the
"default" python objects and is expected to appropriately convert it to the corresponding
python object.

#### with-pythonizers

```lisp
(with-pythonizers (&rest overriding-pythonizers) &body body)
```

> NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

Each entry of `overriding-pythonizers` is a two-element list of the form

```
  (type pythonizer)
```

Here, `type` is unevaluated, while `pythonizer` will be evaluated; the `pythonizer` is expected
to take a default-pythonized object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

```lisp
  (pyeval "[1, 2, 3]") ;=> #(1 2 3) ; the default object
  (with-pythonizers ((vector "tuple"))
    (print (pyeval "[1,2,3]"))
    (print (pyeval 5)))
  ; #(1 2 3) ; default object
  ; (1 2 3)  ; coerced to tuple by the pythonizer, which then translates to list
  ; 5        ; pythonizer uncalled for non-VECTOR
  5
```

### Errors

#### pyerror

Signalled if there an error in a running python process

#### python-process-startup-error

Signalled if [pystart](#pystart) fails to start a python process

#### python-eof-and-dead

Signalled (from `dispatch-messages`) if the lisp process is unable to read/write from/to a python process and the process has died. This should provide a cleaner error than `CL:END-OF-FILE`.

#### python-eof-but-alive

Signalled (from `dispatch-messages`) if the lisp process is unable to read/write from/to a python process but the process is alive. This should provide a cleaner error than `CL:END-OF-FILE`.

### Using functions and methods

#### pycall
`(pycall fun-name &rest args)`

Equivalent to the lisp `(funcall function &rest arguments)`. Call a python (or lisp! See [generators and lambdas](#generators-and-lambdas)) function.

```lisp
CL-USER> (py4cl2:pycall "print" "hello")
;; hello
+py-none+
CL-USER> (py4cl2:pycall #'+ 2 3 1)
6
```

Note that `fun-name` can be a name (see [Name Mapping]), a function, or a [callable] python-object. See the example in [defpymodule](#defpymodule).

#### pymethod
`(pymethod obj method-name &rest args)`

`pymethod` always pythonizes; `method-name` is [name mapped to Python names][Name Mapping].

```lisp
SEQ2SEQ> (pymethod model 'summary) ;; for some "ready" model
__________________________________________________________________________________________________
Layer (type)                    Output Shape         Param #     Connected to
==================================================================================================
input_1 (InputLayer)            (None, None, 43)     0
__________________________________________________________________________________________________
input_2 (InputLayer)            (None, None, 64)     0
__________________________________________________________________________________________________
lstm_1 (LSTM)                   [(None, 256), (None, 307200      input_1[0][0]
__________________________________________________________________________________________________
lstm_2 (LSTM)                   [(None, None, 256),  328704      input_2[0][0]
                                                                 lstm_1[0][1]
                                                                 lstm_1[0][2]
__________________________________________________________________________________________________
dense_1 (Dense)                 (None, None, 64)     16448       lstm_2[0][0]
==================================================================================================
Total params: 652,352
Trainable params: 652,352
Non-trainable params: 0
__________________________________________________________________________________________________
+py-none+
```

See [pymethod-list](#pymethod-list).

#### pyslot-value
`(pyslot-value object slot-name)`

```lisp
CL-USER> (pyslot-value model 'input-shape)
#((NIL NIL 43) (NIL NIL 64))
```

See [pyslot-list](#pyslot-list)

Also see [Name Mapping].

#### export-function
`(export-funtion function python-name)`

Lisp functions can be made available to python code using `export-function`:
```lisp
(py4cl2:python-exec "from scipy.integrate import romberg")

(py4cl2:export-function (lambda (x) (/ (exp (- (* x x)))
                                      (sqrt pi))) "gaussian")

(py4cl2:python-eval "romberg(gaussian, 0.0, 1.0)") ; => 0.4213504
```

#### pyhelp
`(pyhelp python-object)`

Calls python's `help` function on `python-object`. (NOTE: some descriptions, especially
for modules, are too big to be transferred in a reasonable time.)

### Generators and Lambdas

#### pygenerator
`(pygenerator function stop-value)`

```lisp
CL-USER> (let ((a 0)) (defun foo () (incf a)))
FOO

CL-USER> (pyeval "[x for x in " (pygenerator #'foo 3) "]")
#(1 2)
```

#### lambdas

Lisp functions are `pythonize`d to `LispCallbackObject`s. As the name suggests, python can call LispCallbackObjects (and therefore, lisp functions), just like it is any other python callable (which it is!).

```lisp
CL-USER> (py4cl2::pythonize #'car)
"_py4cl_LispCallbackObject(4)"

CL-USER> (pycall (lambda (string) (concatenate 'string string " - from Lisp"))
                 "hello")
"hello - from Lisp"
```

### Slot and Method Lists

Currently, all the python objects are grouped under the class `python-object`. The list of methods
and slots associated with these objects can be obtained using the following two functions.

#### pyslot-list
`(pyslot-list python-object &key as-vector)`

```lisp
CL-USER> (defpyfun "Model" "keras")
NIL

CL-USER> (pyslot-list (model))
("__class__" "__delattr__" "__dict__" "__doc__" "__eq__" "__ge__"
 "__getattribute__" "__gt__" "__hash__" "__le__" "__lt__" "__module__" "__ne__"
 "__repr__" "__str__" "__weakref__" "_built" "_expects_training_arg"
 "_inbound_nodes" "_initial_weights" "_is_compiled" "_is_graph_network"
 "_layers" "_losses" "_outbound_nodes" "_per_input_losses" "_per_input_updates"
 "_updates" "_uses_inputs_arg" "built" "input_spec" "inputs" "layers" "losses"
 "name" "non_trainable_weights" "optimizer" "outputs" "state_updates"
 "stateful" "supports_masking" "trainable" "trainable_weights" "updates"
 "uses_learning_phase" "weights")

CL-USER> (pyeval (model) ".inputs")
+py-none+
```

Optionally, see [pyslot-value](#pyslot-value)

#### pymethod-list
`(pymethod-list python-object &key as-vector)`

```lisp
CL-USER> (pymethod-list (model))
("__call__" "__class__" "__delattr__" "__dir__" "__eq__" "__format__" "__ge__"
 "__getattribute__" "__getstate__" "__gt__" "__hash__" "__init__"
 "__init_subclass__" "__le__" "__lt__" "__ne__" "__new__" "__reduce__"
 "__reduce_ex__" "__repr__" "__setattr__" "__setstate__" "__sizeof__" "__str__"
 "__subclasshook__" "_add_inbound_node" "_base_init"
 "_check_trainable_weights_consistency" "_get_node_attribute_at_index"
 "_init_graph_network" "_init_subclassed_network" "_make_predict_function"
 "_make_test_function" "_make_train_function" "_node_key" "_set_inputs"
 "_standardize_user_data" "_updated_config" "_uses_dynamic_learning_phase"
 "add_loss" "add_update" "add_weight" "assert_input_compatibility" "build"
 "call" "compile" "compute_mask" "compute_output_shape" "count_params"
 "evaluate" "evaluate_generator" "fit" "fit_generator" "from_config"
 "get_config" "get_input_at" "get_input_mask_at" "get_input_shape_at"
 "get_layer" "get_losses_for" "get_output_at" "get_output_mask_at"
 "get_output_shape_at" "get_updates_for" "get_weights" "load_weights" "predict"
 "predict_generator" "predict_on_batch" "reset_states" "run_internal_graph"
 "save" "save_weights" "set_weights" "summary" "test_on_batch" "to_json"
 "to_yaml" "train_on_batch")
```
Optionally, see [pymethod](#pymethod).

### chain(*)
`(chain &rest chain)`

This is inspired by the `chain` in parenscript, discussed in [this issue](https://github.com/bendudson/py4cl/issues/4).

In python it is quite common to apply a chain of method calls, data
member access, and indexing operations to an object. To make this work
smoothly in Lisp, there is the `chain` macro (Thanks to @kat-co and
[parenscript](https://common-lisp.net/project/parenscript/reference.html)
for the inspiration). This consists of a target object,
followed by a chain of operations to apply.  For example
```lisp
(chain "hello {0}" (format "world") (capitalize)) ; => "Hello world"
```
which is converted to python `return "hello {0}".format("world").capitalize()`.

`chain` has two variants: `chain` is a macro, and `chain*` is a function.

A few examples are as follows:

```lisp
(chain (slice 3) stop) ; => 3
(let ((format-str "hello {0}")
      (argument "world"))
 (py4cl2:chain* format-str `(format ,argument))) ; => "hello world"
```

Arguments to methods are lisp, since only the top level forms in `chain` are treated specially:
```lisp
CL-USER> (chain (slice 3) stop)
3
CL-USER> (let ((format-str "hello {0}")
               (argument "world"))
           (chain* format-str `(format ,argument)))
"hello world"
CL-USER> (chain* "result: {0}" `(format ,(+ 1 2)))
"result: 3"
CL-USER> (chain (aref "hello" 4))
"o"
CL-USER> (chain (aref "hello" (slice 2 4)))
"ll"
CL-USER> (chain (aref #2A((1 2 3) (4 5 6)) (slice 0 2)))
#2A((1 2 3) (4 5 6))
CL-USER> (chain (aref #2A((1 2 3) (4 5 6))  1 (slice 0 2)))
#(4 5)
CL-USER> (pyexec "class TestClass:
      def doThing(self, value = 42):
        return value")
CL-USER> (chain ("TestClass") ("doThing" :value 31))
31
```

There is also `(setf chain)`:

```lisp
CL-USER> (pyeval
          (with-remote-object (array (np:zeros '(2 2)))
            (setf (chain* `(aref ,array 0 1)) 1.0
                  (chain* `(aref ,array 1 0)) -1.0)
            array))
#2A((0.0 1.0) (-1.0 0.0))
```

Note that this modifies the value in python, so the above example only
works because =array= is a handle to a python object, rather than an
array which is stored in lisp. The following therefore does not work:

```lisp
CL-USER> (let ((array (np:zeros '(2 2))))
           (setf (chain* `(aref ,array 0 1)) 1.0
                 (chain* `(aref ,array 1 0)) -1.0)
           array)
#2A((0.0 0.0) (0.0 0.0))
```

### with-remote-objects(*)
`(with-remote-objects &body body)

If a sequence of python functions and methods are being used to manipulate data,
then data may be passed between python and lisp. This is fine for small amounts
of data, but inefficient for large datasets.

The `with-remote-objects` and `with-remote-objects*` macros provide `unwind-protect` environments
in which all python functions return handles rather than values to lisp. This enables
python functions to be combined without transferring much data.

```lisp
(with-remote-objects (py4cl2:python-eval "1+2"))
; => #S(PY4CL2::PYTHON-OBJECT :TYPE "<class 'int'>" :HANDLE 0)
```

`with-remote-objects*` evaluates the last result, instead of merely returning a handle

```lisp
(with-remote-objects* (py4cl2:python-eval "1+2")) ; => 3
```

The advantage comes when dealing with large arrays or other datasets:
```lisp
CL-USER> (time (let ((arr (make-array 1000000
                                      :element-type 'single-float
                                      :initial-element 2.0)))
                 (np:sum (np:add arr arr))))
;  0.258 seconds of real time
;  8,065,504 bytes consed
4000000.0
CL-USER> (time (with-remote-objects
                 (let ((arr (make-array 1000000
                                        :element-type 'single-float
                                        :initial-element 2.0)))
                   (np:sum (np:add arr arr)))))
;  0.100 seconds of real time
;  4,065,456 bytes consed
4000000.0
```
Note that this requires you to solely use python functions and methods. So, do not expect something like this to work:

```lisp
(with-remote-objects (print (aref (np:ones :shape '(10000000)) 0)))
; Error
```

to work.

Besides this, see [Setting up](#setting-up) for using ram-disk and `numpy-file-format` to
combine lisp and python functions.
.

### python-getattr
`(python-getattr object slot-name)`

Lisp structs and class objects can be passed to python, put into data structures and
returned:

```lisp
(defpyfun "dict") ; Makes python dictionaries

(defstruct test-struct
    x y)

(let ((map (dict :key (make-test-struct :x 1 :y 2))))  ; Make a dictionary, return as hash-map
  ;; Get the struct from the hash-map, and get the Y slot
  (test-struct-y
    (chain* `(aref ,map "key"))))  ; => 2
```


In python this is handled using an object of class `UnknownLispObject`, which
contains a handle. The lisp object is stored in a hash map
`*lisp-objects*`. When the python object is deleted, a message is sent to remove
the object from the hash map.

To enable python to access slots, or call methods on a struct or class, a
handler function needs to be registered. This is done by providing a method
for generic function `python-getattr`. This function will be called when a
python function attempts to access attributes of an object (`__getattr__`
method).

```lisp
;; Define a class with some slots
(defclass test-class ()
  ((value :initarg :value)))

;; Define a method to handle calls from python
(defmethod python-getattr ((object test-class) slot-name)
  (cond
    ((string= slot-name "value") ; data member
      (slot-value object 'value))
    ((string= slot-name "func")  ; method, return a function
      (lambda (arg) (* 2 arg)))
    (t (call-next-method)))) ; Otherwise go to next method

(let ((instance (make-instance 'test-class :value 21)))
  ;; Get the value from the slot, call the method
  ;; python: instance.func(instance.value)
  (chain* `((@ ,instance func) (@ ,instance value))))  ; => 42
```
Inheritance then works as usual with CLOS methods:
```lisp
;; Class inheriting from test-class
(defclass child-class (test-class)
  ((other :initarg :other)))

;; Define method which passes to the next method if slot not recognised
(defmethod py4cl2:python-getattr ((object child-class) slot-name)
  (cond
    ((string= slot-name "other")
     (slot-value object 'other))
    (t (call-next-method))))

(let ((object (make-instance 'child-class :value 42 :other 3)))
  (list
    (chain* object 'value) ; Call TEST-CLASS getattr method via CALL-NEXT-METHOD
    (chain* object 'other))) ;=> (42 3)
```

### python-setattr

## Type Mapping and Pythonize

Data is passed between python and lisp as text. The python function
`lispify` converts values to a form which can be read by the lisp
reader; the lisp function `pythonize` outputs strings which can be
`eval`'d in python. The following type conversions are done:


```
| Lisp type               | Python type           |
|-------------------------+-----------------------|
| NIL                     | False                 |
| integer                 | int                   |
| ratio                   | fraction.Fractions    |
| real                    | float                 |
| complex                 | complex float         |
| string                  | str                   |
| hash map                | dict                  |
| list                    | tuple                 |
| unspecialized vector    | list                  |
| (un)specialized array   | NumPy array*          |
| single-float            | numpy.float32         |
| double-float            | float                 |
| symbol                  | Symbol class          |
| function                | function              |
```

\*The currently supported numpy types include: `((un)signed-byte XX)` where XX can be from `(08 16 32 64)`, and arrays of `bit`s. Raise an [issue](https://github.com/digikar99/py4cl2/issues) if you want more types supported or this functionality exposed to the user.

Special conversion rules include:

```
nil              False
t                True
+py-none+        None
+py-empty-tuple+ ()        # empty tuple
```


Because `pyeval` and `pyexec` evaluate strings as python
expressions, strings passed to them are not escaped or converted as
other types are. To pass a string to python as an argument, call `py4cl2:pythonize`

```lisp
CL-USER> (py4cl2:pythonize "string")
"\"string\""
CL-USER> (py4cl2:pythonize #'identity)
"_py4cl_LispCallbackObject(1)"
CL-USER> (py4cl2:pythonize 3.0)
"3.0"
CL-USER> (py4cl2:pythonize (model)) ;; keras.Model
"_py4cl_objects[1918]"
```

If python objects cannot be converted into a lisp value, then they are
stored and a handle is returned to lisp. This handle can be used to
manipulate the object, and when it is garbage collected the python
object is also deleted (using the [trivial-garbage](https://common-lisp.net/project/trivial-garbage/)
package).

### Customizing Type-Mapping

> Unstable feature

Since version 2.8.0, values returned from the python process are wrapped inside a call to `py4cl2::customize`. This function essentially does the following:

```lisp
(defun customize (object)
  (loop :for (type . lispifier) :in *lispifiers*
        :if (typep object type)
          :do (return-from customize (funcall lispifier object)))
  object)
```

`with-lispifiers` provides a convenient wrapper to bind the dynamic variable `*lispifiers*` for executing its `body`.

## Name Mapping

The arguments passed to `pycall` are parsed as follows: the lisp keywords are converted to their python equivalents. This only entails downcasing the symbol-name of the keywords and replacing hyphens with underscores. If the symbol-name contained capital letters, then, if all the letters are capitals, the symbol-name is downcased; else it stays as it is

```lisp
CL-USER> (pyexec "
def foo(A, b):
  return True")
CL-USER> (pycall 'foo :*A* 4 :b 3)
T
CL-USER> (pycall 'foo :a 4 :b 3)
; Evaluation aborted on #<PYERROR {100E2AF473}>.
;; unexpected keyword argument 'a'
CL-USER> (pycall 'foo 4 3)
T
```
Lispfication of python names is done by `defpyfun`, in import-export.lisp. Both `CamelCase` and `joint_words` are converted to `camel-case` and `joint-words`; the actual names of the arguments are substituted:

```lisp
CL-USER> (macroexpand-1 '(defpyfun "foo"))
(DEFUN FOO (&KEY (A 'NIL) (B 'NIL))
  "None"
  NIL
  (PYTHON-START-IF-NOT-ALIVE)
  (RAW-PYEVAL "foo" "(" "A" "=" (PY4CL2::PYTHONIZE A) "," "b" "="
              (PY4CL2::PYTHONIZE B) "," ")"))
T
```

The format of the calling expression does depend on the signature of the function.



## What remains?

Feel free to create an [Issue on Github](https://github.com/digikar99/py4cl2/issues).

### Future Work

In no order of priority:

- adding/documenting proper multithreaded support
- [ABANDON since python2 has reached end-of-life] finding equivalent of inspect._empty in python2 (unable to google)
- importing python classes, and methods, may be, as subclasses
  of 'python-object; to be able to use make-instance and slot-value
  might require knowledge of MOP, to make python-object at the same level
  as standard-object
- should return value of defpyfun matter - so as to indicate success or failure?
  failure is anyways indicated by errors
- ability to define customized arg-lists, documentation, and calling methods
  for functions: this can serve as a community project to cover up some
  naming and arg-list idiosyncrasies
- cleaning up documentation while defining functions - many python functions
  have documentation intended for use directly in md/rst files


## Also check out

### [The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/)

[tCLC]: https://github.com/LispCookbook/cl-cookbook
[pycall]: #pycall
[Name Mapping]: #name-mapping
[bendudson/py4cl]: https://github.com/bendudson/py4cl

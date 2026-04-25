# Python interface for py4cl
#
# This code handles messages from lisp, marshals and unmarshals data,
# and defines classes which forward all interactions to lisp.
#
# Should work with python 2.7 or python 3

# Do NOT use single quote in this file.

from __future__ import print_function

import sys
import numbers
import itertools
import inspect
import json
import os
import signal
import traceback
import threading
import time

numpy_is_installed = False
try:
	# Use NumPy for multi-dimensional arrays
	import numpy
	numpy_is_installed = True
except:
	pass

return_stream = sys.stdout
output_stream = sys.stderr
sys.stdout = sys.stderr

config = {}

# eval_globals store the environment used
# when evaluating strings from Lisp
eval_globals = {
	"_py4cl_config_file_name" : ".config"
}

def load_config():
	config_file = sys.argv[1] + eval_globals["_py4cl_config_file_name"]
	if os.path.exists(config_file):
		with open(config_file) as conf:
			global config
			config = json.load(conf)
			for k in config:
				if config[k] == None:
					config[k] = False
			try:
				eval_globals["_py4cl_config"] = config
			except:
				pass
	else:
		print(eval_globals["_py4cl_config_file_name"], "file not found!")
		eval_globals["_py4cl_config"] = {}

load_config()

# Handle fractions (python Fraction -> Lisp RATIO)
# Lisp will pass strings containing "_py4cl_fraction(n,d)"
# where n and d are integers.
import fractions
eval_globals["_py4cl_fraction"] = fractions.Fraction

class Symbol(object):
	"""
	A wrapper around a string, representing a Lisp symbol.
	"""
	def __init__(self, name):
		self._name = name
	def __str__(self):
		return self._name
	def __repr__(self):
		return "Symbol({0})".format(self._name)

class LispCallbackObject (object):
	"""
	Represents a lisp function which can be called.

	An object is used rather than a lambda, so that the lifetime
	can be monitoried, and the function removed from a hash map
	"""
	lisp_callback_lock = threading.RLock()
	def __init__(self, handle):
		"""
		handle    A number, used to refer to the object in Lisp
		"""
		self.handle = handle

	def __del__(self):
		"""
		Delete this object, sending a message to Lisp
		"""
		send_value("d", self.handle)

	def __call__(self, *args, **kwargs):
		"""
		Call back to Lisp

		args   Arguments to be passed to the function
		"""
		global return_values

		# Convert kwargs into a sequence of ":keyword value" pairs
		# appended to the positional arguments
		allargs = args
		for key, value in kwargs.items():
			allargs += (Symbol(":"+str(key)), value)

		with LispCallbackObject.lisp_callback_lock:
			old_return_values = return_values # Save to restore after
			try:
				return_values = 0
				send_value("c", (self.handle, allargs))
			finally:
				return_values = old_return_values

			return try_process_message(blocking=True)


class UnknownLispObject (object):
	"""
	Represents an object in Lisp, which could not be converted to Python
	"""

	__during_init = True # Do not send changes during __init__

	def __init__(self, lisptype, handle):
		"""
		lisptype  A string describing the type. Mainly for debugging
		handle    A number, used to refer to the object in Lisp
		"""
		self.lisptype = lisptype
		self.handle = handle
		self.__during_init = False  # Further changes are sent to Lisp

	def __del__(self):
		"""
		Delete this object, sending a message to Lisp
		"""
		try:
			sys.stdout = return_stream
			send_value("d", self.handle)
		finally:
			sys.stdout = output_stream

	def __str__(self):
		return "UnknownLispObject(\"{0}\", {1})".format(self.lisptype, str(self.handle))

	def __getattr__(self, attr):
		# Check if there is a slot with this name
		try:
			sys.stdout = return_stream
			send_value("s", (self.handle, attr)) # Slot read
		finally:
			sys.stdout = output_stream

		# Wait for the result
		return try_process_message(blocking=True) #message_dispatch_loop()

	def __setattr__(self, attr, value):
		if self.__during_init:
			return object.__setattr__(self, attr, value)
		try:
			sys.stdout = return_stream
			send_value("S", (self.handle, attr, value)) # Slot write
		finally:
			sys.stdout = output_stream
		# Wait until finished, to syncronise
		return try_process_message(blocking=True) # message_dispatch_loop()

python_to_lisp_type = {
	bool       : "BOOLEAN",
	type(None) : "NULL",
	int        : "INTEGER",
	float      : "FLOAT",
	complex    : "COMPLEX",
	list       : "VECTOR",
	dict       : "HASH-TABLE",
	str        : "STRING",
}

try:
	python_to_lisp_type[inspect._empty] = "NIL"
except:
	pass

return_values = 0

##################################################################
# This code adapted from cl4py
#
# https://github.com/marcoheisig/cl4py
#
# Copyright (c) 2018  Marco Heisig <marco.heisig@fau.de>
#               2019  Ben Dudson <benjamin.dudson@york.ac.uk>

def lispify_dict(dict):
	segment_  = "(cl::setf (cl::gethash (cl::quote {}) table) (cl::quote {}))"
	segments  = [segment_.format(lispify(key), lispify(value)) for key, value in dict.items()]
	segment_0 = "#.(cl::let ((table (cl::make-hash-table :test (cl::quote cl::equal)))) "
	segment_1 = " ".join(segments)
	segment_2 = " table)"
	return segment_0 + segment_1 + segment_2

def lispify_tuple(tuple):
	if len(tuple) == 0:
		return "\"()\""
	else:
		return "(quote (" + " ".join(lispify(elt) for elt in tuple) + "))"

def lispify_infnan_if_needed (lispified_float):
	infnan = {"infd0" : "float-features:double-float-positive-infinity",
			  "-infd0": "float-features:double-float-negative-infinity",
			  "inf"   : "float-features:single-float-positive-infinity",
			  "-inf"  : "float-features:single-float-negative-infinity",
			  "nan"   : "float-features:single-float-nan",
			  "nand0" : "float-features:double-float-nan"}
	if lispified_float in infnan:
		return infnan[lispified_float]
	else:
		return lispified_float

def lispify_float (float):
	if "e" in str(float):
		lispified_float = str(float).replace("e", "d")
	else:
		lispified_float = str(float)+"d0"
	return lispify_infnan_if_needed(lispified_float)

def lispify_exception (obj):
	if config["printPythonTraceback"]:
		return "".join(traceback.format_exception(type(obj), obj, obj.__traceback__))
	else:
		return str(obj)

lispifiers = {
	bool              : lambda x: "T" if x else "NIL",
	type(None)        : lambda x: "\"None\"", # Better be "NIL"..?
	int               : str,
	fractions.Fraction: str,
	float             : lispify_float, # floats in python are double-floats of common-lisp
	complex           : lambda x: "#C(" + lispify(x.real) + " " + lispify(x.imag) + ")",
	list              : lambda x: "#(" + " ".join(lispify(elt) for elt in x) + ")",
	tuple             : lispify_tuple,
	dict              : lispify_dict,
	str               : lambda x: "\"" + x.replace("\\", "\\\\").replace("\"", "\\\"")  + "\"",
	type              : lambda x: "(quote " + python_to_lisp_type[x] + ")",
	Symbol            : str,
	UnknownLispObject : lambda x: "#.(py4cl2::lisp-object {})".format(x.handle),
}

if numpy_is_installed: #########################################################
	NUMPY_PICKLE_INDEX = 0 # optional increment in ndarray_lispifier and reset to 0

	def load_pickled_ndarray(filename):
		arr = numpy.load(filename, allow_pickle = True)
		return arr

	def delete_numpy_pickle_arrays():
		global NUMPY_PICKLE_INDEX
		while NUMPY_PICKLE_INDEX:
			NUMPY_PICKLE_INDEX -= 1
			numpy_pickle_location = config["numpyPickleLocation"] \
				+ ".from." + str(NUMPY_PICKLE_INDEX)
			if os.path.exists(numpy_pickle_location):
				os.remove(numpy_pickle_location)

	numpy_cl_type = {
		numpy.dtype("int64"): "(cl:quote (cl:signed-byte 64))",
		numpy.dtype("int32"): "(cl:quote (cl:signed-byte 32))",
		numpy.dtype("int16"): "(cl:quote (cl:signed-byte 16))",
		numpy.dtype("int8"):  "(cl:quote (cl:signed-byte 8))",
		numpy.dtype("uint64"): "(cl:quote (cl:unsigned-byte 64))",
		numpy.dtype("uint32"): "(cl:quote (cl:unsigned-byte 32))",
		numpy.dtype("uint16"): "(cl:quote (cl:unsigned-byte 16))",
		numpy.dtype("uint8"):  "(cl:quote (cl:unsigned-byte 8))",
		numpy.dtype("bool_"): "(cl:quote cl:bit)",
		numpy.dtype("float64"): "(cl:quote cl:double-float)",
		numpy.dtype("float32"): "(cl:quote cl:single-float)",
		numpy.dtype("object"): "cl:t",
	}

	def numpy_to_cl_type(numpy_type):
		try:
			return numpy_cl_type[numpy_type]
		except KeyError:
			raise Exception("Do not know how to convert {0} to CL.".format(str(numpy_type)))

	def lispify_ndarray (obj):
		"""Convert a NumPy array to a string which can be read by lisp
		Example:
		array([[1, 2],     => "#2A((1 2) (3 4))"
			   [3, 4]])
		"""
		global NUMPY_PICKLE_INDEX
		if "numpyPickleLowerBound" in config and \
		   "numpyPickleLocation" in config and \
		   obj.size >= config["numpyPickleLowerBound"]:
			numpy_pickle_location = config["numpyPickleLocation"] \
				+ ".from." + str(NUMPY_PICKLE_INDEX)
			NUMPY_PICKLE_INDEX += 1
			with open(numpy_pickle_location, "wb") as f:
				numpy.save(f, obj, allow_pickle = True)
			array = "#.(numpy-file-format:load-array \"" + numpy_pickle_location + "\")"
			return array
		if obj.ndim == 0:
			# Convert to scalar then lispify
			return lispify(obj.item())
		# First convert to 1d array, then ask lisp to reshape
		# FIXME: Will this play nice with both row-major and column-major arrays?
		initial_contents = "(cl:list {0})".format(
			" ".join(map(lispify, numpy.ndarray.flatten(obj)))
		)
		element_type = numpy_to_cl_type(obj.dtype)
		total_size = str(obj.size)
		dimensions = lispify(obj.shape)
		array_1d = "(cl:make-array {0} :element-type {1} :initial-contents {2})".format(
			total_size, element_type, initial_contents
		)
		displaced_array = """#.(cl:make-array (cl:quote {0}) :element-type {1}
		  :displaced-index-offset 0 :displaced-to {2})""".format(
			  dimensions, element_type, array_1d
		  )
		return displaced_array

	# Register the handler to convert Python -> Lisp strings
	#
	# The case for integers is handled inside lispify
	# function. At best, you would want a way to compare /
	# check for subtypes to avoid casing on u/int64/32/16/8.
	lispifiers.update({
		numpy.ndarray: lispify_ndarray,
		numpy.float64: lispify_float,
		numpy.float32: lambda x : lispify_infnan_if_needed(str(x)),
		numpy.bool_  : lambda x : "1" if x else "0"})
# end of "if numpy_is_installed" ###############################################

def handle_lispifier (obj):
	"""
	Store an object in a dictionary, and return a handle
	"""
	handle = next(python_handle)
	python_objects[handle] = obj
	return "#.(py4cl2::customize "                        + \
		"(py4cl2::make-python-object-finalize :type " + \
		"\"{0}\"".format(str(type(obj)))            + \
		" :handle {0}".format(str(handle))          + \
		"))"

def lispify(obj):
	"""
	Turn a python object into a string which can be parsed by Lisp reader.

	If return_values is false then always creates a handle
	"""
	if return_values > 0:
		if isinstance(obj, Exception): return str(obj)
		else: return handle_lispifier(obj)

	try:
		if isinstance(obj, Exception):
			return lispify_exception(obj)
		elif numpy_is_installed and isinstance(obj, numpy.integer):
			return str(obj)
		else:
			return lispifiers[type(obj)](obj)
	except KeyError:
		# Unknown type. Return a handle to a python object
		return handle_lispifier(obj)

def generator(function, stop_value):
	temp = None
	while True:
		temp = function()
		if temp == stop_value: break
		yield temp

##################################################################

def recv_string():
	"""
	Get a string from the input stream
	"""
	length = int(sys.stdin.readline())
	return sys.stdin.read(length)

def recv_value():
	"""
	Get a value from the input stream
	Return could be any type
	"""
	return eval(recv_string(), eval_globals)

send_value_lock = threading.RLock()
def send_value(cmd_type, value):
	"""
	Send a value to stdout as a string, with length of string first
	"""
	with send_value_lock:
		return_stream.write(cmd_type)
		try:
			# if type(value) == str and return_values > 0:
			# value_str = value # to handle stringified-errors along with remote-objects
			# else:
			value_str = lispify(value)
		except Exception as e:
			# At this point the message type has been sent,
			# so we cannot change to throw an exception/signal condition
			value_str = "Lispify error: {0}".format(lispify_exception(e))
		excess_char_count = (0 if os.name != "nt" else value_str.count("\n"))
		print(len(value_str)+excess_char_count, file = return_stream, flush=True)
		return_stream.write(value_str)
		return_stream.flush()

def return_value(value):
	"""
	Return value to lisp process, by writing to return_stream
	"""
	if isinstance(value, Exception):
		return return_error(value)
	# return_stream.flush() # TODO not sure
	send_value("r", value)

def return_error(error):
	send_value("e", error)

def pythonize(value): # assumes the symbol name is downcased by the lisp process
	"""
	Convertes the value (Symbol) to python conventioned strings.
	In particular, replaces "-" with "_"
	"""
	return str(value)[1:].replace("-", "_")

def message_dispatch_loop():
	while True:
		try_process_message(blocking=True)

def try_process_message(blocking=True):
	"""
	Wait for a message, dispatch on the type of message.
	Message types are determined by the first character:

	e  Evaluate an expression (expects string)
	x  Execute a statement (expects string)
	O  Enable handles
	o  Disable handles
	q  Quit
	"""
	global return_values  # Controls whether values or handles are returned
	busy_loop = 100
	while True:
		try:
			output_stream.flush()
			if not blocking:
				os.set_blocking(sys.stdin.fileno(), False)
			# Read command type
			cmd_type = sys.stdin.read(1)
			if cmd_type == "":
				if busy_loop > 0 and not blocking:
					busy_loop = busy_loop - 1
					continue;
				if not blocking:
					os.set_blocking(sys.stdin.fileno(), True)
				return None
			busy_loop = 100
			# It is possible that python would have finished sending the data to CL
			# but CL would still not have finished processing. We will receive further
			# instructions only after CL has finished processing, and therefore we can delete
			# the arrays. (TODO: But how does this happen with callbacks?)
			if numpy_is_installed: delete_numpy_pickle_arrays()

			if cmd_type == "e":  # Evaluate an expression
				expr = recv_string()
				# if expr not in cache:
				# print("Adding " + expr + " to cache")
				# cache[expr] = eval("lambda : " + expr, eval_globals)
				# result = cache[expr]()
				result = eval(expr, eval_globals)
				return_value(result)
			elif cmd_type == "x": # Execute a statement
				exec(recv_string(), eval_globals)
				return_value(None)
			elif cmd_type == "q":
				exit(0)
			elif cmd_type == "r": # return value from lisp function
				return recv_value() # break out of the while loop
			elif cmd_type == "O":  # Return only handles
				return_values += 1
			elif cmd_type == "o":  # Return values when possible (default)
				return_values -= 1
			else:
				return_error("Unknown message type \"{0}\".".format(cmd_type))
		except KeyboardInterrupt as e: # to catch SIGINT
			# output_stream.write("Python interrupted!\n")
			return_value(None)
		except Exception as e:
			return_error(e)

# Store for python objects which cannot be translated to Lisp objects
python_objects = {}
python_handle = itertools.count(0)

# For user to use to give time to our dispatch loop
eval_globals["try_process_message"] = try_process_message
# Make callback function accessible to evaluation
eval_globals["_py4cl_LispCallbackObject"] = LispCallbackObject
eval_globals["_py4cl_Symbol"] = Symbol
eval_globals["_py4cl_UnknownLispObject"] = UnknownLispObject
eval_globals["_py4cl_objects"] = python_objects
eval_globals["_py4cl_generator"] = generator
# These store the environment used when eval-ing strings from Lisp
eval_globals["_py4cl_config"] = config
eval_globals["_py4cl_load_config"] = load_config
if numpy_is_installed:
	# NumPy is used for Lisp -> Python conversion of multidimensional arrays
	eval_globals["_py4cl_numpy"] = numpy
	eval_globals["_py4cl_load_pickled_ndarray"] \
		= load_pickled_ndarray

# Lisp-side customize-able lispifiers
old_lispifiers = lispifiers.copy()
def customize_wrap_lispifier(lispifier):
	def _customize_wrap_lispifier(x):
		lispified_value = lispifier(x)
		return "#.(py4cl2::customize {0})".format(lispified_value)
	return _customize_wrap_lispifier

for key in lispifiers:
	lispifiers[key] = customize_wrap_lispifier(lispifiers[key])

async_results = {}  # Store for function results. Might be Exception
async_handle = itertools.count(0) # Running counter

# Main loop
message_dispatch_loop()

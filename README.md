
# sara.el

this provides method to parallel processing for emacs.
this not have some operator and functions, because this is a prototype.
I write the todos to *section of todo*.

# provide methods

* `(sara &rest body)`
    * this evaluate the argument of *body* in the forked process.
    this return a vector object that will use to any methods.
    forked process could not effect to the variable that in the parent process.
    in this version, argument of body cannot use the functions, macros and variables those in the parent process.

* `(sara-read)`
    * get a object from a parent process.

* `(sara-send sara object)`
    * send a *object* to an argument of *sara*.

* `(sara-set-onread sara func)`
    * set an event handler to argument of *sara*.
    that event handler will be called when received a object.

* `(sara-eof sara)`
    * close a process of argument of *sara*.
    if process was already closed, this dont close the process.

* `(sara-kill sara)`
    * send a kill signal to argument of *sara*.
    if process was already closed, this dont send a signal.
    
* `(sara-quit sara)`
    * send a quit signal to argument of *sara*.
    if process was already closed, this dont send a signal.

* `(sara-interrupt sara)`
    * send a intrrupt signal to argument of *sara*.
    if process was already closed, this dont send a signal.

* `(sara-stop sara)`
    * send a stop signal to argument of *sara*.
    if process was already closed, this dont send a signal.

* `(sara-continue sara)`
    * send a continue signal to argument of *sara*.
    if process was already closed, this dont send a signal.

# todo

1. I want to define a `wait` function.
2. when generate a source code, include the variables, functions and macros those are nessesary.

# license

this provide under the MIT License.
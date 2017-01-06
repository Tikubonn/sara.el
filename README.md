
# sara.el

this provides method to parallel processing for emacs.
this not have some operator and functions, because this is a prototype.
I write the todos to *section of todo*.

# provide methods

* `(sara &rest BODY)`
    * this evaluate the argument of *BODY* in the forked process.
    this return a vector object that will use to any methods.
    forked process could not effect to the variable that in the parent process.
    in this version, argument of BODY cannot use the functions, macros and variables those in the parent process.

* `(sara-read)`
    * get a object from a parent process.

* `(sara-send SARA OBJECT)`
    * send a *OBJECT* to an argument of *SARA*.

* `(sara-set-onread SARA FUNC)`
    * set an event handler to argument of *SARA*.
    that event handler will be called when received a object.

* `(sara-eof SARA)`
    * close a process of argument of *SARA*.

* `(sara-kill SARA)`
    * send a kill signal to argument of *SARA*.
    
* `(sara-quit SARA)`
    * send a quit signal to argument of *SARA*.

* `(sara-interrupt SARA)`
    * send a intrrupt signal to argument of *SARA*.

* `(sara-stop SARA)`
    * send a stop signal to argument of *SARA*.

* `(sara-continue SARA)`
    * send a continue signal to argument of *SARA*.

# todo

1. I want to define a `wait` function.
2. when generate a source code, include the variables, functions and macros those are nessesary.

# license

this provide under the MIT License.
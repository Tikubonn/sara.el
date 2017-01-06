
# sara.el

this provides method to parallel processing for emacs.
this not have some operator and functions, because this is a prototype.
I write the todos to *section of todo*.

# provide methods

* `(sara &rest body ... )`
    * argument of `body` are evaluated in the forked process.
    this return a vector object that will use to any methods.
    forked process could not effect to the variable that in the parent process.
    in this version, argument of `body` cannot use the functions, macros and variables those in the parent process.

* `(sara-read)`
    * get a object from a parent process.

* `(sara-send sara data)`
    * send a object to an argument of sara.

* `(sara-set-onread sara func)`
    * set an event handler to argument of sara.
    that event handler will be called when received a object.

# todo

1. I want to define a `wait` function.
1. when generate a source code, include the variables, functions and macros those are nessesary.

# license

this provide under the MIT License.

;; test code

(push (file-truename ".") load-path)

;; require some packages

(require 'cl)
(require 'sara-util)

;; define temp directory

(defvar sara-temp-directory
  (concat (directory-file-name user-emacs-directory) "/.sara"))

(defmacro sara-init-temp-directory ()
  `(progn
     (sara-make-temp-directory)
     (sara-cleanup-temp-directory)))

(defmacro sara-make-temp-directory ()
  `(when sara-temp-directory
     (unless (file-exists-p sara-temp-directory)
       (make-directory sara-temp-directory))))

(defmacro sara-cleanup-temp-directory ()
  `(when
     (and sara-temp-directory
       (file-exists-p sara-temp-directory))
     (dolist (filename (directory-files sara-temp-directory))
       (cond
         ((equal filename "."))
         ((equal filename ".."))
         (delete-file (sara-temp-file filename))))))

(defmacro sara-temp-file (file)
  `(concat (directory-file-name sara-temp-directory) "/" ,file))

(sara-init-temp-directory)

;; define internal variables

(defvar sara-alist nil)

(defmacro get-sara-alist (process)
  `(cdr (assq ,process sara-alist)))

(defmacro add-sara-alist (process sara)
  `(push (cons ,process ,sara) sara-alist))

;; define methods for sara structures

(defmacro make-sara (process) `(vector ,process ""))
(defmacro get-sara-process (sara) `(elt ,sara 0))
(defmacro get-sara-buffer (sara) `(elt ,sara 1))

;; define util methods

(defun sara-live-p (sara)

  "this return a boolean, that is process of sara was alive or not."
  
  (and (get-sara-process sara)
    (process-live-p (get-sara-process sara))))

(defmacro sara-when-process-alive (sara &rest rest)
  `(when (sara-live-p ,sara) ,@rest))

(defmacro sara-if-process-alive (sara then &rest  else)
  `(if (sara-live-p ,sara) ,then ,@else))

;; define internal methods

(defun sara-process-write (sara text)
  (sara-when-process-alive sara
    (process-send-string (get-sara-process sara) text)))

;; public methods for child process.

(defmacro sara-read ()
  `(read--expression ""))

(defmacro sara-exit ()
  `(kill-emacs))

;; public methods for parent process.

(defun sara-send (sara object)
  (sara-process-write sara (format "%S\n" object)))

(defun sara-set-onread (sara func)
  (sara-when-process-alive sara
    (set-process-filter
      (get-sara-process sara)
      (sara-set-onread-function func))))

(defun sara-set-onread--default (process text)
  (let ((sara (get-sara-alist process)))
    (when sara
      (setf (get-sara-buffer sara)
        (concat (get-sara-buffer sara) text)))))

;; define public process methods

(defun sara-eof (sara)

  "close a process of an argument of sara. 
if process was not alive, this not close the process."
  
   (prog1 sara
       (sara-when-process-alive sara
         (process-send-eof (get-sara-process sara)))))

(defun sara-kill (sara)

  "send a kill signal to an argument of sara.
if process was not alive, this not send a signal."
  
   (prog1 sara
       (sara-when-process-alive sara
         (kill-process (get-sara-process sara)))))

(defun sara-quit (sara)

  "send a quit signal to an argument of sara.
if process was not alive, this not send a signal."
  
   (prog1 sara
       (sara-when-process-alive sara
         (quit-process (get-sara-process sara)))))

(defun sara-interrupt (sara)

  "send a interrupt signal to an argument of sara.
if process was not alive, this not send a signal."
  
   (prog1 sara
       (sara-when-process-alive sara
         (interrupt-process (get-sara-process sara)))))

(defun sara-stop (sara)

  "send a stop signal to an argument of sara.
if process was not alive, this not send a signal."
  
   (prog1 sara
       (sara-when-process-alive sara
         (stop-process (get-sara-process sara)))))

(defun sara-continue (sara)

  "send a continue signal to an argument of sara.
if process was not alive, this not send a signal."
  
   (prog1 sara
       (sara-when-process-alive sara
         (continue-process (get-sara-process sara)))))

;; sara internal methods

(defmacro sara-temp-name ()
  `(concat "sara temp name #" (make-temp-name "")))

(defmacro sara-temp-buffer-name ()
  `(concat "sara temp buffer #" (make-temp-name "")))

(defmacro sara-temp-buffer ()
  `(get-buffer-create (sara-temp-buffer-name)))

(defmacro sara-temp-filename ()
  `(sara-temp-file (concat (make-temp-name "") ".el")))

(defmacro sara-load-formula (filename)
  `(format "(load %S nil t)" ,filename))

;; define macros

(defmacro with-sara (sara-name sara-formula &rest body)
  `(let ((,sara-name (sara ,sara-formula))) ,@body
     (sara-eof ,sara-name)
     (sara-kill ,sara-name)))

;; main

(defmacro sara (&rest body)
  (let*
    ((filename (sara-temp-filename))
      (tempbuff (sara-temp-buffer))
      (tempname (sara-temp-name))        
      (symprocess (gensym))
      (symsara (gensym)))
    (sara--write-file filename (mapcar 'macroexpand-all body))
    (sara--compile-file filename)    
    (let ((,symprocess (sara--open ,tempname ,tempbuffer ,filename))
           (,symsara (make-sara ,symprocess)))
      (prog1 ,symsara
        (sara-set-onread ,symsara 'sara-set-onread--default)
        (add-sara-alist ,symprocess ,symsara)))))

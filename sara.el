
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

(defmacro sara-live-p (sara)
  `(and (get-sara-process ,sara)
     (process-live-p (get-sara-process ,sara))))

(defmacro sara-when-process-alive (sara &rest rest)
  `(when (sara-live-p ,sara) ,@rest))

;; define internal methods

(defmacro sara-process-write (sara text)
  `(sara-when-process-alive ,sara
     (process-send-string (get-sara-process ,sara) ,text)))

;; define public io methods

(defmacro sara-read ()
  `(read--expression ""))

(defmacro sara-exit ()
  `(kill-emacs))

(defmacro sara-send (sara object)
  `(sara-process-write ,sara (format "%S\n" ,object)))

(defmacro sara-set-onread (sara func)
  `(sara-when-process-alive ,sara
     (set-process-filter
       (get-sara-process ,sara)
       (sara-set-onread-function ,func))))

(defmacro sara-set-onread-function (func)
  `(lambda (process text)
     (let ((sara (get-sara-alist process)))
       (when sara
         (setf (get-sara-buffer sara)
           (concat (get-sara-buffer sara) text))
         (while
           (let ((readone (ignore-errors (read-from-string (get-sara-buffer sara)))))             
             (when readone
               (setf (get-sara-buffer sara)
                 (subseq (get-sara-buffer sara) (cdr readone)))
               (funcall ,func (car readone)))))))))

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

;; main

(defmacro sara (&rest body)
  (let*
    ((filename (sara-temp-filename))
      (tempbuff (sara-temp-buffer))
      (tempname (sara-temp-name))
      (formula (sara-load-formula filename))        
      (symprocess (gensym))
      (symsara (gensym)))
    `(progn
       (sara-write-file ,filename ,@(mapcar 'macroexpand-all body))
       (sara-compile-file ,filename)
       (let*
         ((,symprocess (sara-run ,tempname ,tempbuff ,formula))
           (,symsara (make-sara ,symprocess)))
         (prog1 ,symsara (add-sara-alist ,symprocess ,symsara))))))

(defmacro sara-run (name buffer formula)
  `(start-process ,name ,buffer "emacs" "--quick" "--batch" "--eval" ,formula))

(defmacro sara-write-file (filename &rest body)
  (with-current-buffer
    (find-file filename)
    (dolist (form body)
      (print form (current-buffer)))
    (save-buffer)
    (kill-buffer)))

(defmacro sara-compile-file (filename)
  (byte-compile-file filename))

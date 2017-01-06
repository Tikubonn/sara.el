
;; provide sara-util.el

(provide 'sara-util)

;; define sara 

(defmacro sara-set-onread--function (func)
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

;; define sara utils

(defun sara--write-file (filename formula)
  (with-current-buffer
    (find-file filename)
    (dolist (form formula)
      (print form (current-buffer)))
    (save-buffer)
    (kill-buffer)))

(defun sara--compile-file (filename)
  (byte-compile-file filename))

(defun sara--open (name buffer filename)
  (let ((expression (format "(load %S nil t nil nil)" filename)))
    (start-process name buffer "emacs" "--quick" "--batch" "--eval" expression)))

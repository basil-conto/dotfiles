;; -*- lexical-binding: t -*-

(require 'notifications)
(eval-when-compile
  (require 'cl-lib))

(defmacro blc-randr-with-proc (sym &rest body)
  "Bind SYM to a `call-process'-like function and evaluate BODY."
  (declare (debug (symbolp body)) (indent 1))
  `(with-temp-buffer
     (condition-case err
         (cl-flet ((,sym (prog &rest args)
                     (let ((exit (save-excursion
                                   (apply #'call-process prog nil t nil args))))
                       (unless (eql exit 0)
                         (let ((cmd (string-join (cons prog args) " ")))
                           (signal 'error (list cmd exit)))))))
           ,@body
           (,sym "~/.fehbg"))
       (:success 0)
       (error
        (let ((msg (error-message-string err))
              (num (car (last err))))
          (notifications-notify :title "Error" :body msg)
          (message "%s\nBuffer contents:\n%s" msg (buffer-string))
          (if (numberp num) num 1))))))

(rx-define blc-randr-output
  (: bol (group (+ (not blank))) " connected"))

(rx-define blc-randr-resolution
  (: bol (+ blank) (group (+ digit) ?x (+ digit))))

(defun blc-randr-mode (x y)
  "Return \"xrandr\" mode string \"XxY\"."
  (format "%dx%d" x y))

(defun blc-randr-resolutions (call)
  "CALL \"xrandr\" and return alist of outputs with resolutions."
  (funcall call "xrandr" "-q")
  (let (outs)
    (while (re-search-forward (rx blc-randr-output) nil 'move)
      (let ((out (match-string 1)) res)
        (while (progn (forward-line) (looking-at (rx blc-randr-resolution)))
          (push (match-string 1) res))
        (push (cons out (nreverse res)) outs)))
    (nreverse outs)))

(provide 'blc-randr)

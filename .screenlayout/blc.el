;; -*- lexical-binding: t -*-

(require 'notifications)
(eval-when-compile
  (require 'cl-lib))

(defmacro blc-with-call (sym &rest body)
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
           ,@body)
       (:success 0)
       (error
        (let ((msg (error-message-string err))
              (num (car (last err))))
          (notifications-notify :title "Error" :body msg)
          (message "%s\nBuffer contents:\n%s" msg (buffer-string))
          (if (numberp num) num 1))))))

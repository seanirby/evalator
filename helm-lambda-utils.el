;; Utility funcitons 

(defun helm-lambda-utils-get-file-string (filepath)
  "Returns contents of file at filepath"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(provide 'helm-lambda-utils)

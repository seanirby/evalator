;; Utility funcitons 

(defun evalator-utils-get-file-string (filepath)
  "Returns contents of file at filepath"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(provide 'evalator-utils)

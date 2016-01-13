(defun evalator-utils-put! (plst k v)
  (setq plst (plist-put plst k v)))

(defun evalator-utils-get-file-string (filepath)
  "Returns contents of file at filepath"
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(provide 'evalator-utils)

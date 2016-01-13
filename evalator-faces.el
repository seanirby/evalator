(defgroup evalator-faces nil
  "Customize the appearance of evalator."
  :prefix "evalator-"
  :group 'faces
  :group 'evalator)

(defface evalator-success
  '((((background dark))
     :background "green4"
     :foreground "white"
     )
    (((background light))
     :background "green4"
     :foreground "white"
     ))
  "Face for source header in the evalator buffer."
  :group 'evalator-faces)

(defface evalator-error
  '((((background dark))
     :background "red4"
     :foreground "white"
     )
    (((background light))
     :background "red4"
     :foreground "white"
     ))
  "Face for source header in the evalator buffer."
  :group 'evalator-faces)

(provide 'evalator-faces)

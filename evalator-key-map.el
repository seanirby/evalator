(require 'helm)

(defvar evalator-key-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "RET") 'evalator-action-confirm)
    (define-key map (kbd "C-j") 'evalator-action-next)
    (define-key map (kbd "C-l") 'evalator-action-previous)
    (define-key map (kbd "C-;") 'evalator-action-insert-arg)
    map))

(provide 'evalator-key-map)

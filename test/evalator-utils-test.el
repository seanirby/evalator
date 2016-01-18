(require 'evalator-utils)
(require 'noflet)

(ert-deftest evalator-utils-put!-test ()
  (let ((plst '(:foo :bar)))
    (evalator-utils-put! plst :foo :baz)
    (should (equal '(:foo :baz) plst))))

(ert-deftest evalator-utils-get-file-string ()
  (noflet ((insert-file-contents (filepath) (insert "foo")))
    (should (equal "foo" (evalator-utils-get-file-string "/dummy/file/path")))))
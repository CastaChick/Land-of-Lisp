;;;; About lambda
;; function
(defun half (n)
  (/ n 2))

;; lambda returns function as object
(lambda (n) (/ n 2)); => <FUNCTION :LAMBDA ...>
(mapcar (lambda (n) (/ n 2)) '(2 4 6)); => (1 2 3)

(defun sum-list (list)
  (if list
      (+ (car list) (sum-list (cdr list)))
    0))
    
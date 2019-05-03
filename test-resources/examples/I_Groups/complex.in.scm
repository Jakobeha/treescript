((lambda (f) (f 2 3))
 ((lambda (f x x) ((f x) (f x)))
  (lambda (x y) (list x y 4))))

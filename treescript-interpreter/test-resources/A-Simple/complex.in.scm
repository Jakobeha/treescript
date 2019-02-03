((lambda (f) ((f 2) 3))
 ((lambda (f) (lambda (x) (lambda (x) ((f x) (f x)))))
  (lambda (x) (lambda (y) (((list x) y) 4)))))

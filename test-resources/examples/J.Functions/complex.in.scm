((lambda (f) (list (f 1) (f 4) (f 7)))
 ((lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (lambda (y) ((x x) y))))))
   (lambda (fib)
    (lambda (n)
     (if ((< n) 2) 1 ((+ (fib ((+ n) -1))) (fib ((+ n) -2))))))))

((lambda (f) (list (f 3) (f 8) (f 12)))
 (((lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (lambda (y) ((x x) y))))))
   (lambda (fib)
    (lambda (n)
     (if (< n 2) 1 (+ (fib (- n 1) (- n 2)))))))))
(define (triple num)
  (+ (+ num num) num))

(define (map f x)
  (if (null? x)
    ()
    (cons (f (car x)) (map f (cdr x)))))

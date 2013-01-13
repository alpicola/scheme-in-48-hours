(define (fix f)
  (lambda (x) ((f (fix f)) x)))

(define fact
  (fix (lambda (f) (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))))

(display (fact 5))

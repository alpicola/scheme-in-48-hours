(define (fix f)
  (lambda (x) ((f (fix f)) x)))

(define fact
  (fix (lambda (f) (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))))

(display (fact 5))

(define (fold f acc l)
  (if (null? l)
      acc
      (fold f (f (car l) acc) (cdr l))))

(define (reverse l)
  (fold cons '() l))

(display (reverse '(1 2 3 4)))

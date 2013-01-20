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

(define (iota n)
  (define (iter n l)
    (if (= n 0)
        l
        (iter (- n 1) (cons n l))))
  (iter n '()))

(display (reverse (iota 10)))

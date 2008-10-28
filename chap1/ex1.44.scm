(define dx 0.00001)

(define (smooth f)
  (define (ave a b c) (/ (+ a b c) 3))
  (lambda (x)
    (ave (f (- x dx)) (f x) (f (+ x dx)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n)
        result
        (repeated-iter (inc i) (compose f result))))
  (repeated-iter 1 f))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

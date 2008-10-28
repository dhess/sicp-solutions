(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n)
        result
        (repeated-iter (inc i) (compose f result))))
  (repeated-iter 1 f))

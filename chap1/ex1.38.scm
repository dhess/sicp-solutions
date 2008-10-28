(define (d i)
  (let ((rem (remainder i 3)))
    (if (or (= rem 0) (= rem 1))
        1
        (* 2 (+ 1 (quotient i 3))))))

(define (cont-frac n d k)
  (define (cont-frac-iter k result)
    (if (= k 0)
        result
        (cont-frac-iter (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-iter k 0))

(define (approximate-e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
             d
             k)))

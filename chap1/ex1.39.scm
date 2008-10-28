(define (cont-frac n d k)
  (define (cont-frac-iter k result)
    (if (= k 0)
        result
        (cont-frac-iter (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-iter k 0))

(define (square x) (* x x))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

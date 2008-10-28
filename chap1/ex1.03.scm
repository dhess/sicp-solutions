(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (>= x y)
  (not (< x y)))

(define (sum-of-squares-of-two-largest x y z)
  (cond
   ((and (>= x y) (>= y z)) (sum-of-squares x y))
   ((and (>= y x) (>= x z)) (sum-of-squares x y))
   ((and (>= x y) (>= z y)) (sum-of-squares x z))
   (else (sum-of-squares y z))))

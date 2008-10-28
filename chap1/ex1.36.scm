(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess guess-num)
    (let ((next (f guess)))
      (display "guess ")
      (display guess-num)
      (display ": ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ guess-num 1)))))
  (try first-guess 1))

;; The solution to the fixed point of x |-> log(1000)/log(x):

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;; With average damping:

(define (average x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))

(define (repeated f n)
  (define (repeated-iter i result)
    (if (= i n)
        result
        (repeated-iter (inc i) (compose f result))))
  (repeated-iter 1 f))

(define (floor-log2 n)
  (define (iter m count)
    (if (= m 0)
        (- count 1)
        (iter (arithmetic-shift m -1) (inc count))))
  (iter n 0))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (nth-root x n)
  (let ((number-of-damps (floor-log2 n)))
    (fixed-point-of-transform (lambda (y) (/ x (fast-expt y (- n 1))))
                              (repeated average-damp number-of-damps)
                              1.0)))

;; Part a. Here's a definition of cont-frac which produces a recursive
;; process:

(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cont-frac-helper (+ i 1))))))
  (cont-frac-helper 1))

;; 1/phi is approximately 0.6180 (to 4 decimal places).  Our tolerance
;; is 0.0001.  We can find the minimum k which gets a good-enough
;; approximation using this procedure:

(define (find-minimum-k n d answer tolerance)
  (define (close-enough? guess)
    (< (abs (- guess answer)) tolerance))
  (define (iter k)
    (let ((guess (cont-frac n d k)))
      (if (close-enough? guess)
          k
          (iter (+ k 1)))))
  (iter 1))

;; Evaluate this combination to find the minimum k for the given
;; tolerance:

(find-minimum-k (lambda (i) 1.0)
                (lambda (i) 1.0)
                0.6180
                0.0001)

;; Part b. Here's a version which produces an iterative process:

(define (cont-frac n d k)
  (define (cont-frac-iter k result)
    (if (= k 0)
        result
        (cont-frac-iter (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-iter k 0))

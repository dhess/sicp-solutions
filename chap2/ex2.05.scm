(define (square x) (* x x))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (count-factors p n)
  (if (= (remainder p n) 0) (+ 1 (count-factors (/ p n) n))
      0))

(define (car p) (count-factors p 2))

(define (cdr p) (count-factors p 3))

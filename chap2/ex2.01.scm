(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (define (maker x y g)
    (cons (/ x g) (/ y g)))
  (let ((g (gcd (abs n) (abs d))))
    (cond ((and (< n 0) (< d 0)) (maker (abs n) (abs d) g))
          ((< d 0) (maker (- n) (abs d) g))
          (else (maker n d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

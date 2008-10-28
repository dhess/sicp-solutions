;; Here is a procedure to perform the Fermat test on all numbers a<n.
;; It returns #t if n passes the test, #f otherwise.

(define (complete-fermat-test n)
  (complete-fermat-test-helper n (- n 1)))

(define (complete-fermat-test-helper n a)
  (cond ((= a 0) #t)
        ((fermat-test n a) (fermat-test n (- a 1)))
        (else #f)))

(define (fermat-test n a)
  (= (expmod a n n) a))
  
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Here's the expmod procedure modified for the Miller-Rabin test.
; Note that 1 modulo n is 1 for all n > 1, 0 for n = 1.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; This helper procedure returns 0 if n is a non-trival square root of
; 1 modulo m, otherwise it returns the square of n modulo m.  This
; procedure is a lot easier to write efficiently if we cheat a bit and
; use the let special form, which isn't introduced until section
; 1.3.2.

(define (check-nontrivial-sqrt n m)
  (let ((x (remainder (square n) m)))
    (if (and (not (= n 1)) (not (= n (- m 1))) (= x 1))
        0
        x)))

; Here's a procedure which tests a number for primality via the
; Miller-Rabin test.  Note that the text states that one of the proofs
; only applies to odd numbers, so the prime test checks for even
; numbers (excepting 2, which is prime) before employing the
; Miller-Rabin test.

(define true #t)

(define false #f)

(define (miller-rabin-test n a)
  (= (expmod a (- n 1) n) 1))

(define (prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else
         (prime-helper n (- n 1)))))

(define (prime-helper n a)
  (cond ((= a 0) #t)
        ((miller-rabin-test n a) (miller-rabin-test n (- a 1)))
        (else #f)))

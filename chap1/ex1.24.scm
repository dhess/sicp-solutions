;; Here is the Fermat test, as given in the text (with a couple of
;; extra definitions to make the code portable to a few more Scheme
;; implementations):

(define true #t)

(define false #f)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Here is timed-prime-test from our answer to exercise 1.22, adapted
;; to use fast-prime?  See the notes in that answer for information
;; about this code and executing it on various Scheme implementations.
;;
;; Note: you may find that your particular Scheme implementation is
;; unable to compute large primes using the Fermat test; see
;;
;; http://wiki.drewhess.com/wiki/SICP_exercise_1.24
;;
;; for details.

(define runtime current-milliseconds)

(define (square x)
  (* x x))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " (")
  (display elapsed-time)
  (display "ms)\n")
  #t)

(define (search-for-primes a n)
  (search-for-primes-helper (next-odd a) 0 n))

(define (search-for-primes-helper a found n)
  (if (= found n)
      0
      (search-for-primes-helper (+ a 2)
                                (if (timed-prime-test a)
                                    (+ found 1)
                                    found)
                                n)))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      n))

(define (even? n)
  (= 0 (remainder n 2)))

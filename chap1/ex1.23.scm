;; Here is the original, inefficient version of smallest-divisor:

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; Here is the modified version (square, smallest-divisor and divides?
;; remain the same):

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; And here is the procedure timed-prime-test as we designed it in our
;; answer to exercise 1.22 (see that answer for details on the
;; differences between this timed-prime-test and the one described in
;; the text):

(define runtime current-milliseconds)

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
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

;; An even faster implementation that achieves the desired 2x speedup
;; over the original version; see
;;
;; http://wiki.drewhess.com/wiki/SICP_exercise_1.22
;;
;; for details.

(define (smallest-divisor n)
  (if (divides? n 2)
      2
      (find-divisor n 3)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

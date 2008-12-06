;;; Requires the PLT DrScheme graphical environment. Set the language
;;; to "Module".
#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split identity-op smaller-op)
  (define (rec-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec-split painter (- n 1))))
          (identity-op painter (smaller-op smaller smaller)))))
  rec-split)

(define right-split (split beside below))
(define up-split (split below beside))

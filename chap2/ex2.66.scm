(define false #f)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (key record) (car record))

(define (datum record) (cdr record))

(define (make-record key datum) (cons key datum))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((record (entry set-of-records)))
        (let ((k (key record)))
          (cond ((equal? given-key k) record)
                ((< given-key k)
                 (lookup given-key (left-branch set-of-records)))
                (else
                 (lookup given-key (right-branch set-of-records))))))))

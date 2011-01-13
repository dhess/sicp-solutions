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

;;; The following procedures are given only for creating databases and
;;; testing the solution given above. They're not part of the
;;; solution, proper.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define database
  (list->tree (list (make-record 1 'a)
                    (make-record 5 'e)
                    (make-record 10 'j)
                    (make-record 26 'z))))

(lookup 1 database)

(lookup 3 database)

(lookup 10 database)

(lookup 15 database)

(lookup 5 database)

(lookup 26 database)

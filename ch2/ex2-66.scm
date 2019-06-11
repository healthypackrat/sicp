(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

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

(define (key record) (car record))

(define (value record) (cadr record))

(define (make-record key value) (list key value))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((entry-of-record (entry set-of-records)))
        (let ((key-of-record (key entry-of-record)))
          (cond ((= given-key key-of-record) (value entry-of-record))
                ((< given-key key-of-record) (lookup given-key (left-branch set-of-records)))
                ((> given-key key-of-record) (lookup given-key (right-branch set-of-records))))))))

(define tree-of-records
  (list->tree
    (list
      (make-record 1 'alice)
      (make-record 2 'bob)
      (make-record 3 'chris)
      (make-record 4 'dave))))

(print (lookup 2 tree-of-records))
; bob

(print (lookup 99 tree-of-records))
; #f

; p.90
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; ex2-62.scm
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2 (union-set set1 (cdr set2)))))))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

; ex2-63.scm
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; ex2-64.scm
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

(define (union-set-tree tree1 tree2)
  (list->tree (union-set (tree->list-1 tree1)
                         (tree->list-1 tree2))))

(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set (tree->list-1 tree1)
                                (tree->list-1 tree2))))

(define tree1 (list->tree '(1 2 3 4 5)))

(define tree2 (list->tree '(2 4 6)))

(print tree1)
; (3
;  (1
;   ()
;   (2 () ()))
;  (4
;   ()
;   (5 () ())))

(print tree2)
; (4
;  (2 () ())
;  (6 () ()))

(print (union-set-tree tree1 tree2))
; (3
;  (1
;   ()
;   (2 () ()))
;  (5
;   (4 () ())
;   (6 () ())))

(print (intersection-set-tree tree1 tree2))
; (2
;  ()
;  (4 () ()))

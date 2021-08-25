#lang racket

(require (planet dyoo/simply-scheme))
;(require berkeley)
(provide (all-defined-out))

; Exercise 1 SICP 2.26
; What are the result of the expressions? Make sure to comment your answer out.

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)                            ; '(1 2 3 4 5 6)
(cons x y)                              ; '((1 2 3) 4 5 6))
(list x y)                              ; '((1 2 3) (4 5 6))

; Exercise 2 Mobile. SICP 2.29
;; A binary mobile consists of two branches, a left branch and a right branch.
;; Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile.
;; We can represent a binary mobile using compound data by constructing it from two branches
;; (for example, using list):
(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure,
;; which may be either a number (representing a simple weight) or another mobile:
(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile) (car mobile))
  ;; (error "Not yet implemented"))

(define (right-branch mobile) (cadr mobile))
  ;; (error "Not yet implemented"))

(define (branch-length branch) (car branch))
  ;; (error "Not yet implemented"))

(define (branch-structure branch) (cadr branch))
  ;; (error "Not yet implemented"))

; b. Define total-weight.

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+
       (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))))

        ;; (foldl + 0 (map fn branches))))

; c. Define balanced?
;; A mobile is said to be balanced if the torque applied by its top-left branch
;; is equal to that applied by its top-right branch
;; (that is, if the length of the left rod multiplied by the weight hanging from that rod
;; is equal to the corresponding product for the right side)
;; and if each of the submobiles hanging off its branches is balanced.
;; Design a predicate that tests whether a binary mobile is balanced.
(define (balanced? b-mobile)
  (let ((torque (lambda (b) (* (branch-length b) (total-weight (branch-structure b))))))
    (if (number? b-mobile)
        true
        (and
         (balanced? (branch-structure (left-branch b-mobile)))
         (balanced? (branch-structure (right-branch b-mobile)))
         (equal? (torque (left-branch b-mobile)) (torque (right-branch b-mobile)))))))
  ;; (error "Not yet implemented"))

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

; Exercise 3 SICP 2.30, 2.31
;; a. Define square-tree.
;; > (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

(define (square-tree d-l)
  (let ((sq (lambda (x) (* x x))))
    (cond ((number? d-l) (sq d-l))
          ((list? d-l) (map square-tree d-l)))))
  ;; (error "Not yet implemented"))

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  (if (list? tree)
      (map (lambda (x) (tree-map fn x)) tree)
      (fn tree)))
  ;; (error "Not yet implemented"))

;Exercise 4 -  SICP 2.36 Complete the definition of accumulate-n
;; The procedure accumulate-n is similar to accumulate
;; except that it takes as its third argument a sequence of sequences,
;; which are all assumed to have the same number of elements.
;; It applies the designated accumulation procedure to combine all the first elements
;; of the sequences, all the second elements of the sequences, and so on,
;; and returns a sequence of the results.
;; For instance, if s is a sequence containing four sequences,
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
;; should be the sequence(22 26 30).
;; Fill in the missing expressions in the following definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       '()
;;       (cons (accumulate op init <??>)
;;             (accumulate-n op init <??>))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.
;; Suppose we represent vectors v = (vi) as sequences of numbers,
;; and matrices m = (mi,j) as sequences of vectors (the rows of the matrix)
;; We can define the dot product as
(define (dot-product v w)
  (foldr + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
  ;; (map "YOUR CODE HERE" m))

(define (transpose mat)
  (accumulate-n cons '() mat))
  ;; (accumulate-n "YOUR CODE HERE" "YOUR CODE HERE" mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
    ;; (map "YOUR CODE HERE" m)))


;Exercise 6 - SICP 2.38
;; What are the values of the following:

(foldr / 1 (list 1 2 3))           ; 3/2
(foldl / 1 (list 1 2 3))           ; 3/2
(foldr list '() (list 1 2 3))      ; '(1 (2 (3 ())))
(foldl list '() (list 1 2 3))      ; '(3 (2 (1 ())))

#|
Give the property that op should satisfy to guarantee that
fold-right and fold-left will produce the same values for any sequence.

Your property here

Commutative property

|#

;Exercise 7 - Define equal? SICP 2.54
;; define equal? recursively in terms of the basic eq? equality of symbols
;; by saying that a and b are equal? if they are both symbols and the symbols are eq?
;; or if they are both lists such that
;; (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b).
(define (my-equal? l1 l2)
  (cond ((eq? l1 l2) true)
        ((null? l1) (null? l2))
        ((null? l2) false)
        ((and (list? l1) (list? l2)) (and (my-equal? (car l1) (car l2))
                                          (my-equal? (cdr l1) (cdr l2))))
        (else false)))
  ;; (error "Not yet implemented"))

;Exercise 8 - Complete the definition of subsets
;; if the set is (1 2 3), then the set of all subsets is
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
        ;; (append rest (map "YOUR CODE HERE" rest)))))


;Exercuse 9 - Modify the calc program

;; Extend calc.rkt to include words as data, providing
;; the operations first, butfirst, last, butlast, and word.
;; Unlike Racket, your calculator should treat words as self-evaluating expressions
;; except when seen as the operator of a compound expression.
;; That is, it should work like these examples:

#|
calc: foo
foo
calc: (first foo)
f
calc: (first (butfirst hello))
|#


; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
        ((symbol? exp) exp)
        (else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:
(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
        ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
                           ((= (length args) 1) (- (car args)))
                           (else (- (car args) (foldr + 0 (cdr args))))))
        ((eq? fn '*) (foldr * 1 args))
        ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
                           ((= (length args) 1) (/ (car args)))
                           (else (/ (car args) (foldr * 1 (cdr args))))))
        ((eq? fn 'first) (cond ((null? args) (error "Calc: no args to first"))
                               ((= (length args) 1) (first (car args)))))
        ((eq? fn 'butfirst) (cond ((null? args) (error "Calc: no args to butfirst"))
                                  ((= (length args) 1) (butfirst (car args)))))
        ((eq? fn 'bl) (cond ((null? args) (error "Calc: no args to bl"))
                            ((= (length args) 1) (bl (car args)))))
        ((eq? fn 'butlast) (cond ((null? args) (error "Calc: no args to butlast"))
                                 ((= (length args) 1) (butlast (car args)))))
        ((eq? fn 'last) (cond ((null? args) (error "Calc: no args to last"))
                              ((= (length args) 1) (last (car args)))))
        ((eq? fn 'bf) (cond ((null? args) (error "Calc: no args to bf"))
                              ((= (length args) 1) (bf (car args)))))
        ((eq? fn 'word) (foldr word "" args))
        (else (error "Calc: bad operator:" fn))))

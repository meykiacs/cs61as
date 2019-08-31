#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

;; as a set of arithmetic operations for combining ``intervals'' (objects that represent the range of possible values of an inexact quantity)
;; R = a/(1/R1 + 1/R2)
;; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))
; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
; SICP 2.10 - Modify div-interval

;; to consider dividing by zero
(define (div-interval x y)
  (if (>= (* (upper-bound y) (lower-bound y)) 0.)
      (mul-interval x 
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))
      (error "divide by zero")))

;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (cons (- c (/ (* c tol) 100)) (+ c (/ (* c tol) 100))))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))
; SICP 2.17 - Define last-pair
; Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))
; SICP 2.20 - Define same-parity
; write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

; -> (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; -> (same-parity 2 3 4 5 6 7)
; (2 4 6)

(define (same-parity x . y)
  (cons x 
        (if (even? x)
          (filter even? y)
          (filter odd? y))))

; SICP 2.22 - Write your explanation in the comment block:

#|
Louis Reasoner tries to rewrite the first square-list procedure of Exercise 2.21 so that it evolves an iterative process:
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

This doesn't work either. Explain.

Your explanation here
the first once cons the square of car of the rest with the answer from previous iteration

the second cons the answer from the previous iteration (which is a list) with with car of the rest 

|#

; Exercise 2 - Define my-substitute
; Write a procedure my-substitute that takes three arguments: a list, an old word, and a new word. It should return a copy of the list, but with every occurrence of the old word replaced by the new word, even in sublists. For example:

; -> (my-substitute '((lead guitar) (bass guitar) (rhythm guitar) drums)
                  ; 'guitar
                  ; 'axe)
; ((lead axe) (bass axe) (rhythm axe) drums)

(define (substitute lst old new)
  (if (null? lst) 
      '()
      (if (list? (car lst))
          (cons (substitute (car lst) old new) (substitute (cdr lst) old new))
          (if (equal? (car lst) old)
            (cons new (substitute (cdr lst) old new))
            (cons (car lst) (substitute (cdr lst) old new))))))
            
; Exercise 3 - Define my-substitute2
; Now write my-substitute2 that takes a list, a list of old words, and a list of new words; the last two lists should be the same length. It should return a copy of the first argument, but with each word that occurs in the second argument replaced by the corresponding word of the third argument:

; -> (my-substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
                  ;  '(1 2 3 4)
                  ;  '(one two three four))
; ((four calling birds) (three french hens) (two turtle doves))

(define (substitute2 lst old new)
  (define (helper lst1 old1 new1)
    (if (null? lst1)
        '()
        (if (list? (car lst1))
            (cons (helper (car lst1) old new) (helper (cdr lst1) old new))
            (if (null? old1)
              (cons (car lst1) (helper (cdr lst1) old new))
              (if (equal? (car lst1) (car old1))
                (cons (car new1) (helper (cdr lst1) old new))
                (helper lst1 (cdr old1) (cdr new1)))))))
  (helper lst old new))

#|
Extra for Experts
Do these if you want an extra challenge. These are not for credit.

Exercise 4
Write the procedure cxr-function that takes as its argument a word starting with c, ending with r, and having a string of letters a and/or d in between, such as cdddadaadar. It should return the corresponding function.

Exercise 5
SICP Ex. 2.6. Besides addition, invent multiplication and exponentiation of nonnegative integers. If you're really enthusiastic, see if you can invent subtraction. (Remember, the rule of this game is that you have only lambda as a starting point.) Read ~cs61as/lib/church-hint for some suggestions.

Exercise 6
SICP Ex. 2.18; this should take some thought, and you should make sure you get it right, but don't get stuck on it for a whole hour. Note: Your solution should reverse lists, not sentences! That is, you should be using cons, car, and cdr, not first, sentence, etc.

|#
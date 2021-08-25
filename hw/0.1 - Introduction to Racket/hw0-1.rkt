#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
;; (require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 - Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?

2) What is your major?

3) Are you a returning student? (i.e. Did you take 61AS last semester?)

4) What made you to take 61AS?

5) Tell us interesting things about yourself.

|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))
(define (sum-of-squares a b)
  (+ (square a) (square b)))

;; Exercise 2a - Define can-drive
(define (can-drive age)
  (if (< age 16)
      '(Not yet)
      '(Good to go)))

;; Exercise 2b - Define fizzbuzz
(define (fizzbuzz num)
  (cond ((and (= (remainder num 3) 0) (= (remainder num 5) 0)) 'fizzbuzz)
         ((= (remainder num 3) 0) 'fizz)
         ((= (remainder num 5) 0) 'buzz)
         (else num)))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

;; Exercise 4 - new-if vs if
(define (infinite-loop) (infinite-loop))

;; (if (= 3 6)
;;   (infinite-loop)
;;   (/ 4 2))

(define (new-if test then-case else-case)
  (if test
    then-case
    else-case))

; (new-if (= 3 6)
;   (infinite-loop)
;   (/ 4 2))

; (new-if (= 3 6)
;   (/ 1 0)
;   (/ 4 2))


#|
Your answer here

Applicative order for new-if.  if is a special form and it short circuits
|#

#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed
; (dupls-removed '(a b c a e d e b))
;; This should output (c a d e b)
(define (dupls-removed sent)
  (cond ((empty? sent) '())
        ((member? (first sent) (bf sent)) (dupls-removed (bf sent)))
        (else (se (first sent) (dupls-removed (bf sent))))))

; Exercise 2 - Define count-word
;; (count-word '(i really really like 61as) 'really)
;; This should output 2
(define (count-word sent wd)
  (cond ((empty? sent) 0)
        ((equal? wd (first sent)) (+ 1 (count-word (bf sent) wd)))
        (else (count-word (bf sent) wd))))

; Exercise 3
(define (new-if test then-case else-case)
  (if test
    then-case
    else-case))

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here
  the procedure steps into an infinite loop because it evaluates
  the third argument as a recursive call agian and again
|#

; Exercise 4 - Define squares
;; This should output (1 4 9)
;; (squares '(1 2 3))
(define (squares sent)
  (if (empty? sent) '()
                    (se (* (first sent) (first sent)) (squares (bf sent)))))

; Exercise 5 - Define switch
;; This should output (I told you that you should wake me up)
;; (switch '(you told me that I should wake you up))
(define (switch sent)
  (define (general-switch my-sent)
            (cond ((empty? my-sent) '())
                  ((or (equal? (first my-sent) 'I) (equal? (first my-sent) 'me))
                   (se 'you (general-switch (bf my-sent))))
                  ((equal? (first my-sent) 'you)
                   (se 'me (general-switch (bf my-sent))))
                  (else (se (first my-sent) (general-switch (bf my-sent))))))

  (if (equal? (first sent) 'you)
                    (se 'I (general-switch (bf sent)))
                    (general-switch sent)))

; Exercise 6 - Define ordered?
;; (ordered? '(1 2 3)) ; #t
;; (ordered? '(2 1 3)) ; #f
;; (ordered? '(2)) ; #t
(define (ordered? sent)
  (cond ((empty? (bf sent)) #t)
        ((> (first sent) (first (bf sent))) #f)
        (else (ordered? (bf sent)))))

; Exercise 7 - Define ends-e
;; This should output (please the above the blue)
;; (ends-e '(please put the salami above the blue elephant))
(define (ends-e sent)
  (cond ((empty? sent) '())
        ((equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))))

; Exercise 8
;;Devise a test that will tell you whether Racket's and and or are special
;; forms or ordinary functions.
;;Why might it be advantageous for an interpreter to treat or as a special form
;; Can you think of reasons why it might be advantageous to treat or as an ordinary function?
#|
Your explanation here
;; (or #t (/ 1 0))
;; (and #f (/ 1 0))

;; (or (empty? nums) (empty? (bf nums)))
;; (and (integer? x) (even? x))


;; benefits
;; 1: getting desired values instead of #t or #f
;; 2: avoid unnecessay or wrong computation
;; 3: avoid else for a more specific condition
;; disadvantages
;; 1: the next expression needs to be evaluated

|#

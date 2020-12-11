#lang racket

(require berkeley)
(provide (all-defined-out))


; SICP 3.3, 3.4 - Modify make-password-account to make it generate
; password-protected accounts.
; Also, if an incorrect password is given 7 times consecutively, you
; should say (call-the-cops).
; Note: In the case of a wrong password, you should return the string
; "Incorrect password".  Do not use display or print or error.
;; example
;; (define acc (make-password-account 100 'secret-password))

;; ((acc 'secret-password 'withdraw) 40)
;; 60

;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"

(define (make-password-account balance pass)
  (define past-incorrect? false)
  (define incorrect-number 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (wrong-pass-handler amount)   ; function added by me
    (set! past-incorrect? true)
    (set! incorrect-number (+ 1 incorrect-number))
    (if (>= incorrect-number 7)
        '(call the cops)
        "Incorrect password"))
  (define (dispatch p m)                ;line edited for it needs a second argument
    (if (not (eq? p pass))              ; password checking and setting local vars
        wrong-pass-handler
        (begin
          (set! past-incorrect? false)
          (set! incorrect-number 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))))
        dispatch)

; SICP 3.7 - Define make-joint.
;; Define a procedure make-joint that should take three arguments.
;; the first is a password-protected account.
;; the second argument must match the password with which the
;; account was defined in order for the make-joint operation to proceed.
;; the third argument is a new password. make-joint is to create an additional access
;; to the original account using the new password.
;; For example, if peter-acc is a bank account with password open-sesame, then
;; (define paul-acc
;;   (make-joint peter-acc 'open-sesame 'rosebud))
;; will allow one to make transactions on peter-acc using the
;; name paul-acc and the password rosebud . You may wish
;; to modify your solution to Exercise 3.3 to accommodate this
;; new feature.
;; Define a procedure make-
;; joint that accomplishes this. make-joint should take three
;; arguments.
; You may want to modify make-password-account, but you shouldn't
; remove any of its previous functionality.

;; YOUR CODE HERE

(define (make-joint acc old-pass new-pass)
  (define (dispatch p m)
    (if (eq? p new-pass)
        (acc old-pass m)
        (acc (word old-pass 'make-it-erong) m)))
  (if (number? ((acc old-pass 'deposit) 0))
      dispatch
      (error "Incorrect password")))

; SICP 3.8 - Define reset-f!

;; THE ONLY POINT IS THE ORDER OF EVALUATING ARGS MATTERS IF STATE CHANGES

;; Define a simple procedure f such that evaluating
;; (+ (f 0) (f 1))
;; will return 0 if the arguments to + are evaluated from left to
;; right but will return 1 if the arguments are evaluated from right to left.

; This is a function that defines the function f that the exercise
; asks for.
;; (define f #f)

;; (define (reset-f!)
;;   (set! f ??))

; For example, if you think that f should be the square function, you
; would say:
; (define (reset-f!)
;   (set! f (lambda (x) (* x x))))

;; f is a function that changes
;; it evaluates to its argument, at the first state
;; it evaluates to zero at the second state


(define f
   (let ((state 0))
     (lambda (x)
       (if (= state 0)
           (begin (set! state 1)
                  x)
           (begin (set! state 0)
                  0)))))

; SICP 3.10 - Answer in the comment block.
#|
You have two options:
1) Draw the environment diagram here using ASCII art
2) Submit a .jpg or .pdf file containing your environment diagram (for
example, you could scan a paper drawing), in which case you should
write the filename here.

Environment diagram here

Q. How do the environment structures differ for the two versions?
A.

Q. Show that the two versions of make-withdraw create objects with the
same behavior.
A.

|#

; SICP 3.11 - Answer in the comment block.
#|
Same options as in 3.10

Environment diagram here

Q. Where is the local state for acc kept?
A.

Q. How are the local states for the two accounts kept distinct?
A.

Q. Which parts of the environment structure are shared?
A.

|#

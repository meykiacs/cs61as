(load "simply.scm")
(load "obj.scm")

; 1 - Modify the person class
;; to add a repeat method, which repeats the last thing said.
(define-class (person name)
  (instance-vars (last-stuff '()))      ; line added
  (method (say stuff)
          (set! last-stuff stuff)       ; line added
          stuff)
  (method (repeat) (ask self 'say last-stuff)) ;line added
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name))))


;; 2 - Determine which definition works as intended.
;; In particular, make sure the repeat method works.

;; They all work for their purpose but some change the behaviour of the parent


;;  infinite loop
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat))) )

;;  repeat always gives empty sentence (not calling ususal)
;; (define-class (double-talker name)
;;   (parent (person name))
;;   (method (say stuff) (se stuff stuff)) )

;; this one: (repeat) doubles the previous stuff
;; (define-class (double-talker name)
;;   (parent (person name))
;;   (method (say stuff) (usual 'say (se stuff stuff))) )

#|
Definition number ?? works as intended.
Your explanation here.
|#


; 3 - Write the random-generator class.
;; as follows
;; (define r10 (instantiate random-generator 10))
;; (ask r10 'number) ; will return a random number between 0 and 9
;; (ask r10 'number) ;  return the number of random numbers r10 has created

(define-class (random-generator range)
	(class-vars (count 0))
	(method (number)
					(set! count (+ count 1))
					(random range)))

; 4 - Write the coke-machine class.
; For compatibility with the autograder, make sure that you display
; error messages.  That means you should say:
; (display "Not enough money") and
; (display "Machine empty") when appropriate.

;; example
;; machine that can hold 80 Cokes and sells them for 70 cents each
;; (define my-machine (instantiate coke-machine 80 70))
;; > (ask my-machine 'fill 60)
;; > (ask my-machine 'deposit 25)
;; > (ask my-machine 'coke)
;; "Not enough money"

;; > (ask my-machine 'deposit 25) ;; Now there's 50 cents in there.
;; > (ask my-machine 'deposit 25) ;; Now there's 75 cents.
;; > (ask my-machine 'coke)
;; 5 ;; return val is 5 cents change.

;; YOUR CODE HERE
(define-class (coke-machine capacity price)
	(instance-vars (holds 0) (money 0))
	(method (fill number) (if (<= (+ holds number) capacity)
														(set! holds (+ holds number))
														("More than capacity")))
	(method (deposit m) (set! money (+ money m)))
	(method (coke) (cond ((< money price) (display "Not enough money"))
											 ((= holds 0) (display "Machine empty"))
											 (else (begin
															 (set! holds (- holds 1))
															 (- money price))))))

;; 5 - Write the deck class.
;; A deck object responds to two messages: deal and empty?.
;; It responds to "deal" by returning the top card of the deck,
;; after removing that card from the deck,
;; if the deck is empty, it responds to deal by returning '().
;; It responds to empty? by returning #t or #f, according to whether all cards have been dealt.
;; Write a class definition for deck.
;; When instantiated, a deck object should contain a shuffled deck of 52 cards.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; (define ordered-deck
;;   (accumulate append '()
;; 	      (map (lambda (suit)
;; 		     (map (lambda (value) (word value suit))
;; 			  '(A 2 3 4 5 6 7 8 9 10 J Q K)))
;; 		   '(s d c h))))

;; or
(define ordered-deck
	(flatmap
	 (lambda (suit)
		 (map (lambda (value) (word value suit))
					'(A 2 3 4 5 6 7 8 9 10 J Q K)))
	 '(s d c h)))

(define (nth counter lst)
  (cond ((null? lst) (error 'nth "index out of bounds"))
        ((= counter 0) (car lst))
        (else (nth (- counter 1) (cdr lst) ))))

(define delete!
  (lambda (item list)
    (cond
     ((equal? item (car list)) (cdr list))
     (else (cons (car list) (delete! item (cdr list)))))))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
	(cons card (shuffle (delete! card deck))) )))

;; YOUR CODE HERE
(define-class (deck)
	(instance-vars (cards (shuffle ordered-deck)))
	(method (deal)
					(if (ask self 'empty?)
							'()
							(let ((top (car cards)))
								(begin
									(set! cards (cdr cards))
									top))))
	(method (empty?) (null? cards)))


; 6 - Write the miss-manners class.
;; Write a class miss-manners that takes an object as its instantiation argument.
;; The new miss-manners object should accept only one message, namely please.
;; The arguments to the please message should be
;; first, a message understood by the original object
;; and second, an argument to that message.
;; (Assume that all messages to the original object require exactly one additional argument.)

;; Here is an example using the person class from the upcoming adventure game project:
;; > (define BH (instantiate person 'Brian BH-office))
;; BH
;; > (ask BH 'go 'down)
;; BRIAN MOVED FROM BH-OFFICE TO SODA
;; > (define fussy-BH (instantiate miss-manners BH))
;; > (ask fussy-BH 'go 'east)
;; ERROR: NO METHOD GO
;; > (ask fussy-BH 'please 'go 'east)
;; BRIAN MOVED FROM SODA TO PSL

;; YOUR CODE HERE
(define-class (miss-manners obj)
	(method (please msg arg)
					(ask obj msg arg)))

;; 7
(define-class (worker)
	(method (work)
					'whistle-while-you-work ))

(define-class (TA)
	(parent (worker))
	(method (work)
					(usual 'work)
					'(Let me help you with that box and pointer diagram)))

(define-class (singer)
	(parent (worker)))

(define-class (singer-TA)
	(parent (singer) (TA)) )

(define-class (TA-singer)
	(parent (TA) (singer)) )

;; > (define Matt (instantiate singer-TA))
;; > (define Chris (instantiate TA-singer))

;; > (ask Matt 'work)
;; WHISTLE-WHILE-YOU-WORK
;; > (ask Chris 'work)
;; (LET ME HELP YOU WITH THAT BOX AND POINTER DIAGRAM)

;; Matt is primarily a singer , so he responds to the work message
;; as a singer would.
;; Chris , however, is primarily a TA , and uses the work method from the TA class.

;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (place name)
	(instance-vars
	 (directions-and-neighbors '())
	 (things '())
	 (people '())
	 (entry-procs '())
	 (exit-procs '()))
	(method (type) 'place)
	(method (neighbors) (map cdr directions-and-neighbors))
	(method (exits) (map car directions-and-neighbors))
	(method (look-in direction)
					(let ((pair (assoc direction directions-and-neighbors)))
						(if (not pair)
								'()                     ;; nothing in that direction
								(cdr pair))))           ;; return the place object
	(method (appear new-thing)
					(if (memq new-thing things)
							(error "Thing already in this place" (list name new-thing)))
					(set! things (cons new-thing things))
					'appeared)
	(method (enter new-person)
					(if (memq new-person people)
							(error "Person already in this place" (list name new-person)))

					;; ------------------------------------------------------------------
					;; answer for QA4 part 1
					(for-each (lambda (p) (ask p 'notice new-person)) people)
					;; ------------------------------------------------------------------

					(set! people (cons new-person people))
					(for-each (lambda (proc) (proc)) entry-procs)
					'appeared)

	(method (gone thing)
					(if (not (memq thing things))
							(error "Disappearing thing not here" (list name thing)))
					(set! things (delete thing things))
					'disappeared)
	(method (exit person)
					(for-each (lambda (proc) (proc)) exit-procs)
					(if (not (memq person people))
							(error "Disappearing person not here" (list name person)))
					(set! people (delete person people))
					'disappeared)
	(method (new-neighbor direction neighbor)
					(if (assoc direction directions-and-neighbors)
							(error "Direction already assigned a neighbor" (list name direction)))
					(set! directions-and-neighbors
								(cons (cons direction neighbor) directions-and-neighbors))
					'connected)
	(method (add-entry-procedure proc)
					(set! entry-procs (cons proc entry-procs)))
	(method (add-exit-procedure proc)
					(set! exit-procs (cons proc exit-procs)))
	(method (remove-entry-procedure proc)
					(set! entry-procs (delete proc entry-procs)))
	(method (remove-exit-procedure proc)
					(set! exit-procs (delete proc exit-procs)))
	(method (clear-all-procs)
					(set! exit-procs '())
					(set! entry-procs '())
					'cleared)
;;  ------------------------------------------------------------------
	;; answer QA4 part 2
	(method (may-enter? person) #t))

(define-class (locked-place name)
	(parent (place name))
	(instance-vars
	 (locked? #t))
	(method (may-enter? person)
					(not locked?))
	(method (unlock)
					(set! locked? #f)))
;;  ------------------------------------------------------------------


(define-class (person name place)
	(instance-vars
	 (possessions '())
	 (saying ""))
	(initialize
	 (ask place 'enter self))
	(method (type) 'person)
	(method (look-around)
					(map (lambda (obj) (ask obj 'name))
							 (filter (lambda (thing) (not (eq? thing self)))
											 (append (ask place 'things) (ask place 'people)))))
	(method (take thing)
					(cond ((not (thing? thing)) (error "Not a thing" thing))
								((not (memq thing (ask place 'things)))
								 (error "Thing taken not at this place"
												(list (ask place 'name) thing)))
								((memq thing possessions) (error "You already have it!"))
								(else
								 (announce-take name thing)
								 (set! possessions (cons thing possessions))

								 ;; If somebody already has this object...
								 (for-each
									(lambda (pers)
										(if (and (not (eq? pers self)) ; ignore myself
														 (memq thing (ask pers 'possessions)))
												(begin
													(ask pers 'lose thing)
													(have-fit pers))))
									(ask place 'people))
								 (ask thing 'change-possessor self)
								 'taken)))
	(method (lose thing)
					(set! possessions (delete thing possessions))
					(ask thing 'change-possessor 'no-one)
					'lost)
	(method (talk) (print saying))
	(method (set-talk string) (set! saying string))
	(method (exits) (ask place 'exits))
	(method (notice person) (ask self 'talk))
	(method (go direction)
					(let ((new-place (ask place 'look-in direction)))
						(cond ((null? new-place)
									 (error "Can't go" direction))
									((not (ask new-place 'may-enter? self))
									 (error "The place is locked for you"))
									(else
									 (ask place 'exit self)
									 (announce-move name place new-place)
									 (for-each
										(lambda (p)
											(ask place 'gone p)
											(ask new-place 'appear p))
										possessions)
									 (set! place new-place)
									 (ask new-place 'enter self))))) )

;; Q2e We have provided a definition of the Thing class
;; that does not use the object-oriented programming syntax described in the handout.
;; Translate it into the new notation.

;; (define thing
;; 	(let ()
;; 		(lambda (class-message)
;; 			(cond
;; 			 ((eq? class-message 'instantiate)
;; 				(lambda (name)
;; 					(let ((self '()) (possessor 'no-one))
;; 						(define (dispatch message)
;; 							(cond
;; 							 ((eq? message 'initialize)
;; 								(lambda (value-for-self)
;; 									(set! self value-for-self)))
;; 							 ((eq? message 'send-usual-to-parent)
;; 								(error "Can't use USUAL without a parent." 'thing))
;; 							 ((eq? message 'name) (lambda () name))
;; 							 ((eq? message 'possessor) (lambda () possessor))
;; 							 ((eq? message 'type) (lambda () 'thing))
;; 							 ((eq? message 'change-possessor)
;; 								(lambda (new-possessor)
;; 									(set! possessor new-possessor)))
;; 							 (else (no-method 'thing))))
;; 						dispatch)))
;; 			 (else (error "Bad message to class" class-message))))))

;;  ------------------------------------------------------------------
;; answer to Q2e
(define-class (thing name)
	(instance-vars
	 (possessor 'no-one)
	 (type 'thing))
	(method (change-possessor person) (set! possessor person)))
;;  ------------------------------------------------------------------

;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))

;; q2-f) Sometimes it's inconvenient to debug an object interactively
;; because its methods return objects and we want to see the names of the objects.
;; You can create auxiliary procedures for interactive use
;; (as opposed to use inside object methods) that provide the desired information
;; in printable form. For example:

;; (define (name obj) (ask obj 'name))
;; (define (inventory obj)
;;     (if (person? obj)
;;         (map name (ask obj 'possessions))
;;         (map name (ask obj 'things))))

;; write a procedure whereis that takes a person as its argument
;; and returns the name of the place where that person is.

(define (whereis person)
	(let
			((place (ask person 'place)))
		(ask place 'name)))

;; write a procedure owner that takes a thing as its argument and returns
;; the name of the person who owns it.
;; (Make sure it works for things that aren't owned by anyone.)

(define (owner thing)
	(let
			((possessor (ask thing 'possessor)))
		(if (eq? possessor 'no-one)
				possessor
				(ask possessor 'name))))


;; Q A4 Part 1
;; We've provided people with the ability to say something
;; using the messages 'talk and 'set-talk.

;; the talker is already at the place
;; listener entering the place
;; the listener sends an enter message to the place when entering.
;; the talker has a notice method that when called, he talks.

;; modify the enter method for places, so that in addition
;; to what that method already does, it sends a notice message
;; to each person in that place other than the person who is entering.
;; The notice message should have the newly-entered person as an argument.
;; (You won't do anything with that argument now, but you'll need it later.)

;; Add the following to adv-world.scm:

;; (define singer (instantiate person 'rick sproul-plaza))

;; (ask singer 'set-talk "My funny valentine, sweet comic valentine")

;; (define preacher (instantiate person 'preacher sproul-plaza))

;; (ask preacher 'set-talk "Praise the Lord")

;; (define street-person (instantiate person 'harry telegraph-ave))

;; (ask street-person 'set-talk "Brother, can you spare a buck")

;; QA4 part 2
;; Invent a may-enter? message for places that takes a person as an argument and always returns #t.


;; QA5
;; Invent a special kind of place called a garage.
;; Garages have two methods (besides the ones all places have): park and unpark.
;; The park method takes a vehicle (a thing) as its argument.

;; You'll also need a special kind of thing called a ticket.
;; what's special about it is that it has a number as an instantiation variable.

;; The person who possesses the vehicle will enter the garage,
;; then ask to park it, so the vehicle should have entered the garage
;; along with the person before the park message is sent.


;; First check to be sure that the vehicle is actually in the garage.
;; Then generate a ticket with a unique serial number.
;; The counter for serial numbers should be shared among all garages,
;; so that we don't get in trouble later trying to unpark a vehicle
;; from one garage that was parked in a different garage.)
;; Every ticket should have the name ticket.

;; The file tables.scm contains an implementation of the table Abstract Data Type:
;; constructor: (make-table) returns a new, empty table.
;; mutator: (insert! key value table) adds a new key-value pair to a table.
;; selector: (lookup key table) returns the corresponding value, or #f if
;;                      the key is not in the table.


;; Make a table entry with the ticket number as the key, and the vehicle as the value.
;; Then ask the vehicle's owner to lose the vehicle and take the ticket.

;; The unpark method takes a ticket as argument. First make sure the object
;; you got is actually a ticket (by checking the name). Then look up the ticket number
;; in the garage's table. If you find a vehicle, ask the ticket's owner to
;; lose the ticket and take the vehicle. Also, insert #f in the table for that ticket number,
;; so that people can't unpark the vehicle twice.


;; Notes:
;; A ticket only has one instantiation variable, a serial number. (e.g., (instantiate ticket 120)).
;; A ticket is a thing with the name 'ticket
;; A garage takes one instantiation variable, its name. (e.g., (instantiate garage 'soda-garage)).

;; Do NOT define a new class for vehicles. You can assume that
;; park is called with the correct argument.

;; Parking a vehicle that is not owned by anyone should return an error.
;; Unparking a vehicle that is not parked should return an error.

(define-class (ticket num)
	(parent (thing 'ticket)))

(define-class (garage name)
	(parent (place name))
	(class-vars (counter 0))
	(instance-vars (table (make-table)))
	(method (park vehicle)
					(cond ((not (memq vehicle (ask self 'things)))
								 (error "the vehicle" (ask vehicle 'name) "is not in the garage" name))
								((eq? (owner vehicle) 'no-one)
								 (error "the vehicle" (ask vehicle 'name) "is not owned by anyone"))
								(else
								 (let ((tick (instantiate ticket (+ counter 1))))
									 (begin
										 (set! counter (+ counter 1))
										 (insert! (ask tick 'num) vehicle table)
										 (ask self 'appear tick)
										 (ask (ask vehicle 'possessor) 'take tick)
										 (ask (ask vehicle 'possessor) 'lose vehicle)
										 'parked)))))
	(method (unpark tick)
					(if (not (eq? (ask tick 'name) 'ticket))
							(error "not a ticket")
							(let ((vehicle (lookup (ask tick 'num) table)))
								(if vehicle
										(begin
											(ask (ask tick 'possessor) 'take vehicle)
											(ask (ask tick 'possessor) 'lose tick)
											(insert! (ask tick 'num) #f table)
											'unparked)
										(error "vehicle not parked in the garage"))))))

#|
Exercise 1. Why did the student get an error?
Suppose that the following definitions have been provided.

(define x (cons 1 3))
(define y 2)

A CS 61AS student, intending to change the value of x
to a pair with car equal to 1 and cdr equal to 2,
types the expression (set! (cdr x) y) instead of (set-cdr! x y)
and gets an error. Explain why.

Answer:
set! evaluates the second argument and stores in the location in which
the first argument is bound. the first argument must be a variable (name)
(cdr x) is not a variable
in certain implementation of scheme this works

|#

;; Exercise 2
;; Exercise 2a. Fill in the ?? so that the calls produce the desired effect.
;; Do not create any new pairs; just rearrange the pointers to the existing ones.
(define list1 (list (list 'a) 'b))
(define list2 (list (list 'x) 'y))
(define (answer3)
  ;; (set-cdr! ?? ??)
  ;; (set-cdr! ?? ??))
	(set-cdr! (car list2) (cdr list1))
	(set-cdr! (car list1) (car list2)))
(answer3)
list1 ; Should output ((a x b) b)
list2 ; Should output ((x b) y)

;; Exercise 2b.
;; b) After filling in the blanks in the code above and producing the specified effect
;; on list1 and list2, draw a box-and-pointer diagram that explains the effect
;; of evaluating the expression (set-car! (cdr list1) (cadr list2)).

;;
;;            +-----+-----+       +-----+-----+                +-----+-----+       +-----+-----+
;; list1----> |     |     |       |     |  /- |     list2----> |     |     |       |     |  /- |
;;            |  |  |   --------->|  |  |/-   |                |  |  |   --------->|  |  |/-   |
;;            +--|--+-----+       +--|--------+                +--|--+-----+       +--|--------+
;;               |                   |                            |                   |
;;               |                   |                            |                   |
;;               v                   v                            v                   v
;;            +-----+-----+          b                         +-----+-----+          y
;;            |     |  -/ |                                    |     |  -/ |
;;            |  |  |-/   |                                    |  |  |-/   |
;;            +--|--+-----+                                    +--|--+-----+
;;               |                                                |
;;               |                                                |
;;               v                                                v
;;               a                                                x



;;              +-----+-----+       +-----+-----+                +-----+-----+       +-----+-----+
;;   list1----> |     |     |       |     |  /- |     list2----> |     |     |       |     |  // |
;;              |  |  |   --------->|  |  |/-   |                |  |  |   --------->|  |  | //  |
;;              +--|--+-----+       +--|--------+                +--|--+-----+       +--|--------+
;;                 |                   |                            |                   |
;;                 |                   |                            |                   |
;;                 v<------------------|-------------      /------->v                   v
;;              +-----+-----+     -------------------\----/      +-----+-----+          y
;;              |     |  --------/     v              \-------------------   |
;;              |  |  |     |          b                         |  |  |     |
;;              +--|--+-----+                                    +--|--+-----+
;;                 |                                                |
;;                 |                                                |
;;                 v                                                v
;;                 a                                                x


;;              +-----+-----+       +-----+-----+                +-----+-----+       +-----+-----+
;;   list1----> |     |     |       |     |     |     list2----> |     |     |       |     |  // |
;;              |  |  |   --------->|  |  | -------\             |  |  |   --------->|  |  | //  |
;;              +--|--+-----+       +--|--------+   \            +--|--+-----+       +--|--------+
;;                 |                   |             \--------------|--------\          |
;;                 |                   |                            |         \         |
;;                 v<------------------|-------------      /------->v          -------> v
;;              +-----+-----+     -------------------\----/      +-----+-----+          y
;;              |     |  --------/     v              \----------|-----|--   |
;;              |  |  |     |          b                         |  |  |     |
;;              +--|--+-----+                                    +--|--+-----+
;;                 |                                                |
;;                 |                                                |
;;                 v                                                v
;;                 a                                                x


;; Exercise 3.
;; SICP 3.13
;; Draw the box-pointer diagram of z
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))


;;      |---------------------------------------------
;;      |                                             |
;;      |                                             |
;;      v  +-----+-----+     +-----+-----+    +-----+-|---+
;; x ----> |     |     |     |     |     |    |     | |   |
;;         |  |  |   ------->|  |  |  ------> |  |  |     |
;;         +--|--+-----+     +--|--+-----+    +--|--+-----+
;;            |                 |                |
;;            |                 |                |
;;            v                 v                v
;;            a                 b                c

;; What happens if we try to compute (last-pair z)?
;; infinite loop happens

;; SICP 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; What does mystery do in general?
;; takes a list and reverse it.

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;; Draw the box-pointer diagram of v before the call to mystery,


;;         +-----+-----+     +-----+-----+    +-----+-----+    +-----+-----+
;; v ----> |     |     |     |     |     |    |     |     |    |     |     |
;;         |  |  |   ------->|  |  |  ------> |  |  |  ------> |  |  |     |
;;         +--|--+-----+     +--|--+-----+    +--|--+-----+    +--|--+-----+
;;            |                 |                |                |
;;            |                 |                |                |
;;            v                 v                v                v
;;            a                 b                c                d

;; v after the call to mystery, and w

;;                                         ---------------------------------
;;      |----------------------------|     |                               |
;;      |                            |     |                               |
;;      |                            |     |                               |
;;      v  +-----+-----+    +-----+--|--+  | +-----+-----+        +-----+--|--+
;; v ----> |     |     |    |     |  |  |  | |     |     |  w---> |     |  |  |
;;         |  |  |     |  ->|  |  |     |  ->|  |  |  |  |        |  |  |     |
;;         +--|--+-----+  | +--|--+-----+    +--|--+--|--+        +--|--+-----+
;;            |           |    |                |     |              |
;;            |           |    |                |     |              |
;;            v           |    v                v     |              v
;;            a           |    b                c     |              d
;;                        |                           |
;;                        |                           |
;;                        ----------------------------|

;; What would be printed as the values of v and w?
;; > v  ; (a)
;; > w  ; (d c b a)

;; Exercise 4.
;; SICP 3.16 Draw the 4 box-and-pointer diagrams.
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

 #|
a. Returns 3:
(define x (list 'a 'b 'c))

         +-----+-----+     +-----+-----+    +-----+-----+
  x----> |     |     |     |     |     |    |     |     |
         |  |  |   ------->|  |  |  ------> |  |  |     |
         +--|--+-----+     +--|--+-----+    +--|--+-----+
            |                 |                |
            |                 |                |
            v                 v                v
            a                 b                c

b. Returns 4:
(define x (list 'a))
(define z (list x x))

             +---------+---------+        +---------+---------+
             |         |         |        |         |         |
             |         |         |        |         |         |
 z --------> |    |    |     -----------> |  |      |         |
             |    |    |         |        |  |      |         |
             +----|----+---------+        +--|------+---------+
                  |                          |
                  |                          |
                  |                          |
                  |                          |
                  |                          |
                  |                          |
                  v <------------------------
             +---------+---------+
             |         |         |
             |         |         |
 x---------> |    |    |         |
             |    |    |         |
             +----|----+---------+
                  |
                  |
                  |
                  |
                  |
                  |
                  v
                  a
c. Returns 7:
(define x (list 'a))
(define y (cons x x))
(define z (cons y y))

             +---------+---------+        +---------+---------+
             |         |         | y----> |         |         |
             |  ------------------------> |         |         |
 z --------> |         |     -----------> |  |      |    |    |
             |         |         |        |  |      |    |    |
             +---------+---------+        +--|------+----|----+
                                             |           |
                                             |           |
                                             |           |
                    -------------------------------------|
                    |                        |
                    |                        |
                    v<------------------------
             +---------+---------+
             |         |         |
             |         |         |
 x---------> |    |    |         |
             |    |    |         |
             +----|----+---------+
                  |
                  |
                  |
                  |
                  |
                  |
                  v
                  a

d. Never returns:
(define x (list 'a 'b 'c))
(set-cdr! (last-pair x) x)

      |---------------------------------------------
      |                                             |
      |                                             |
      v  +-----+-----+     +-----+-----+    +-----+-|---+
 x ----> |     |     |     |     |     |    |     | |   |
         |  |  |   ------->|  |  |  ------> |  |  |     |
         +--|--+-----+     +--|--+-----+    +--|--+-----+
            |                 |                |
            |                 |                |
            v                 v                v
            a                 b                c

|#

;; SICP 3.17 Write a correct version of count-pairs.
;; Devise a correct version of the count-pairs procedure of exercise 3.16
;; that returns the number of distinct pairs in any structure.
;; Hint: Traverse the structure, maintaining an auxiliary data structure
;; that is used to keep track of which pairs have already been counted

(define (count-pairs x)
	;; (error "Not implemented yet!"))
	(define counted '())
	(define (helper x)
		(if (or (not (pair? x)) (memq x counted))
      0
      (begin
				(set! counted (cons x counted))
				(+ (helper (car x))
					 (helper (cdr x))
					 1))))
	(helper x))


;; SICP 3.21 Explain what Eva Lu Ator is talking about, and what happened with
;; Ben's examples.  Then define print-queue.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

#| What happened with Ben's examples?
show why Ben's examples produce the printed results that they do

-> (define q1 (make-queue))
-> (insert-queue! q1 'a)
((a) a)
-> (insert-queue! q1 'b)
((a b) b)
-> (delete-queue! q1)
((b) b)
-> (delete-queue! q1)
(() b)

the insert-queue! and delete-queue! return the queue
the queue is a pair whose first element points to the front element of the queue
the second element is a pointer to the rear element of the queue
the elements are implemented as a continous list
so when returned from the procedure, it is a pair whose first element is a list of all elements
of the queue, and the second element is the last element

|#

;; Implement the definition of print-queue
;; Make sure you use display to print the queue.
(define (print-queue queue)
	;; (error "Not implemented yet!"))
	(display (front-ptr queue))
	(newline))


;; SICP 3.25 Write lookup and insert!
;; not verified
(define (lookup keys table)
	;; (error "Not implemented yet!"))
	(if (null? keys)
			(cdr table)
			(let ((m (assoc (car keys) (cdr table))))
				(cond ((not m) #f)
							((not (pair? (cdr m))) (cdr m))
							(else (lookup (cdr keys) (cdr m)))))))

;; from http://community.schemewiki.org
;; not verified
(define (insert! keys value table)
	;; (error "Not implemented yet!"))
       (if (null? keys)
         (set-cdr! table value)
         (let ((table-rest (if (list? table) (cdr table) '())))
           (let ((record (assoc (car keys) table-rest)))
             (if (not record)
               (begin
                 (set! record (list (car keys)))
                 (set-cdr! table (cons record table-rest))))
             (insert! record (cdr keys) value)))))

#|
SICP 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

The memoized version of the same procedure is

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

where the memoizer is defined as

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))


Explain why the number of steps is proportional to n (you may want to
include a trace to explain).
You do not need to draw the environment diagram.

Answer: table is implemented such that we put the recent computed values
at the beginnning of the table
so the order is O(n)

Would it still work (efficiently) if we define memo-fib as (memoize
fib)?  Why or why not?
will not work because when evaluating code for (memoize fib);
 (fib x) will be evaluated whose enclosing envrionment is the
global environment and thus memoization is lost.


|#

;; Exercise 5. Write vector-append.
;; Write vector-append, which takes two vectors as arguments
;; and returns a new vector containing the elements of both arguments,
;; analogous to append for lists.
;; Don't use a list as an intermediate value.
;; (That is, don't convert the vectors to lists at any time!)

(define (vector-append v1 v2)
	;; (error "Not implemented yet!"))
	(let*	((lenv1 (vector-length v1))
				 (lenv2 (vector-length v2))
				 (lenv (+ lenv1 lenv2))
				 (v (make-vector lenv)))
		(define (loop i)
			(cond ((< i lenv1)
						 (begin (vector-set! v i (vector-ref v1 i))
										(loop (+ i 1))))
						((< i lenv)
						 (begin
							 (vector-set! v i (vector-ref v2 (- i lenv1)))
							 (loop (+ i 1))))
						(else v)))
		(loop 0)))


;;Exercise 6. Write vector-filter.
;; The new vector should be exactly big enough for the chosen elements.
;; Compare the running time of your program to this version:
;; (define (vector-filter pred vec)
;;     (list->vector (filter pred (vector->list vec))))

;; Don't use a list as an intermediate value.
;; (That is, don't convert the vectors to lists at any time!)

(define (vector-filter pred vec)
	;; (error "Not implemented yet!"))
	(define (loop v i)
		(if (= i (vector-length vec))
				v
				(if (pred (vector-ref vec i))
						(loop (vector-append v (make-vector 1 (vector-ref vec i))) (+ i 1))
						(loop v (+ i 1)))))
	(loop #() 0))


;; Exercise 7. Write bubble-sort!
;; a) Write bubble-sort!, which takes a vector of numbers and rearranges them
;; to be in increasing order.
;; You'll modify the argument vector; do not create a new one.
;; Use the following algorithm for your definition:
;; 1. Go through the array, looking at two adjacent elements at a time,
;; starting with elements 0 and If the earlier element is larger than the later element,
;; swap them. Then look at the next overlapping pair (0 and 1, then 1 and 2, etc.).
;; 2. Recursively bubble-sort all but the last element
;; (which is now the largest element).
;; 3. Stop when you have only one element to sort.

;; Don't use a list as an intermediate value.
;; (That is, don't convert the vectors to lists at any time!)

(define (bubble-sort! vec)
	;; (error "Not implemented yet!"))
	(define (loop i len)
		(if (= len 1)
				vec
				(if (< i (- len 1))
						(if (<= (vector-ref vec i) (vector-ref vec (+ i 1)))
								(loop (+ i 1) len)
								(let ((temp (vector-ref vec i)))
									(vector-set! vec i (vector-ref vec (+ i 1)))
									(vector-set! vec (+ i 1) temp)
									(loop (+ i 1) len)))
						(loop 0 (- len 1)))))
	(loop 0 (vector-length vec)))



;; b) Prove that this algorithm really does sort the vector.
;; Hint: Prove the parenthetical claim in step 2.

;; c) What is the order of growth of the running time of this algorithm?

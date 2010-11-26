;; paip p.322
;; tconc

(define (queue-contents q)
  (cdr q))

(define (make-queue)
  (rlet1 q (cons '() '())
         (set! (car q) q)))

(define (enqueue item q)
  (set! (car q)
        (rlet1 f (cons item '())
               (set! (cdr (car q)) f)))
  q)

(define (dequeue q)
  (pop! (cdr q))
  (when (null? (cdr q))
    (set! (car q) q))
  q)

(define (front q)
  (car (queue-contents q)))

(define (empty-queue? q)
  (null? (queue-contents q)))

(define (queue-append! q ls)
  (set! (cdr (car q)) ls)
  (set! (car q)
        (last-pair q))
  q)



(define q (make-queue))

(queue-contents q)
;; ()

(empty-queue? q)
;; #t

(enqueue 'a q)
;; (#0=(a) . #0#)
(enqueue 'b q)
;; (#0=(b) a . #0#)
(enqueue 'c q)
;; (#0=(c) a b . #0#)

(empty-queue? q)
;; #f

(front q)
;; a

(queue-contents q)
;; (a b c)

(queue-append! q '(d e f))
;; (#0=(f) a b c d e . #0#)

(dequeue q)
;; (#0=(f) b c d e . #0#)
(dequeue q)
;; (#0=(f) c d e . #0#)
(dequeue q)
;; (#0=(f) d e . #0#)


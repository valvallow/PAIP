(use gauche.parameter) ; parameter
(use srfi-1) ; last

(define names '((John Q Public)(Malcolm X)(Admiral Grace Murray Hopper)
                (Spot)(Aristotle)(A A Milne)(Z Z Top)(Sir Larry Olivier)
                (Miss Scarlet)))

names
;; ((John Q Public) (Malcolm X) (Admiral Grace Murray Hopper) (Spot) (Aristotle) (A A Milne) (Z Z Top) (Sir Larry Olivier) (Miss Scarlet))

(define *titles* (make-parameter
                  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)))

(*titles*)
;; (Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)


(define (last-name name)
  (last name))

(last-name (car names))
;; Public

(define (first-name name)
  (first name))

(first-name (car names))
;; John

(first-name (caddr names))
;; Admiral


(map last-name names)
;; (Public X Hopper Spot Aristotle Milne Top Olivier Scarlet)

(map first-name names)
;; (John Malcolm Admiral Spot Aristotle A Z Sir Miss)


(define (first-name name)
  (if (member (car name)(*titles*))
      (first-name (cdr name))
      (car name)))

(map first-name names)
;; (John Malcolm Grace Spot Aristotle A Z Larry Scarlet)

(use slib)
(require 'trace)
(trace first-name)

(map first-name names)
;; CALL first-name (John Q Public)
;; RETN first-name John
;; CALL first-name (Malcolm X)
;; RETN first-name Malcolm
;; CALL first-name (Admiral Grace Murray Hopper)
;;  CALL first-name (Grace Murray Hopper)
;;  RETN first-name Grace
;; RETN first-name Grace
;; CALL first-name (Spot)
;; RETN first-name Spot
;; CALL first-name (Aristotle)
;; RETN first-name Aristotle
;; CALL first-name (A A Milne)
;; RETN first-name A
;; CALL first-name (Z Z Top)
;; RETN first-name Z
;; CALL first-name (Sir Larry Olivier)
;;  CALL first-name (Larry Olivier)
;;  RETN first-name Larry
;; RETN first-name Larry
;; CALL first-name (Miss Scarlet)
;;  CALL first-name (Scarlet)
;;  RETN first-name Scarlet
;; RETN first-name Scarlet
;; (John Malcolm Grace Spot Aristotle A Z Larry Scarlet)
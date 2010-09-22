;; PAIP gps


(use liv.macros)
(use liv.debugs)

(use gauche.parameter)
(use srfi-1)

;; (debug :gps)

;; The current state: a list of conditions.
;; (define *state* (make-parameter '()))
;; (define state '())
;; (define state (make-parameter '()))

;; A list of available operators.
;; (define *ops* (make-parameter '()))
(define ops (make-parameter '()))
;; (define ops (make-parameter '()))

;; ;; fail
;; (define fail '())

;; ;; find-all (filter?)
;; (define (find-all item sequence . keyword-args)
;;   (let-keywords keyword-args ((test eq?)(test-not #f) . rest)
;;                 (let1 test (complement (if test-not test-not test))
;;                   (apply remove (pa$ test item) sequence rest))))

(define-struct op action preconds add-list del-list)

;; general problem solver
(define (gps state goals ops)

  (define (achieve goal)
    (or (member goal state)
        (any apply-op (filter (pa$ appropriate? goal) ops))))

  (define (appropriate? goal op)
    (member goal (op-add-list op)))

  (define (apply-op op)
    (if (every achieve (op-preconds op))
        (begin
          (print (list 'executing (op-action op)))
          (set! state (lset-difference eq? state (op-del-list op)))
          (set! state (lset-union eq? state (op-add-list op))))
        #f))

  ;; gps body
  (if (every achieve goals)
      'solved
      #f))

(define school-ops
   (list
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works)
             :del-list '())
    (make-op :action 'tell-shop-problem
             :preconds '(in-communication-with-shop)
             :add-list '(shop-knows-problem)
             :del-list '())
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-communication-with-shop)
             :del-list '())
    (make-op :action 'look-up-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number)
             :del-list '())
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money))))

(ops school-ops)

(gps '(son-at-home car-needs-battery have-money have-phone-book)
       '(son-at-school)
       (ops))
;; (executing look-up-number)
;; (executing telephone-shop)
;; (executing tell-shop-problem)
;; (executing give-shop-money)
;; (executing shop-installs-battery)
;; (executing drive-son-to-school)
;; solved

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     (ops))
;; (executing look-up-number)
;; (executing telephone-shop)
;; (executing tell-shop-problem)
;; (executing give-shop-money)
;; (executing shop-installs-battery)
;; (executing drive-son-to-school)
;; solved

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     (ops))
;; #f

(gps '(son-at-home car-works)
     '(son-at-school)
     (ops))
;; (executing drive-son-to-school)
;; solved



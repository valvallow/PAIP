
(define (power n m)
  (let rec ((m m)(acc 1))
    (if (zero? m)
        acc
        (rec (- m 1)(* n acc)))))

(power 3 2)
;; 9
(power 10 0)
;; 1
(power 3 -1)

(use srfi-1)
(define (power n m)
  (apply * (list-tabulate m (lambda (x)
                              n))))

(power 3 2)
;; 9
(power 3 0)
;; 1

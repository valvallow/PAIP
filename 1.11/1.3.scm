
(use srfi-1)
(define (flatten tree)
  (fold-right (lambda (e acc)
                (if (pair? e)
                    (append (flatten e) acc)
                    (cons e acc)))
              '() tree))

(define (count-atoms tree)
  (length (flatten tree)))

(count-atoms '(a (b) c))
;; 3
(count-atoms '(a () c))
;; 3
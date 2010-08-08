
(define (count-anywhere a tree)
  (fold (lambda (e acc)
          (+ acc
             (cond ((pair? e)(count-anywhere a e))
                   ((eq? a e) 1)
                   (else 0))))
        0 tree))

(count-anywhere 'a '(a ((a) b) a))
;; 3


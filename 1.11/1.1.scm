;; (last-name '((Rex Morgan MD)(Morton Downey Jr))) -> (Morgan Downey)

(use gauche.parameter)

(define *prefs* (make-parameter '(MD Jr)))

(define (last-name name)
  (let1 name (reverse name)
    (let rec ((name name))
      (if (member (car name)(*prefs*))
          (rec (cdr name))
          (car name)))))

(last-name (car '((Rex Morgan MD)(Morton Downey Jr))))
;; Morgan
(map last-name '((Rex Morgan MD)(Morton Downey Jr)))
;; (Morgan Downey)
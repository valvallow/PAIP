(define (terminal? phrase)
  (and (not (list? phrase))
       (not (rewrites phrase))))

(define (generate phrase)
  (if (list? phrase)
      (mapend generate phrase)
      (if (terminal? phrase)
          (list phrase)
          (generate (random-elt (rewrites phrase))))))

(generate 'sentene)
;; (the table liked a woman)


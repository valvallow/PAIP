(use srfi-27)
(use gauche.parameter)

(define *simple-grammar*
  (make-parameter '((sentene -> (noun-phrase verb-phrase))
                    (noun-phrase -> (article noun))
                    (verb-phrase -> (verb noun-phrase))
                    (article -> the a)
                    (noun -> man ball woman table)
                    (verb -> hit took saw liked))))

(define *grammar* *simple-grammar*)

(define (rule-lhs rule)
  (car rule))

(define (rule-rhs rule)
  (cdr (cdr rule)))

(define (rewrites category)
  (let1 ret (assoc category (*grammar*))
    (if (list? ret)
        (rule-rhs ret)
        ret)))

(define (mappend proc ls)
  (apply append (map proc ls)))

(define (one-of set)
  (list (random-elt set)))

(define (random-elt choices)
  (list-ref choices (random-integer (length choices))))

(define (generate phrase)
  (cond ((list? phrase)(mappend generate phrase))
        ((rewrites phrase)(generate (random-elt (rewrites phrase))))
        (else (list phrase))))

(generate 'sentene)
;; (the man saw a table)
(generate 'sentene)
;; (a table liked the woman)


;; --------------------------------------------------------------------

(define (generate phrase)
  (cond ((list? phrase)
         (mappend generate phrase))
        ((rewrites phrase)
         => (compose generate (pa$ random-elt)))
        (else (list phrase))))

(generate 'sentene)
;; (a table liked the ball)
(generate 'sentene)
;; (a table saw a table)


;; --------------------------------------------------------------------

(define (generate phrase)
  (if (list? phrase)
      (mapend generate phrase)
      (let1 choices (rewrites phrase)
        (if choices
            (generate (random-elt choices))
            (list phrase)))))

(generate 'sentene)
;; (the table hit a woman)


;; --------------------------------------------------------------------

(define (generate phrase)
  (if (list? phrase)
      (mapend generate phrase)
      (if-let1 choices (rewrites phrase)
               (generate (random-elt choices))
               (list phrase))))

(generate 'sentene)
;; (a ball liked a woman)



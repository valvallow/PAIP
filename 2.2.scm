(use srfi-27)

(define (sentence)
  (append (noun-phrase)(verb-phrase)))

(define (noun-phrase)
  (append (Article)(Noun)))

(define (verb-phrase)
  (append (Verb)(noun-phrase)))

(define (Article)
  (one-of '(the a)))

(define (Noun)
  (one-of '(man ball woman table)))

(define (Verb)
  (one-of '(hit took saw liked)))

(define (one-of set)
  (list (random-elt set)))

(define (random-elt choices)
  (list-ref choices (random-integer (length choices))))

(sentence)
;; (a man liked a man)
(sentence)
;; (a table hit a ball)
(sentence)
;; (the woman took the woman)
(sentence)
;; (a table liked a table)
(sentence)
;; (the woman liked a table)



;; ------------------------------------------------------------------

(define-syntax def-phrase
  (syntax-rules ()
    ((_ (name proc exp1 exp2 ...))
     (define (name)
       (proc exp1 exp2 ...)))
    ((_ (name11 proc11 exp11 exp12 ...)
        (name21 proc21 exp21 exp22 ...) ...)
     (begin
       (def-phrase (name11 proc11 exp11 exp12 ...))
       (def-phrase (name21 proc21 exp21 exp22 ...) ...)))))

(define-macro (def-choice name proc ls)
  (let ((varname (gensym)))
    (let ((varname (string->symbol #`"*,|name|s*")))
      `(begin
         (define ,varname ,ls)
         (define (,name)
           (,proc ,varname))))))

(define-syntax def-choices
  (syntax-rules ()
    ((_ (name proc ls))
     (def-choice name proc ls))
    ((_ (name1 proc1 ls1)(name2 proc2 ls2) ...)
     (begin
       (def-choices (name1 proc1 ls1))
       (def-choices (name2 proc2 ls2) ...)))))



(define (one-of set)
  (list (random-elt set)))

(define (random-elt choices)
  (list-ref choices (random-integer (length choices))))

(def-phrase
  (sentence append (noun-phrase)(verb-phrase))
  (noun-phrase append (article)(noun))
  (verb-phrase append (verb)(noun-phrase)))

(def-choices
  (article one-of '(the a))
  (noun one-of  '(man ball woman table))
  (verb one-of '(hit took saw liked)))

(sentence)
;; (the man saw a table)
(sentence)
;; (a table liked the woman)
(sentence)
;; (the ball took a man)
(sentence)
;; (a table took the man)
(sentence)
;; (the man liked a table)



(define (adj*)
  (if (= (random-integer 2) 0)
      '()
      (append (adj)(adj*))))

(define (pp*)
  (if (random-elt '(#t #f))
      (append (pp)(pp*))
      '()))

(def-phrase
  (noun-phrase append (article)(adj*)(noun)(pp*))
  (pp append (prep)(noun-phrase)))

(def-choices
  (adj one-of '(big little blue green adiabatic))
  (prep one-of '(to in by with on)))

(sentence)
;; (a little blue little table took a adiabatic blue blue blue big table in the adiabatic little table in the little table on the green blue little table)
(sentence)
;; (a green woman took a little woman)
(sentence)
;; (the man by a little adiabatic man by the woman in the green ball hit the blue ball)


;; ------------------------------------------------------------------

(define-macro (def-choice name proc ls)
  (let ((varname (gensym))
        (valname (gensym)))
    (let1 varname (string->symbol #`",|name|s")
      `(begin
         (define-values (,name ,(string->symbol #`"push-,|varname|!"))
           (let1 ,varname ,ls
             (values (lambda ()
                       (,proc ,varname))
                     (lambda (,valname)
                       (push! ,varname ,valname)))))))))

(def-choice adj one-of '(big))
(def-choice prep one-of '(to))

(sentence)
;; (the big man to the ball to a big big big big big woman to a big woman to the table to a big big man to a big big big table to a big big man to the table to the big man to a ball to a table to a man saw a big big big big woman)
(sentence)
;; (the big big ball to a man took a big man)


(push-adjs! 'little)
;; (little big)
(push-adjs! 'blue)
;; (blue little big)
(push-adjs! 'red)
;; (red blue little big)

(sentence)
;; (the blue man to the big table liked the little little table to the big little big big blue little table)
(sentence)
;; (a red little woman hit the red table to a table to the ball to the blue ball)


(push-preps! 'by)
;; (by to)
(push-preps! 'in)
;; (in by to)
(push-preps! 'with)
;; (with in by to)
(push-preps! 'on)
;; (on with in by to)


(sentence)
;; (the table saw a little ball)
(sentence)
;; (the big table to the little man saw a little ball on the big table in the ball)
(sentence)
;; (a little table hit the woman with a table with the red little woman by the red big ball to a ball by the red big big ball with the red table on the big blue red red ball)
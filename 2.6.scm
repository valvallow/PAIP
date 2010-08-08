(define (generate-tree phrase)
  (if (list? phrase)
      (map generate-tree phrase)
      (if-let1 choices (rewrites phrase)
               (cons phrase
                     (generate-tree (random-elt choices)))
               (list phrase))))

(set! *grammar* *simple-grammar*)

(generate-tree 'sentene)
#|
(sentene (noun-phrase (article the)
                      (noun ball))
         (verb-phrase (verb hit)
                      (noun-phrase (article a)
                                   (noun ball))))
|#


(define (generate-all phrase)
  (cond ((null? phrase)(list '()))
        ((list? phrase)
         (combine-all (generate-all (car phrase))
                      (generate-all (cdr phrase))))
        ((rewrites phrase)
         => (pa$ mappend generate-all))
        (else (list (list phrase)))))

(define (combine-all xlis ylis)
  (mappend (lambda (y)
             (map (lambda (x)
                    (append x y)) xlis))
           ylis))

(print (combine-all (map list '(a b c))(map list '(1 2 3))))
;; ((a 1) (b 1) (c 1) (a 2) (b 2) (c 2) (a 3) (b 3) (c 3))

(generate-all 'article)
;; ((the) (a))
(generate-all 'noun)
;; ((man) (ball) (woman) (table))
(print (generate-all 'noun-phrase))
;; ((the man) (a man) (the ball) (a ball) (the woman) (a woman) (the table) (a table))



;; --------------------------------------------------------------------

;; PAIP 2.6 P.41
(define (combine-all xlis ylis)
  (mappend (lambda (y)
             (map (lambda (x)
                    (append x y)) xlis))
           ylis))

(print (combine-all (map list '(a b c))(map list '(1 2 3))))
;; ((a 1) (b 1) (c 1) (a 2) (b 2) (c 2) (a 3) (b 3) (c 3))



;; (cross-combine '(1 2 3)'(a b c))
;; ((1 a) (2 a) (3 a) (1 b) (2 b) (3 b) (1 c) (2 c) (3 c))
(define (cross-combine xlis ylis)
  (mappend (lambda (y)
             (map (lambda (x)
                    (list x y)) xlis))
           ylis))


;; (add-combine-elements (cross-combine '(1 2 3)'(a b c))
;;                       '(100 200 300))
;; ((1 a 100) (2 a 100) (3 a 100) (1 b 100) (2 b 100) (3 b 100) (1 c 100) (2 c 100) (3 c 100) (1 a 200) (2 a 200) (3 a 200) (1 b 200) (2 b 200) (3 b 200) (1 c 200) (2 c 200) (3 c 200) (1 a 300) (2 a 300) (3 a 300) (1 b 300) (2 b 300) (3 b 300) (1 c 300) (2 c 300) (3 c 300))
(define (add-combine-elements comb elements)
  (mappend (lambda (c)
             (map (lambda (e)
                    (append e (list c))) comb))
           elements))


(define (cross-combine xls yls . opt)
  (let-optionals* opt ((kons list))
    (mappend (lambda (y)
             (map (lambda (x)
                    (kons x y)) xls))
           yls)))


(define (add-combine-elements comb elements)
  (cross-combine comb elements (lambda (e c)
                                 (append e (list c)))))


(define (cross-combines ls1 ls2 . lss)
  (let rec ((lss lss)(comb (cross-combine ls1 ls2)))
    (if (null? lss)
        comb
        (rec (cdr lss)
             (add-combine-elements comb (car lss))))))


;; (print (combine-all (map list '(a b c))(map list '(1 2 3))))
;; ((a 1) (b 1) (c 1) (a 2) (b 2) (c 2) (a 3) (b 3) (c 3))
(define (combine-all xlis ylis)
  (cross-combine xlis ylis append))


(print (cross-combines '(a b c)'(1 2 3)'(100 200 300)))
;; ((a 1 100) (b 1 100) (c 1 100)
;;  (a 2 100) (b 2 100) (c 2 100)
;;  (a 3 100) (b 3 100) (c 3 100)
;;  (a 1 200) (b 1 200) (c 1 200)
;;  (a 2 200) (b 2 200) (c 2 200)
;;  (a 3 200) (b 3 200) (c 3 200)
;;  (a 1 300) (b 1 300) (c 1 300)
;;  (a 2 300) (b 2 300) (c 2 300)
;;  (a 3 300) (b 3 300) (c 3 300))

(print (cross-combines '(a b c)'(#f #t)'(1 2 3)'(100 200 300)))
;; ((a #f 1 100) (b #f 1 100) (c #f 1 100)
;;  (a #t 1 100) (b #t 1 100) (c #t 1 100)
;;  (a #f 2 100) (b #f 2 100) (c #f 2 100)
;;  (a #t 2 100) (b #t 2 100) (c #t 2 100)
;;  (a #f 3 100) (b #f 3 100) (c #f 3 100)
;;  (a #t 3 100) (b #t 3 100) (c #t 3 100)
;;  (a #f 1 200) (b #f 1 200) (c #f 1 200)
;;  (a #t 1 200) (b #t 1 200) (c #t 1 200)
;;  (a #f 2 200) (b #f 2 200) (c #f 2 200)
;;  (a #t 2 200) (b #t 2 200) (c #t 2 200)
;;  (a #f 3 200) (b #f 3 200) (c #f 3 200)
;;  (a #t 3 200) (b #t 3 200) (c #t 3 200)
;;  (a #f 1 300) (b #f 1 300) (c #f 1 300)
;;  (a #t 1 300) (b #t 1 300) (c #t 1 300)
;;  (a #f 2 300) (b #f 2 300) (c #f 2 300)
;;  (a #t 2 300) (b #t 2 300) (c #t 2 300)
;;  (a #f 3 300) (b #f 3 300) (c #f 3 300)
;;  (a #t 3 300) (b #t 3 300) (c #t 3 300))


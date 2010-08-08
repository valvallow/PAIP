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


(define (cross-product proc xls yls)
  (cross-combine xls yls proc))

(define (combine-all xls yls)
  (cross-product append xls yls))

(print (combine-all (map list '(a b c))(map list '(1 2 3))))
;; ((a 1) (b 1) (c 1) (a 2) (b 2) (c 2) (a 3) (b 3) (c 3))

(cross-product + '(1 2 3)'(10 20 30))
;; (11 12 13 21 22 23 31 32 33)

(cross-product list '(a b c d e f g h)'(1 2 3 4 5 6 7 8))
;; ((a 1) (b 1) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)
;;  (a 2) (b 2) (c 2) (d 2) (e 2) (f 2) (g 2) (h 2)
;;  (a 3) (b 3) (c 3) (d 3) (e 3) (f 3) (g 3) (h 3)
;;  (a 4) (b 4) (c 4) (d 4) (e 4) (f 4) (g 4) (h 4)
;;  (a 5) (b 5) (c 5) (d 5) (e 5) (f 5) (g 5) (h 5)
;;  (a 6) (b 6) (c 6) (d 6) (e 6) (f 6) (g 6) (h 6)
;;  (a 7) (b 7) (c 7) (d 7) (e 7) (f 7) (g 7) (h 7)
;;  (a 8) (b 8) (c 8) (d 8) (e 8) (f 8) (g 8) (h 8))
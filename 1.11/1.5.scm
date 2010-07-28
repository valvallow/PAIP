
;; (dot-product '(10 20)'(3 4)) -> (+ (* 10 3)(* 20 4))

(define (dot-product ls1 ls2)
  (apply + (map (pa$ *) ls1 ls2)))

(dot-product '(10 20)'(3 4))
;; 110

(define (dot-product . ls)
  (apply + (apply map (pa$ *) ls)))

(dot-product '(10 20)'(3 4)'(2 3))
;; 300

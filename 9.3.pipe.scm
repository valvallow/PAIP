;; PAIP 9.3 P.263-
;; pipe
;; delay & force

(define-macro (make-pipe head tail)
  `(cons ,head (delay ,tail)))

(define-constant empty-pipe '())

(define (head pipe)
  (car pipe))

(define (tail pipe)
  (force (cdr pipe)))

(define (pipe-ref pipe n)
  (if (zero? n)
      (head pipe)
      (pipe-ref (tail pipe)(- n 1))))

(define (integers . startend)
  (let-optionals* startend ((start 0)(end +inf.0))
    (if (<= start end)
        (make-pipe start (integers (+ start 1) end))
        empty-pipe)))


;; PAIP 9.3 P.266-
;; pipe
;; closure

(define-macro (make-pipe head tail)
  `(cons ,head (lambda () ,tail)))

(define-constant empty-pipe '())

(define (head pipe)
  (car pipe))

(define (pipe-empty? pipe)
  (equal? empty-pipe pipe))

(define (pipe-ref pipe n)
  (cond ((pipe-empty? pipe) #f)
        ((zero? n)(head pipe))
        (else (pipe-ref (tail pipe)(- n 1)))))

(define (tail pipe)
  (cond ((pipe-empty? pipe) empty-pipe)
        ((procedure? (cdr pipe))
         (set! (cdr pipe)((cdr pipe)))
         (cdr pipe))
        (else (cdr pipe))))

(define (integers . startend)
  (let-optionals* startend ((start 0)(end +inf.0))
    (if (<= start end)
        (make-pipe start (integers (+ start 1) end))
        empty-pipe)))


;; example

(define (pipe-enumerate pipe . keys)
  (let-keywords* keys ((count 0)(key #f)(result pipe))
    (if (or (pipe-empty? pipe)(zero? count))
        result
        (begin
          (when key
            (key (head pipe)))
          (pipe-enumerate (tail pipe)
                          :count (- count 1)
                          :key key
                          :result result)))))

(define (pipe-filter pred pipe)
  (if (pred (head pipe))
      (make-pipe (head pipe)
                 (pipe-filter pred (tail pipe)))
      (pipe-filter pred (tail pipe))))

(define (sieve pipe)
  (make-pipe (head pipe)
             (pipe-filter (lambda (x)
                            ((complement zero?)(modulo x (head pipe))))
                          (sieve (tail pipe)))))

(pipe-enumerate (sieve (integers 2)) :count 10)
;; (2 3 5 7 11 13 17 19 23 29 31 . #<closure (pipe-filter pipe-filter)>)

(pipe-enumerate (sieve (integers 2)) :count 100)
;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 . #<closure (pipe-filter pipe-filter)>)


(define (facts pipe)
  (let rec ((pipe pipe)(acc 1))
    (let* ((h (head pipe))
           (cur (* (if (zero? h)
                     1
                     h) acc)))
      (make-pipe cur
                 (rec (tail pipe) cur)))))

(pipe-enumerate (facts (integers 0)) :count 5)
;; (1 1 2 6 24 120 . #<closure (facts rec rec)>)











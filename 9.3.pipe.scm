;; pipe

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


(define-macro (make-pipe head tail)
  `(cons ,head (lambda () ,tail)))

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





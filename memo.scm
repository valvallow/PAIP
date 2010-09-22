;; memo

(define (memo fn)
  (let1 cache (make-hash-table)
    (lambda (x)
      (if-let1 val (hash-table-get cache x #f)
               val
               (rlet1 r (fn x)
                      (hash-table-put! cache x r))))))

(define (fib n)
  (if (or (zero? n)
          (= n 1))
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(use slib)
(require 'trace)
(trace fib)

(fib 3)
;; CALL fib 3
;;  CALL fib 2
;;   CALL fib 1
;;   RETN fib 1
;;   CALL fib 0
;;   RETN fib 1
;;  RETN fib 2
;;  CALL fib 1
;;  RETN fib 1
;; RETN fib 3
;; 3

(define memo-fib (memo fib))

(trace memo-fib)

(memo-fib 3)
;;  CALL fib 3
;;   CALL fib 2
;;    CALL fib 1
;;    RETN fib 1
;;    CALL fib 0
;;    RETN fib 1
;;   RETN fib 2
;;   CALL fib 1
;;   RETN fib 1
;;  RETN fib 3
;; 3

(memo-fib 3)
;; 3


(define *memo-hash-tables* (make-hash-table))

(define (memo fn fn-name . keys)
  (let-keywords* keys ((key car)(test 'eq?))
    (let1 cache (make-hash-table test)
      (hash-table-put!  *memo-hash-tables* fn-name cache)
      (lambda args
        (let1 key (key args)
          (if-let1 val (and (hash-table-exists? cache key)
                            (hash-table-get cache key))
                   val
                   (rlet1 r (apply fn args)
                          (hash-table-put! cache key r))))))))

(define-syntax memoize
  (syntax-rules ()
    ((_ fn . args)
     (set! fn (apply memo fn 'fn args)))))

(define (clear-memo fn-name)
  (let1 cache (hash-table-get *memo-hash-tables* fn-name #f)
    (when cache (hash-table-clear! cache))))

(define-syntax clear-memoize
  (syntax-rules ()
    ((_ fn)
     (clear-memo 'fn))))

(memoize fib)
(trace fib)
(fib 10)
;; CALL fib 10
;;  CALL fib 9
;;   CALL fib 8
;;    CALL fib 7
;;     CALL fib 6
;;     RETN fib 13
;;     CALL fib 5
;;     RETN fib 8
;;    RETN fib 21
;;    CALL fib 6
;;    RETN fib 13
;;   RETN fib 34
;;   CALL fib 7
;;   RETN fib 21
;;  RETN fib 55
;;  CALL fib 8
;;  RETN fib 34
;; RETN fib 89
;; 89
(fib 10)
;; CALL fib 10
;; RETN fib 89
;; 89


(clear-memo 'fib)
(fib 10)
;; CALL fib 10
;;  CALL fib 9
;;   CALL fib 8
;;    CALL fib 7
;;     CALL fib 6
;;     RETN fib 13
;;     CALL fib 5
;;     RETN fib 8
;;    RETN fib 21
;;    CALL fib 6
;;    RETN fib 13
;;   RETN fib 34
;;   CALL fib 7
;;   RETN fib 21
;;  RETN fib 55
;;  CALL fib 8
;;  RETN fib 34
;; RETN fib 89
;; 89

(clear-memoize fib)
(fib 10)
;; CALL fib 10
;;  CALL fib 9
;;   CALL fib 8
;;    CALL fib 7
;;     CALL fib 6
;;     RETN fib 13
;;     CALL fib 5
;;     RETN fib 8
;;    RETN fib 21
;;    CALL fib 6
;;    RETN fib 13
;;   RETN fib 34
;;   CALL fib 7
;;   RETN fib 21
;;  RETN fib 55
;;  CALL fib 8
;;  RETN fib 34
;; RETN fib 89
;; 89
(fib 10)
;; CALL fib 10
;; RETN fib 89
;; 89

(define-syntax define-memo
  (syntax-rules ()
    ((_ (fn arg ...) body ...)
     (begin
       (define (fn arg ...)
         body ...)
       (memoize fn)))))

(macroexpand '(define-memo (fib n)
                (if (or (zero? n)
                        (= n 1))
                    1
                    (+ (fib (- n 1))
                       (fib (- n 2))))))

;; (#<identifier user#begin>
;;               (#<identifier user#define> (fib n)
;;                             (if (or (zero? n) (= n 1)) 1
;;                                 (+ (fib (- n 1)) (fib (- n 2)))))
;;               (#<identifier user#memoize> fib))

(define-memo (fib n)
                (if (or (zero? n)
                        (= n 1))
                    1
                    (+ (fib (- n 1))
                       (fib (- n 2)))))

(clear-memoize fib)

(trace fib)
(fib 10)
;; CALL fib 10
;;  CALL fib 9
;;   CALL fib 8
;;    CALL fib 7
;;     CALL fib 6
;;     RETN fib 13
;;     CALL fib 5
;;     RETN fib 8
;;    RETN fib 21
;;    CALL fib 6
;;    RETN fib 13
;;   RETN fib 34
;;   CALL fib 7
;;   RETN fib 21
;;  RETN fib 55
;;  CALL fib 8
;;  RETN fib 34
;; RETN fib 89
;; 89
(fib 10)
;; CALL fib 10
;; RETN fib 89
;; 89


(defun count-all-atoms (exp &optional (if-null 1))
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))

(count-all-atoms '(a (b) c))
;; 3
(count-all-atoms '(a nil c))
;; 3


(defun count-all-atoms (exp &optional (if-null 1))
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) if-null)
              (count-all-atoms (rest exp) if-null)))))

(count-all-atoms '(a (b) c))
;; 5
(count-all-atoms '(a (b) c) 0)
;; 3
(count-all-atoms '(a nil c))
;; 4
(count-all-atoms '(a nil c) 0)
;; 2





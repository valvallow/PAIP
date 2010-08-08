(define *bigger-grammar*
  (make-parameter '((sentene -> (noun-phrase verb-phrase))
                    (noun-phrase -> (article adj* noun pp*)(name)(pronoun))
                    (verb-phrase -> (verb noun-phrase pp*))
                    (pp* -> ()(pp pp*))
                    (adj* -> ()(adj adj*))
                    (pp -> (prep noun-phrase))
                    (prep -> to in by with on)
                    (adj -> big little blue green adiabatic)
                    (article -> the a)
                    (name -> Pat Kim Lee Terry Robin)
                    (noun -> man ball woman table)
                    (verb -> hit took saw liked)
                    (pronoun -> he she it these those that))))

(set! *grammar* *bigger-grammar*)

(generate 'sentene)
;; (the adiabatic man by the table to the ball saw those with she with a big big ball on Kim by she on a adiabatic blue blue blue big table)

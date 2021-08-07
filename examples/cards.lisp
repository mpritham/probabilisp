; probablity of selecting the same card twice from a deck of 52 cards
; where a card is put back after being selected

; length of list
(define (length xs)
    (cond 
        ((null? xs)
            0)
        (else
            (+ 1 (length (cdr xs))))))

; create a list from 1 to n
(define (listN n)
    (cond
        ((= 0 n)
            '())
        (else
            (cons n (listN (- n 1))))))

; helper predicate check if two adjacent elements are the same
(define (predH xs)
    (cond
        ((<= (length xs) 1)
            #f)
        ((= (car xs) (car (cdr xs)))
            #t)
        (else
            (predH (cdr xs))))))

; predicate check if two adjacent elements are the same after sorting
(define (pred xs)
    (predH (sort xs)))

; probablity of selecting the same card twice from a 52-card deck
; (card is returned to deck)
(?? pred (sample 2 (listN 52)))
;    probabilisp> (?? pred (sample 2 (listN 52)))
;    OUTPUT: 1.9230774e-2


(?? (predH (sort (sample 2 (listN 52)))))
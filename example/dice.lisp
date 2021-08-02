; probability of getting at least 2 sixes when throwing 4 dice

; distribution of six-sided die
(define die
    (uniform '(1 2 3 4 5 6)))

; distribution of n dice
(define (dice n)
    (cond
        ((= n 0)
            (uniform '( '()))) 
        (else
            (concatP die (dice (- n 1))))))

; filter (higher-order function)
(define (filter pred xs)
    (cond
        ((null? xs)
            xs)
        ((pred (car xs))
            (cons (car xs) (filter pred (cdr xs))))
        (else
            (filter pred (cdr xs)))))

; length of list
(define (length xs)
    (cond 
        ((null? xs)
            0)
        (else
            (+ 1 (length (cdr xs))))))

; predicate equal to 6
(define (eq6? x)
    (= 6 x))

; predicate at least 2 sixes
(define (pred xs)
    (>=
        (length (filter eq6? xs))
        2))

; probablity of at least 2 sixes when throwing 4 dice
(?? pred (dice 4))
;    probabilisp> (?? pred (dice 4))
;    OUTPUT: 0.13194445

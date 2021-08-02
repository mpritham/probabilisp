; probability of drawing a red, green, and blue marble (in this order) from a jar
; containing two red, two green, and one blue marble without putting them back

; predicate whether first 3 items are r, g, and b (in that order)
(define (pred xs)
    (let
        ((i1 (car xs))
        (i2 (car (cdr xs)))
        (i3 (car (cdr (cdr xs)))))
    (and
        (eq? i1 'r)
        (and
            (eq? i2 'g)
            (eq? i3 'b)))))

; probability of drawing r g b (in that order) out of 2 r's, 2 g's, and 1 b without
; putting them back
(?? pred (select 3 '('r 'r 'g 'g 'b)))
;    probabilisp> (?? pred (select 3 '('r 'r 'g 'g 'b)))
;    OUTPUT: 6.666667e-2
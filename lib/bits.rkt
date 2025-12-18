;; Some shorthands to make life easier
(define & bitwise-and)
(define << arithmetic-shift)
(define ^ bitwise-xor)
(define bor bitwise-ior)
(:
 >>
 (-> Integer Integer
     Integer))
(define >> (λ (a b) (arithmetic-shift a (* -1 b))))


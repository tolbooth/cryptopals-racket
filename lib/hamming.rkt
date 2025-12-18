;; Count the number of 1's in a byte. Kernighan's method.
;; This is probably slow.
(: count-ones (-> Byte Nonnegative-Integer))
(define (count-ones b)
  (: count-ones-loop (-> Byte Nonnegative-Integer Nonnegative-Integer))
  (define (count-ones-loop b sum)
    (cond [(zero? b) sum]
          [else
           (count-ones-loop (& b (sub1 b))
                            (+ 1 sum))]))
  (count-ones-loop b 0))

;; Compute the hamming distance between two byte strings.
(: hamming (-> Bytes Bytes Nonnegative-Integer))
(define (hamming byt1 byt2)
  (define xord (fixed-xor byt1 byt2))
  (for/fold ([dist : Nonnegative-Integer 0])
            ([byte (in-bytes xord)])
    (+ dist (count-ones byte))))

;; too good to pass up
(: ham-string (-> String String Nonnegative-Integer))
(define (ham-string str1 str2)
  (hamming (hex-encode str1) (hex-encode str2)))


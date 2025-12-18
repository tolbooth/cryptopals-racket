;; Global constants
(define english-freqs : (Listof (Pairof Char Inexact-Real))
  '((#\a . 8.55) (#\k . 0.81) (#\u . 2.68)
                 (#\b . 1.60) (#\l . 4.21) (#\v . 1.06)
                 (#\c . 3.16) (#\m . 2.53) (#\w . 1.83)
                 (#\d . 3.87) (#\n . 7.17) (#\x . 0.19)
                 (#\e . 12.1) (#\o . 7.47) (#\y . 1.72)
                 (#\f . 2.18) (#\p . 2.07) (#\z . 0.11)
                 (#\g . 2.09) (#\q . 0.10)
                 (#\h . 4.96) (#\r . 6.33)
                 (#\i . 7.33) (#\s . 6.73)
                 (#\j . 0.22) (#\t . 8.94)))

;; Very basic frequency table. May need to be improved
(: freq-table (Vectorof Inexact-Real))
(define freq-table
  (let ([vec : (Vectorof Inexact-Real) (make-vector 256 0.0)])
    (for ([pair english-freqs])
      (define char (car pair))
      (define score (cdr pair))
      (vector-set! vec (char->integer char) score)
      (vector-set! vec (char->integer (char-upcase char)) score))
    (vector-set! vec (char->integer #\space) 13.0) ;; Arbitrary, but space is most common
    vec))

;; Structs
(struct Candidate
  ([plaintext : Bytes]
   [key : Byte]
   [score : Inexact-Real]))

;; Functions

(: fixed-xor
   (-> Bytes Bytes
       Bytes))
(define (fixed-xor hex1 hex2)
  (list->bytes
   (for/list : (Listof Byte)
     ([b1 (bytes->list hex1)]
      [b0 (bytes->list hex2)])
     (cast (^ b0 b1) Byte))))

(: fixed-xor-string
   (-> String String
       String))
(define
  (fixed-xor-string str1 str2)
  (bytes->hex-string
   (fixed-xor (hex-string->bytes str1)
              (hex-string->bytes str2))))

(: score-text
   (-> Bytes
       Inexact-Real))
(define (score-text plaintext)
  (for/fold ([total : Inexact-Real 0.0])
            ([byte (in-bytes plaintext)])
    (+ total (vector-ref freq-table byte))))



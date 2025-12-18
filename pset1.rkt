#lang typed/racket
(require typed/rackunit)

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

;; Global constants
(define BASE64-ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define BASE64-LOOKUP
  (let ([vec (make-vector 256 0)])
    (for ([char (in-string BASE64-ALPHABET)]
          [i (in-naturals)])
      (vector-set! vec (char->integer char) i))
    vec))

(define HEX-DIGITS "0123456789abcdef")

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

;; Generic utilities

(: read-lines-as-list (-> String (Listof String)))
(define (read-lines-as-list path)
  (call-with-input-file path
    (lambda ([in : Input-Port])
      (sequence->list (in-lines in)))))

(: read-chunk (-> Bytes Integer Integer Bytes))
(define (read-chunk bytes start size)
  (subbytes bytes start (min (+ start size) (bytes-length bytes))))

;; Convert a single hex char to its Byte value
(: hex-char->int (-> Char Fixnum))
(define (hex-char->int c)
  (define i (char->integer c))
  (cond
    [(<= 48 i 57) (- i 48)]
    [(<= 65 i 70) (- i 55)]
    [(<= 97 i 102) (- i 87)]
    [else (error "Invalid character provided.")]))

;; Convert a hex pair to a Byte value
(: hex-pair->byte (-> Char Char Byte))
(define (hex-pair->byte x1 x0)
  (cast (+ (<< (hex-char->int x1) 4)
           (hex-char->int x0)) Byte))

;; Convert a hexadecimal string to a Byte string
(: hex-string->bytes (-> String Bytes))
(define (hex-string->bytes hex-str)
  (define len (string-length hex-str))
  (unless (even? len)
    (error "Input string must have even length."))
  (define buffer (make-bytes (quotient len 2)))
  (let loop ((i : Nonnegative-Integer 0)
             (j : Nonnegative-Integer 0))
    (cond
      [(= i len) buffer]  ;; Base case, return the buffer
      [else
       (let* ([lo-char (string-ref hex-str i)]
              [hi-char (string-ref hex-str (add1 i))]
              [byte-val (hex-pair->byte lo-char hi-char)])
         (bytes-set! buffer j byte-val)
         (loop (+ i 2) (+ j 1)))])))

;; Decode a file encoded as base64
(: base64-file->bytes (-> String Bytes))
(define (base64-file->bytes path)
  (let* ([lines (file->lines path)]
         [clean-str (apply string-append lines)])
    (base64-decode clean-str)))

;; Convert a Bytes string to a hexidecimal string
(: bytes->hex-string (-> Bytes String))
(define (bytes->hex-string bytes)
  (define len (bytes-length bytes))
  (define out-str (make-string (* len 2)))

  (let loop ([i : Nonnegative-Integer 0])
    (cond
      [(= i len) out-str] ;; Base case, return the string
      [else
       (let ([b (bytes-ref bytes i)])
         (string-set! out-str (* i 2)       (string-ref HEX-DIGITS (>> b 4)))
         (string-set! out-str (add1 (* i 2)) (string-ref HEX-DIGITS (& b #xF)))
         (loop (add1 i)))])))

;; Produce the hex encoding of a given string
(: hex-encode (-> String Bytes))
(define (hex-encode str)
  (list->bytes
   (map char->integer (string->list str))))

;; Get the character corresponding to the base64 integer value
(: base64-val->char (-> Integer Char))
(define (base64-val->char n) (string-ref BASE64-ALPHABET n))

;; Get the base64 integer value corresponding to the character
(: base64-char->val (-> Char Integer))
(define (base64-char->val char)
  (if (char=? char #\=)
      0
      (vector-ref BASE64-LOOKUP (char->integer char))))

;; Convert a Byte string to a base64 encoded string
;; Based on https://datatracker.ietf.org/doc/html/rfc4648#section-4
(: base64-encode (-> Bytes String))
(define (base64-encode bytes)
  (define len (bytes-length bytes))
  (define string (make-string (* (quotient (+ len 2) 3) 4))) ; Pre-allocate string

  (let loop ((i : Nonnegative-Integer 0)
             (j : Nonnegative-Integer 0))
    (define remaining (- len i))
    (cond [(zero? remaining) string]
          [(= remaining 1)
           (let
               ([b0 (bytes-ref bytes i)])
             (string-set! string j
                          (base64-val->char (>> b0 2)))
             (string-set! string (add1 j)
                          (base64-val->char  (<< (& b0 3) 4)))
             (string-set! string (+ j 2) #\=)
             (string-set! string (+ j 3) #\=)
             string)]
          [(= remaining 2)
           (let
               ([b0 (bytes-ref bytes i)]
                [b1 (bytes-ref bytes (+ i 1))])
             (string-set! string j
                          (base64-val->char  (>> b0 2)))
             (string-set! string (+ j 1)
                          (base64-val->char  (bor (<< (& b0 3) 4)
                                                  (>> b1 4))))
             (string-set! string (+ j 2)
                          (base64-val->char  (<< (& b1 15) 2)))
             (string-set! string (+ j 3) #\=)
             string)]
          ;; Main loop, takes chunks of 3 bytes
          [else
           (let ([b0 (bytes-ref bytes i)]
                 [b1 (bytes-ref bytes (+ i 1))]
                 [b2 (bytes-ref bytes (+ i 2))])

             (string-set! string j
                          (base64-val->char  (>> (& b0 252) 2)))
             (string-set! string (+ j 1)
                          (base64-val->char  (+ (<< (& b0 3) 4)
                                                (>> (& b1 240) 4))))
             (string-set! string (+ j 2)
                          (base64-val->char  (+ (<< (& b1 15) 2)
                                                (>> (& b2 192) 6))))
             (string-set! string (+ j 3)
                          (base64-val->char  (+ (& b2 63))))
             (loop (+ i 3) (+ j 4)))])))

;; Convert a base64 encoded string to a Byte string
(: base64-decode (-> String Bytes))
(define (base64-decode str)
  (define len (string-length str))

  ;; Exclude padding characters
  (define pad-count
    (cond
      [(zero? len) 0]
      [(and (> len 1) (char=? (string-ref str (- len 1)) #\=))
       (if (char=? (string-ref str (- len 2)) #\=) 2 1)]
      [else 0]))

  (define out-len (- (* (quotient len 4) 3) pad-count))
  (define bytes (make-bytes out-len))

  (let loop ((i : Nonnegative-Integer 0)
             (j : Nonnegative-Integer 0))
    (if (>= i len)
        bytes
        (let* ([c0 (base64-char->val (string-ref str i))]
               [c1 (base64-char->val (string-ref str (+ i 1)))]
               [c2 (base64-char->val (string-ref str (+ i 2)))]
               [c3 (base64-char->val (string-ref str (+ i 3)))]

               ;; Build our integer
               [bits (bor (<< c0 18)
                          (<< c1 12)
                          (<< c2 6)
                          c3)])
          (bytes-set! bytes j (& (>> bits 16) 255))
          ;; Watch out for padding
          (when (< (+ j 1) out-len)
            (bytes-set! bytes (+ j 1) (& (>> bits 8) 255)))
          (when (< (+ j 2) out-len)
            (bytes-set! bytes (+ j 2) (& bits 255)))

          (loop (+ i 4) (+ j 3))))))

(: hex2b64
   (-> String
       String))
(define
  (hex2b64 hex)
  (let* ([bytes (hex-string->bytes (string-trim hex))])
    (base64-encode bytes)))

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

(struct Candidate
  ([plaintext : Bytes]
   [key : Byte]
   [score : Inexact-Real]))

(: score-text
   (-> Bytes
       Inexact-Real))
(define (score-text plaintext)
  (for/fold ([total : Inexact-Real 0.0])
            ([byte (in-bytes plaintext)])
    (+ total (vector-ref freq-table byte))))

(: break-single-byte-xor
   (-> Bytes
       Candidate))
(define (break-single-byte-xor ciphertext)
  (define init-best (Candidate ciphertext 0 -1.0))

  (for/fold ([current-best : Candidate init-best])
            ([key : Integer (in-range 256)])

    (define plaintext
      (fixed-xor (make-bytes (bytes-length ciphertext) key)
                 ciphertext))
    (define score
      (score-text plaintext))

    (if (> score (Candidate-score current-best))
        (Candidate plaintext (cast key Byte) score)
        current-best)))


(: find-single-byte-xor
   (-> (Listof String)
       Candidate))
(define (find-single-byte-xor messages)
  (define init-best (Candidate #"" 0 -1.0))
  (for/fold ([current-best : Candidate init-best])
            ([str (in-list messages)])
    (define candidate
      (break-single-byte-xor (hex-string->bytes str)))
    (if (> (Candidate-score candidate)
           (Candidate-score current-best))
        candidate
        current-best)))

(: repeating-key-xor-string
   (-> String Bytes
       String))
(define (repeating-key-xor-string str bytes)
  (bytes->hex-string
   (fixed-xor (hex-encode str)
              (list->bytes
               (for/list ([_i (in-range (string-length str))]
                          [b (in-cycle bytes)])
                 b)))))

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

;; Compare edit distance between each of the first 4 chunks
;; Assumes the Byte string is big enough for this
(: score-keysize (-> Bytes Nonnegative-Integer Inexact-Real))
(define (score-keysize bytes size)
  (let* ([num-blocks 4]
         [total-dist
          (for/sum : Nonnegative-Integer ([i (in-range (sub1 num-blocks))])
            (let ([b0 (read-chunk bytes (* i size) (* (add1 i) size))]
                  [b1 (read-chunk bytes (* (add1 i) size) (* (+ i 2) size))])
              (hamming b0 b1)))])
    (exact->inexact (/ total-dist (sub1 num-blocks) size))))

;; Return the top 3 keysizes by edit distance
(: find-top-keysizes
   (-> Bytes (Listof Nonnegative-Integer)))
(define (find-top-keysizes bytes)
  (take
   (sort (range 2 41)
         (λ ([s1 : Nonnegative-Integer] [s2 : Nonnegative-Integer])
           (< (score-keysize bytes s1)
              (score-keysize bytes s2))))
   3))

;; Produce a vector of bytes corresponding to the transpose of the input 
;; Byte string. Each vector consists of characters read with stride keysize.
(: transpose-keysize
   (-> Bytes Nonnegative-Integer Nonnegative-Integer (Vectorof Bytes)))
(define
  (transpose-keysize bytes keysize len)
  (for/vector : (Vectorof Bytes) ([i (in-range keysize)])
    (let* ([block-size (quotient (+ len (- keysize i 1)) keysize)]
           [buf (make-bytes block-size)])
      (for ([j (in-range block-size)])
        (let ([idx (+ i (* j keysize))])
          (when (< idx len)
            (bytes-set! buf j (bytes-ref bytes idx)))))
      buf)))

;; Play that funky music...
(: break-repeating-key-xor
   (-> Bytes
       Bytes))
(define (break-repeating-key-xor bytes)
  (define len (bytes-length bytes))
  (define keysizes (find-top-keysizes bytes))
  (argmax
   (λ ([plaintext : Bytes]) (score-text plaintext))
   (for/list : (Listof Bytes) ([size (in-list keysizes)])
     (let* ([blocks (transpose-keysize bytes size len)]
            [candidates (vector-map break-single-byte-xor blocks)]
            [key (list->bytes
                  (for/list ([c (in-vector candidates)])
                    (Candidate-key c)))])
       (fixed-xor bytes
                  (list->bytes
                   (for/list ([_i (in-range len)]
                              [b (in-cycle key)]) ;; This is not a good way to do things
                     b)))))))





;; =========== TESTS =============

;; Utility tests
(check-equal? (hex-string->bytes "deadbeef")
              (bytes #xde #xad #xbe #xef))
(check-equal? (hex-string->bytes "48656c6c6f")
              #"Hello")
(check-equal? (hex-string->bytes "aB1C9f")
              (bytes #xab #x1c #x9f))
(check-equal? (hex-string->bytes "")
              #"")

(check-exn exn:fail?
           (lambda () (hex-string->bytes "abc"))
           "Should throw an error if input length is odd")
(check-exn exn:fail?
           (lambda () (hex-string->bytes "zz"))
           "Should throw an error if input contains invalid characters")

(check-equal? (bytes->hex-string (bytes #xde #xad #xbe #xef))
              "deadbeef")
(check-equal? (bytes->hex-string #"Hello")
              "48656c6c6f")
(check-equal? (bytes->hex-string (bytes #xab #x1c #x9f))
              "ab1c9f")
(check-equal? (bytes->hex-string #"")
              "")

(check-equal? (count-ones 0)
              0)
(check-equal? (count-ones (hex-pair->byte #\F #\F))
              8)
(check-equal? (count-ones (hex-pair->byte #\A #\A))
              4)
(check-equal? (ham-string "this is a test" "wokka wokka!!!")
              37)


;; Challenges

;; Set 1, Challenge 1
(check-equal? (hex2b64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
              "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

;; Set 1, Challenge 2
(check-equal? (fixed-xor-string "1c0111001f010100061a024b53535009181c"
                                "686974207468652062756c6c277320657965")
              "746865206b696420646f6e277420706c6179")

;; Set 1, Challenge 3
(define challenge-3-secret
  (break-single-byte-xor (hex-string->bytes
                          "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")))
(Candidate-plaintext challenge-3-secret)
(Candidate-key challenge-3-secret)

;; Set 1, Challenge 4
(define challenge-4-secret
  (find-single-byte-xor (read-lines-as-list "./data/4.txt")))
(Candidate-plaintext challenge-4-secret)
(Candidate-key challenge-4-secret)

;; Set 1, Challenge 5
(define challenge-5-plaintext
  "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal")
(define challenge-5-ciphertext
  "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
(check-equal? (repeating-key-xor-string challenge-5-plaintext #"ICE")
              challenge-5-ciphertext)

;; Set 1, Challenge 6
(define challenge-6-secret
  (break-repeating-key-xor
   (base64-file->bytes "./data/6.txt")))
(displayln challenge-6-secret)

#lang typed/racket
(require typed/rackunit)
(require racket/file)

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
              [hi-char (string-ref hex-str (+ i 1))]
              [byte-val (hex-pair->byte lo-char hi-char)])
         (bytes-set! buffer j byte-val)
         (loop (+ i 2) (+ j 1)))])))

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
         (string-set! out-str (+ (* i 2) 1) (string-ref HEX-DIGITS (& b #xF)))
         (loop (+ i 1)))])))

;; Produce the hex encoding of a given string
(: hex-encode (-> String Bytes))
(define (hex-encode str)
  (list->bytes
    (map char->integer (string->list str))))

;; Get the character corresponding to the base64 integer value
(: base64-char (-> Integer Char))
(define (base64-char n) (string-ref BASE64-ALPHABET n))

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
             (string-set! string j (base64-char (>> b0 2)))
             (string-set! string (+ j 1) (base64-char (<< (& b0 3) 4)))
             (string-set! string (+ j 2) #\=)
             (string-set! string (+ j 3) #\=)
             string)]
          [(= remaining 2)
           (let
               ([b0 (bytes-ref bytes i)]
                [b1 (bytes-ref bytes (+ i 1))])
             (string-set! string j (base64-char (>> b0 2)))
             (string-set! string (+ j 1)
                          (base64-char (bor (<< (& b0 3) 4)
                                            (>> b1 4))))
             (string-set! string (+ j 2)
                          (base64-char (<< (& b1 15) 2)))
             (string-set! string (+ j 3) #\=)
             string)]
          ;; Main loop, takes chunks of 3 bytes
          [else
           (let ([b0 (bytes-ref bytes i)]
                 [b1 (bytes-ref bytes (+ i 1))]
                 [b2 (bytes-ref bytes (+ i 2))])

             (string-set! string j (base64-char (>> (& b0 252) 2)))
             (string-set! string (+ j 1)
                          (base64-char (+ (<< (& b0 3) 4)
                                          (>> (& b1 240) 4))))
             (string-set! string (+ j 2)
                          (base64-char (+ (<< (& b1 15) 2)
                                          (>> (& b2 192) 6))))
             (string-set! string (+ j 3) (base64-char (+ (& b2 63))))
             (loop (+ i 3) (+ j 4)))])))

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
   (-> String
       Candidate))
(define (break-single-byte-xor input)
  (define ciphertext (hex-string->bytes input))
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

(: read-lines-as-list (-> String (Listof String)))
(define (read-lines-as-list path)
  (call-with-input-file path
    (lambda ([in : Input-Port])
      (sequence->list (in-lines in)))))

(: find-single-byte-xor
   (-> (Listof String)
       Candidate))
(define (find-single-byte-xor messages)
  (define init-best (Candidate #"" 0 -1.0))
  (for/fold ([current-best : Candidate init-best])
            ([str (in-list messages)])
    (define candidate
      (break-single-byte-xor str))
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
  (break-single-byte-xor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
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

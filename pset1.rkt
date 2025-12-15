#lang typed/racket
(require typed/rackunit)

;; Some shorthands to make life easier
(define & bitwise-and)
(define << arithmetic-shift)
(define bor bitwise-ior)
(:
 >>
 (-> Integer Integer
     Integer))
(define >> (λ (a b) (arithmetic-shift a (* -1 b))))

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
      ;; Base case, return the buffer
      [(= i len) buffer]
      [else
       (let* ([lo-char (string-ref hex-str i)]
              [hi-char (string-ref hex-str (+ i 1))]
              [byte-val (hex-pair->byte lo-char hi-char)])
         (bytes-set! buffer j byte-val)
         (loop (+ i 2) (+ j 1)))])))

;; Get the character corresponding to the base64 integer value
(define BASE64-ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
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
   (-> String String
       String))
(define
  (fixed-xor hex1 hex2)
  (string-append hex1 hex2)) ;; TODO



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

;; Set 1, Challenge 1
(check-equal? (hex2b64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
              "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

;; Set 1, Challenge 2
(check-equal? (fixed-xor "1c0111001f010100061a024b53535009181c"
                         "686974207468652062756c6c277320657965")
              "746865206b696420646f6e277420706c6179")


#lang typed/racket
(require typed/rackunit)
(include "./lib/bits.rkt")
(include "./lib/crypto.rkt")
(include "./lib/encode.rkt")
(include "./lib/files.rkt")
(include "./lib/hamming.rkt")

(: hex2b64
   (-> String
       String))
(define
  (hex2b64 hex)
  (let* ([bytes (hex-string->bytes (string-trim hex))])
    (base64-encode bytes)))

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

(: repeating-key-xor
   (-> Bytes Bytes
       Bytes))
(define (repeating-key-xor bytes key)
  (fixed-xor bytes
             (list->bytes
              (for/list ([_i (in-range (bytes-length bytes))]
                         [b (in-cycle key)])
                b))))

(: repeating-key-xor-string
   (-> String Bytes
       String))
(define (repeating-key-xor-string str bytes)
  (bytes->hex-string
   (repeating-key-xor (hex-encode str) bytes)))

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
       (repeating-key-xor bytes key)))))


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

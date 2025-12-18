;; Global Constants

(define BASE64-ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define BASE64-LOOKUP
  (let ([vec (make-vector 256 0)])
    (for ([char (in-string BASE64-ALPHABET)]
          [i (in-naturals)])
      (vector-set! vec (char->integer char) i))
    vec))

(define HEX-DIGITS "0123456789abcdef")

;; Hexademical Handling

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

;; Produce the hex encoding of a given string
(: hex-encode (-> String Bytes))
(define (hex-encode str)
  (list->bytes
   (map char->integer (string->list str))))

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

;; Base64 B-bungling?

;; Decode a file encoded as base64
(: base64-file->bytes (-> String Bytes))
(define (base64-file->bytes path)
  (let* ([lines (file->lines path)]
         [clean-str (apply string-append lines)])
    (base64-decode clean-str)))

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


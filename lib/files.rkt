;; Generic utilities
(: read-lines-as-list (-> String (Listof String)))
(define (read-lines-as-list path)
  (call-with-input-file path
    (lambda ([in : Input-Port])
      (sequence->list (in-lines in)))))

(: read-chunk (-> Bytes Integer Integer Bytes))
(define (read-chunk bytes start size)
  (subbytes bytes start (min (+ start size) (bytes-length bytes))))

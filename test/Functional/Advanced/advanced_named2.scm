(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (parity-score n)
  (if (eq? (mod n 2) 0)
      1
      -1))

(define (avg a b)
  (div (+ a b) 2))

(define apply-twice
  (lambda (f x)
    (f (f x))))

(define (inc x)
  (+ x 1))

(define (complex a b)
  (* (parity-score (apply-twice (lambda (x) (inc x)) a))
     (+ (abs (- (avg a b) b))
        (mod b 3))))

(complex 3 10)

(define (sign x)
  (if (eq? x 0)
      0
      (if (< x 0)
          -1
          1)))

(define (max2 a b)
  (if (< a b) b a))

(define (min2 a b)
  (if (< a b) a b))

(define clamp
  (lambda (x lo hi)
    (max2 lo (min2 x hi))))

(define (poly a b c x)
  (+ (+ (* a (* x x))
        (* b x))
     c))

(define p  (poly 1 2 3 4))
(define c1 (clamp -3 0 10))
(define c2 (clamp 42 0 10))
(define s  (+ (sign -10) (+ (sign 0) (sign 5))))

(define result (+ (+ p c1) (+ c2 s)))

result

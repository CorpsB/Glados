(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (gcd a b)
  (if (eq? b 0)
      (abs a)
      (gcd b (mod a b))))

(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))

(define max2
  (lambda (a b)
    (if (< a b) b a)))

(define result
  (+ (gcd 48 18)
     (div (fact (max2 2 4)) 2)))

result

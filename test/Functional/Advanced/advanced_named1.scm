(define (is-divisible n d)
  (eq? (mod n d) 0))

(define (is-prime-helper n d)
  (if (< n (* d d))
      #t
      (if (eq? (mod n d) 0)
          #f
          (is-prime-helper n (+ d 1)))))

(define (is-prime n)
  (if (< n 2)
      #f
      (is-prime-helper n 2)))

(define (sum-primes a b)
  (if (< b a)
      0
      (if (is-prime b)
          (+ b (sum-primes a (- b 1)))
          (sum-primes a (- b 1)))))

(sum-primes 1 20)

# Documentation — Math Library

_Technical documentation of mathematical modules._

**Contents:** description of functions, prototypes, and dependencies.

## Table of Contents
- Overview and dependencies
- basic.npy
- combin.npy
- div.npy
- gcd.npy
- module.npy
- ntheory.npy
- pow2.npy
- power.npy
- prime.npy
- range.npy
- sqrt.npy

---

## Overview and dependencies

- **Location:** src/@lib/maths/
- **Purpose:** provide mathematical utilities (integer arithmetic, number theory, powers, roots, etc.).
- **Internal dependency note:** several functions rely on `modPos` (or `pmod`) and `isSqrt`; `modPos` guarantees a positive remainder, and `isSqrt` computes the integer square root.

---

## basic

**File:** src/@lib/maths/basic.npy

- **Description:** simple utility functions (absolute value, sign, min/max, clamping, parity checks, positive modulo).

### Functions

**Prototype:**
```
func abs(x: int) -> int
```
Returns the absolute value of `x`.

**Prototype:**
```
func sign(x: int) -> int
```
Returns `-1` if `x < 0`, `0` if `x == 0`, `1` otherwise.

**Prototype:**
```
func min(a: int, b: int) -> int
```
Returns the smaller of the two values.

**Prototype:**
```
func max(a: int, b: int) -> int
```
Returns the larger of the two values.

**Prototype:**
```
func clamp(x: int, lo: int, hi: int) -> int
```
Clamps `x` to the interval `[lo, hi]`.

**Prototype:**
```
func isEven(n: int) -> bool
```
Returns `True` if `n` is even.

**Prototype:**
```
func isOdd(n: int) -> bool
```
Returns `True` if `n` is odd.

**Prototype:**
```
func modPos(a: int, m: int) -> int
```
Computes the positive remainder of `a` modulo `m`. If `m <= 0`, returns `0`.

---

## combin

**File:** src/@lib/maths/combin.npy

- **Description:** combinatorial functions (factorial, permutations, binomials), implemented iteratively.

### Functions

**Prototype:**
```
func fact(n: int) -> int
```
Iterative factorial. Returns `0` if `n < 0`.

**Prototype:**
```
func perm(n: int, k: int) -> int
```
Computes P(n,k). Returns `0` if parameters are invalid.

**Prototype:**
```
func binom(n: int, k: int) -> int
```
Binomial coefficient C(n,k), optimized using symmetry.

---

## div

**File:** src/@lib/maths/div.npy

- **Description:** safe integer divisions.

### Functions

**Prototype:**
```
func divTrunc(a: int, b: int) -> int
```
Integer division. Returns `0` if `b == 0`.

**Prototype:**
```
func divCeil(a: int, b: int) -> int
```
Ceiling integer division.

---

## gcd

**File:** src/@lib/maths/gcd.npy

- **Description:** GCD, LCM, and coprimality.

### Functions

**Prototype:**
```
func gcd(a: int, b: int) -> int
```
Iterative Euclidean algorithm.

**Prototype:**
```
func lcm(a: int, b: int) -> int
```
Least common multiple.

**Prototype:**
```
func coPrime(a: int, b: int) -> bool
```
Returns `True` if `a` and `b` are coprime.

---

## module

**File:** src/@lib/maths/module.npy

- **Description:** aggregation file importing all math modules.

---

## ntheory

**File:** src/@lib/maths/ntheory.npy

- **Description:** number theory utilities.

### Functions

**Prototype:**
```
func phi(n: int) -> int
```
Euler's totient function.

**Prototype:**
```
func modinv(a: int, m: int) -> int
```
Modular inverse using extended Euclidean algorithm.

---

## pow2

**File:** src/@lib/maths/pow2.npy

- **Description:** powers of two utilities.

### Functions

**Prototype:**
```
func isPow2(n: int) -> bool
```

**Prototype:**
```
func nextPow2(n: int) -> int
```

---

## power

**File:** src/@lib/maths/power.npy

- **Description:** integer powers and modular exponentiation.

### Functions

**Prototype:**
```
func pow(base: int, exp: int) -> int
```

**Prototype:**
```
func pow10(exp: int) -> int
```

**Prototype:**
```
func modPow(base: int, exp: int, m: int) -> int
```

---

## prime

**File:** src/@lib/maths/prime.npy

- **Description:** primality testing.

### Functions

**Prototype:**
```
func isPrime(n: int) -> bool
```

**Prototype:**
```
func nextPrime(n: int) -> int
```

---

## range

**File:** src/@lib/maths/range.npy

- **Description:** integer range operations.

### Functions

**Prototype:**
```
func sumRange(from: int, to: int) -> int
```

**Prototype:**
```
func prodRange(from: int, to: int) -> int
```

---

## sqrt

**File:** src/@lib/maths/sqrt.npy

- **Description:** integer square root utilities.

### Functions

**Prototype:**
```
func isSqrt(n: int) -> int
```

**Prototype:**
```
func isSquare(n: int) -> bool
```

---

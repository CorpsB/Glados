# Documentation — Bibliothèque Maths

_Documentation technique des modules mathématiques._

**Contenu :** description des fonctions, prototypes et dépendances.

## Sommaire
- [Aperçu et dépendances](#aper%C3%A7u-et-d%C3%A9pendances)
- [basic.npy](#basic)
- [combin.npy](#combin)
- [div.npy](#div)
- [gcd.npy](#gcdnpy)
- [module.npy](#module)
- [ntheory.npy](#ntheory)
- [pow2.npy](#pow2)
- [power.npy](#power)
- [prime.npy](#prime)
- [range.npy](#range)
- [sqrt.npy](#sqrt)

---

## Aperçu et dépendances

- **Emplacement :** src/@lib/maths/
- **But :** fournir des utilitaires mathématiques (arithmétique entière, théorie des nombres, puissances, racines, etc.).
- **Remarque sur les dépendances internes :** plusieurs fonctions utilisent `modPos` (ou `pmod`) et `isSqrt` ; `modPos` garantit un reste positif, `isSqrt` calcule la racine entière.

---

## basic

**Fichier :** [src/@lib/maths/basic.npy](src/@lib/maths/basic.npy)

- **Description :** fonctions utilitaires simples (absolu, signe, min/max, bornes, parités, modulo positif).

### Fonctions

- **Prototype :**
```
func abs(x: int) -> int
```
*Retourne la valeur absolue de `x`.*

- **Prototype :**
```
func sign(x: int) -> int
```
*Renvoie `-1` si `x < 0`, `0` si `x == 0`, `1` sinon.*

- **Prototype :**
```
func min(a: int, b: int) -> int
```
*Renvoie la plus petite des deux valeurs.*

- **Prototype :**
```
func max(a: int, b: int) -> int
```
*Renvoie la plus grande des deux valeurs.*

- **Prototype :**
```
func clamp(x: int, lo: int, hi: int) -> int
```
*Borne `x` dans l'intervalle `[lo, hi]`.*

- **Prototype :**
```
func isEven(n: int) -> bool
```
*Retourne `True` si `n` est pair, `False` sinon.*

- **Prototype :**
```
func isOdd(n: int) -> bool
```
*Retourne `True` si `n` est impair (implémentée comme l'opposée de `isEven`).*

- **Prototype :**
```
func modPos(a: int, m: int) -> int
```
*Calcule le reste positif de `a` modulo `m` : si `m <= 0` retourne `0`. Sinon `r = a % m` et retourne `r + m` si `r < 0`, sinon `r`.*

---

## combin

**Fichier :** [src/@lib/maths/combin.npy](src/@lib/maths/combin.npy)

- **Description :** fonctions combinatoires (factorielle, permutations, binomiaux) implémentées itérativement.

### Fonctions

- **Prototype :**
```
func fact(n: int) -> int
```
*Factorielle itérative ; retourne `0` si `n < 0`. Utilise un accumulateur `acc` et boucle `i` de `2` à `n`.*

- **Prototype :**
```
func perm(n: int, k: int) -> int
```
*Calcul de P(n,k) = n*(n-1)*...*(n-k+1) itératif. Retourne `0` si `n<0` ou `k<0` ou `k>n`.*

- **Prototype :**
```
func binom(n: int, k: int) -> int
```
*Coefficient binomial C(n,k) optimisé : réduit `k` à `min(k, n-k)` et calcule par multiplication/division itératives. Retours d'erreur simples (`0`) si paramètres invalides.*

---

## div

**Fichier :** [src/@lib/maths/div.npy](src/@lib/maths/div.npy)

- **Description :** divisions entières sécurisées (tronquée et plafond) avec gestion `b == 0`.

### Fonctions

- **Prototype :**
```
func divTrunc(a: int, b: int) -> int
```
*Division entière classique `a / b`. Si `b == 0` retourne `0`.*

- **Prototype :**
```
func divCeil(a: int, b: int) -> int
```
*Division entière arrondie vers le haut : calcule `q = a / b` et `r = a % b`. Si `r == 0` retourne `q`; sinon ajuste selon les signes de `a` et `b`.*

---

## gcd

**Fichier :** [src/@lib/maths/gcd.npy](src/@lib/maths/gcd.npy)

- **Description :** plus grand commun diviseur, PPCM, test de coprimalité.

### Fonctions

- **Prototype :**
```
func gcd(a: int, b: int) -> int
```
*Algorithme d'Euclide itératif appliqué aux valeurs absolues ; gère les cas où un argument est zéro.*

- **Prototype :**
```
func lcm(a: int, b: int) -> int
```
*Plus petit commun multiple : si `a == 0 || b == 0` retourne `0`. Calcule `g = gcd(a,b)` et `abs((a / g) * b)` pour réduire le risque d'overflow.*

- **Prototype :**
```
func coPrime(a: int, b: int) -> bool
```
*Retourne `True` si `gcd(a,b) == 1`.*

---

## module

**Fichier :** [src/@lib/maths/module.npy](src/@lib/maths/module.npy)

- **Description :** fichier d'agrégation qui importe et regroupe les modules mathématiques suivants :

```
basic.npy, div.npy, gcd.npy, power.npy, pow2.npy, sqrt.npy, prime.npy, range.npy, combin.npy, ntheory.npy
```

*Utiliser ce fichier pour charger l'ensemble des utilitaires.*

---

## ntheory

**Fichier :** [src/@lib/maths/ntheory.npy](src/@lib/maths/ntheory.npy)

- **Description :** fonctions de théorie des nombres : indicatrice d'Euler et inverse modulaire via Euclide étendu.

### Fonctions

- **Prototype :**
```
func phi(n: int) -> int
```
*Calcul de φ(n) : si `n <= 0` retourne `0`. Parcours des facteurs premiers p jusqu'à sqrt(x), met à jour `result` suivant la formule multiplicative.*

- **Prototype :**
```
func modinv(a: int, m: int) -> int
```
*Inverse modulaire de `a` modulo `m` via l'algorithme d'Euclide étendu. Si `m <= 1` retourne `0`. Initialise `(t, nt) = (0,1)` et `(r, nr) = (m, pmod(a,m))` puis boucle. Si `r != 1` il n'existe pas d'inverse (retourne `0`). Si `t < 0` l'ajuste en `t + m` avant de le retourner.*

**Remarque importante :** la fonction appelle `pmod(a, m)` — dans cette base de code la fonction équivalente fournie est `modPos(a, m)` (dans `basic.npy`). Vérifier la présence exacte de `pmod` sinon remplacer par `modPos`.

---

## pow2

**Fichier :** [src/@lib/maths/pow2.npy](src/@lib/maths/pow2.npy)

- **Description :** vérification et recherche de puissances de deux.

### Fonctions

- **Prototype :**
```
func isPow2(n: int) -> bool
```
*Retourne `True` si `n` est une puissance de deux strictement positive.*

- **Prototype :**
```
func nextPow2(n: int) -> int
```
*Renvoie la plus petite puissance de deux >= `n`. Si `n <= 1` retourne `1`.*

---

## power

**Fichier :** [src/@lib/maths/power.npy](src/@lib/maths/power.npy)

- **Description :** calculs de puissances entières et exponentiation modulaire.

### Fonctions

- **Prototype :**
```
func pow(base: int, exp: int) -> int
```
*Puissance entière (exponentiation binaire). Si `exp < 0` retourne `0`.*

- **Prototype :**
```
func pow10(exp: int) -> int
```
*Calcule `10^exp` par multiplication itérative ; si `exp < 0` retourne `0`.*

- **Prototype :**
```
func modPow(base: int, exp: int, m: int) -> int
```
*Exponentiation modulaire : si `m <= 0` ou `exp < 0` retourne `0`. Si `m == 1` retourne `0`. Utilise `res = 1 % m` et `b = modPos(base, m)` avant la boucle binaire.*

---

## prime

**Fichier :** [src/@lib/maths/prime.npy](src/@lib/maths/prime.npy)

- **Description :** tests de primalité simples et recherche du prochain premier.

### Fonctions

- **Prototype :**
```
func isPrime(n: int) -> bool
```
*Test naïf : rejette `n <= 1`, accepte `n <= 3`, élimine pairs, teste les diviseurs impairs jusqu'à `isSqrt(n)`. Retourne `True` si premier.*

- **Prototype :**
```
func nextPrime(n: int) -> int
```
*Renvoie le premier nombre premier >= `n` (gère la parité en incrémentant par 2). Si `n <= 2` retourne `2`.*

---

## range

**Fichier :** [src/@lib/maths/range.npy](src/@lib/maths/range.npy)

- **Description :** opérations sur des intervalles entiers (somme, produit).

### Fonctions

- **Prototype :**
```
func sumRange(from: int, to: int) -> int
```
*Somme des entiers de `from` à `to` inclus ; si `from > to` retourne `0`.*

- **Prototype :**
```
func prodRange(from: int, to: int) -> int
```
*Produit des entiers de `from` à `to` inclus ; si `from > to` retourne `1`.*

---

## sqrt

**Fichier :** [src/@lib/maths/sqrt.npy](src/@lib/maths/sqrt.npy)

- **Description :** racine entière et test de carré parfait.

### Fonctions

- **Prototype :**
```
func isSqrt(n: int) -> int
```
*Recherche binaire pour la racine entière (plancher). Si `n <= 0` retourne `0`.*

- **Prototype :**
```
func isSquare(n: int) -> bool
```
*Retourne `True` si `n` est un carré parfait (utilise `isSqrt`). Retourn `False` pour `n < 0`.*

---


## Exemples d'utilisation

_Petits snippets d'appel pour chaque fonction (exemples illustratifs)._ 

### basic.npy
```
abs(-5)         // -> 5
sign(-3)        // -> -1
min(2, 3)       // -> 2
max(2, 3)       // -> 3
clamp(10, 0, 5) // -> 5
isEven(4)       // -> True
isOdd(3)        // -> True
modPos(-3, 5)   // -> 2
```

### combin.npy
```
fact(5)     // -> 120
perm(5, 3)  // -> 5*4*3 = 60
binom(5, 2) // -> 10
```

### div.npy
```
divTrunc(7, 3)  // -> 2
divCeil(7, 3)   // -> 3
divCeil(-7, 3)  // -> -2 (selon gestion des signes)
```

### gcd.npy
```
gcd(12, 18)    // -> 6
lcm(4, 6)      // -> 12
coPrime(14,15) // -> True
```

### module.npy
```
import "src/@lib/maths/module.npy";
// Permet d'accéder à toutes les fonctions documentées ici via les imports habituels du projet.
```

### ntheory.npy
```
phi(10)      // -> 4 (1,3,7,9)
modinv(3, 11) // -> 4  (car 3*4 % 11 == 1)
```

### pow2.npy
```
isPow2(8)   // -> True
nextPow2(5) // -> 8
```

### power.npy
```
pow(2, 10)        // -> 1024
pow10(3)          // -> 1000
modPow(3, 13, 17) // -> 3^13 % 17 -> 12 (exemple)
```

### prime.npy
```
isPrime(17)   // -> True
nextPrime(14) // -> 17
```

### range.npy
```
sumRange(1, 5)  // -> 15
prodRange(1, 4) // -> 24
```

### sqrt.npy
```
isSqrt(17)   // -> 4
isSquare(16) // -> True
```

---

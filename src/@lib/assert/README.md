# Bibliothèque d'Assertion (assert.npy)

Ce module fournit un ensemble de fonctions utilitaires et d'assertions pour faciliter les tests et la validation du code.

## Fonctions d'Assertion

Ces fonctions sont utilisées pour vérifier des invariants ou des résultats de tests. Si une assertion échoue, un message d'erreur est affiché et le programme se termine immédiatement avec un code d'erreur (`exit(1)`).

### `assert`
- **Signature**: `func assert(cond: bool, msg: [char])`
- **Arguments**:
  - `cond`: La condition à vérifier.
  - `msg`: Le message à afficher en cas d'échec.
- **Description**: Vérifie que `cond` est vrai. Si c'est faux, affiche "Assertion check failed: " suivi du message utilisateur `msg` et quitte le programme.

### `assert_eq_int`
- **Signature**: `func assert_eq_int(a: int, b: int)`
- **Arguments**:
  - `a`: Premier entier (valeur obtenue).
  - `b`: Deuxième entier (valeur attendue).
- **Description**: Vérifie que `a` est égal à `b`. En cas d'échec, affiche les valeurs comparées.

### `assert_neq_int`
- **Signature**: `func assert_neq_int(a: int, b: int)`
- **Arguments**:
  - `a`: Premier entier.
  - `b`: Deuxième entier.
- **Description**: Vérifie que `a` est différent de `b`. En cas d'échec (si `a == b`), affiche une erreur.

### `assert_eq_bool`
- **Signature**: `func assert_eq_bool(a: bool, b: bool)`
- **Arguments**:
  - `a`: Premier booléen.
  - `b`: Deuxième booléen.
- **Description**: Vérifie que deux valeurs booléennes sont identiques.

### `assert_true`
- **Signature**: `func assert_true(cond: bool)`
- **Arguments**:
  - `cond`: La condition à tester.
- **Description**: Vérifie simplement que la condition donnée est vraie.

### `assert_false`
- **Signature**: `func assert_false(cond: bool)`
- **Arguments**:
  - `cond`: La condition à tester.
- **Description**: Vérifie simplement que la condition donnée est fausse.

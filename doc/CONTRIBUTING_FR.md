# Guide de Contribution – Projet Glados

Merci de votre intérêt pour la contribution au projet **Glados**.  
Ce document décrit les règles, bonnes pratiques et procédures nécessaires pour contribuer efficacement.

---

## 1. Présentation Générale

Le dépôt est **public** et sous licence **MIT**.  
Le projet est développé en **Haskell**, s’appuie sur **stack**, et officiellement supporté uniquement sous **Linux**.

L’objectif de ce guide est de garantir un flux de travail cohérent, une qualité de code stable et une intégration fluide dans la CI.

---

## 2. Pré-requis

- Linux (distribution au choix)
- `stack`
- `make`
- Git (dernière version raisonnable)
- Un éditeur compatible Haskell (VSCode + extensions, Neovim + TreeSitter, etc.)

---

## 3. Installation et Build

Après avoir cloné le dépôt :

```sh
stack setup
stack build
```

Le projet fournit plusieurs règles `make` :

```sh
make        # compile
make re     # recompile proprement
make clean  # supprime les artefacts
make fclean # supprime les artefacts + dépendances
make test   # exécute les tests unitaires et fonctionnels
make doc    # génère/compile la documentation
```

---

## 4. Tests

Les tests sont exécutés automatiquement via **Woodpecker CI**, mais doivent également être lancés en local.

### Structure

- `test/Unit` → **tests unitaires**
- `test/Functional` → **tests fonctionnels**

### Règles

- Chaque fichier de `src/` doit avoir un fichier de test correspondant dans `test/Unit`, organisé de la même manière.
- **Toute nouvelle fonctionnalité doit être accompagnée de tests unitaires.**
- Les tests fonctionnels sont rédigés par le DOP du projet.
- Les PR sans tests unitaires (lorsque pertinent) seront refusées.

Lancer l’ensemble des tests :

```sh
make test
```

---

## 5. Style de Code

### Style Haskell

- Le code doit suivre le **coding style Epitech**.
- Exceptions :
  - `main.hs` → libre
  - tests unitaires → libres

### Formatage

Pas d’outil imposé, mais un style propre et cohérent est attendu.

---

## 6. Conventions Git

### Branches

Chaque contribution doit se faire dans une nouvelle branche nommée selon la convention :

```
Feat/NomDeTaFeature
Fix/NomDuFix
Doc/NomDocumentation
Refactor/NomDeRefactor
```

Le préfixe est **obligatoire**, avec une majuscule.

### Workflow

- **Interdiction de push directement sur `main` et `preprod`.**
- Toute branche doit être fusionnée **d’abord** dans `preprod`.
- Une fois validé, `preprod` est fusionnée dans `main`.
- Chaque merge vers `main` génère automatiquement une **nouvelle release**.
- 2 reviews minimum sont nécessaires pour valider une PR.

### Commits

Le projet utilise une norme de commit basée sur un script :

```
tools/commit.sh
```

- L’utilisation du script est **fortement recommandée**.
- Vous pouvez commiter manuellement **uniquement** si votre message respecte strictement la norme du script.
- Une PR contenant des commits non conformes sera refusée.

### PR

- La description est libre.
- PR refusée si la pipeline CI échoue.
- Les merges se font en **squash**, ne conservant qu’un commit final (les autres sont ajoutés en commentaire).

---

## 7. Intégration Continue (Woodpecker CI)

Les pipelines s’exécutent selon différents triggers (`push`, `pull_request`), selon les règles définies dans `.woodpecker.yml`.

La CI :

- compile le projet,
- exécute les tests unitaires et fonctionnels,
- vérifie la qualité du code,
- **déploie la documentation lors des releases** (merge vers `main`).

En cas d’échec :

> Corrigez votre code, puis poussez une nouvelle version.

---

## 8. Documentation

La documentation est stockée dans :

```
doc/
```

Format : **Markdown**

Vous pouvez contribuer à la documentation en :

- corrigeant des fautes,
- améliorant l’explication de certaines features,
- ajoutant des exemples pertinents.

---

## 9. Rapport de Bugs & Suggestions

Merci d’utiliser le système d’issues GitHub.

Pour un bug :

- description claire,
- étapes de reproduction,
- fichier d’entrée (si applicable),
- logs éventuels.

Pour une feature :

- expliquez le besoin,
- pourquoi elle est utile,
- comment elle s’intègre dans Glados.

---

## 10. Comportement Attendu

- Respect, bienveillance et communication constructive.
- Relire avant de soumettre.
- Ne jamais “force push” sur une branche critique.
- Ne jamais casser la build de `preprod`.

---

## 11. Conclusion

Merci de contribuer à l’évolution du projet Glados !  
Pour toute question, ouvrez une issue ou contactez un mainteneur du dépôt.


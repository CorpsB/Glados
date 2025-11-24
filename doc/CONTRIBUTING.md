# Contribution Guide – Glados Project

Thank you for your interest in contributing to **Glados**.
This document describes the rules, workflows, and good practices required for contributing efficiently.

---

## 1. General Overview

The repository is **public** and licensed under **MIT**.
The project is written in **Haskell**, built using **stack**, and officially supported only on **Linux**.

This guide aims to ensure consistency, code quality, and smooth integration with the CI pipelines.

---

## 2. Requirements

- Linux (any distribution)
- `stack`
- `make`
- Git
- A Haskell-friendly editor (VSCode + extensions, Neovim + TreeSitter, etc.)

---

## 3. Installation & Build

After cloning the repository:

```
stack setup
stack build
```

Available `make` rules:

```
make        # compile
make re     # rebuild cleanly
make clean  # remove build artifacts
make fclean # remove artifacts + dependencies
make test   # run all tests
make doc    # build the documentation
```

---

## 4. Tests

Tests are executed automatically through **Woodpecker CI**, but must also run locally before submitting any work.

### Structure

- `test/Unit` → **unit tests**
- `test/Functional` → **functional tests**

### Rules

- Each file in `src/` must have a corresponding test file in `test/Unit`, mirroring the directory structure.
- **Every new feature must come with its own unit tests.**
- Functional tests are handled by the project DOP.
- PRs without unit tests (when applicable) will be rejected.

Run all tests locally:

```
make test
```

---

## 5. Code Style

### Haskell Style

- All code must follow the **Epitech Haskell coding style**.
- Exceptions:
  - `main.hs` → free style
  - Unit tests → free style

### Formatting

No formatter is strictly enforced, but clean and consistent code is required.

---

## 6. Git Workflow

### Branch Naming

All contributions must be done through dedicated branches following this naming convention:

```
Feat/YourFeatureName
Fix/YourFixName
Doc/YourDocumentationUpdate
Refactor/YourRefactorName
```

Prefixes are **mandatory**, starting with a capital letter.

### Workflow Rules

- **Direct pushes to `main` and `preprod` are forbidden.**
- All new branches must be merged into **preprod** first.
- Once validated, `preprod` is merged into `main`.
- Every merge into `main` generates a **new release**.
- A minimum of **two reviews** is required to approve a PR.

### Commits

The project uses a custom commit convention enforced by the script:

```
tools/commit.sh
```

- Using the script is **strongly recommended**.
- Manual commits are allowed **only if they strictly follow the commit convention**.
- A PR containing invalid commit messages will be rejected.

### Pull Requests

- PR descriptions are free-form.
- A PR is rejected if the CI pipeline fails.
- Merges are done using **squash merge**, keeping only one final commit (others appear as comments).

---

## 7. Continuous Integration (Woodpecker CI)

Woodpecker pipelines run on various triggers (`push`, `pull_request`) depending on the workflow configuration.

The CI:

- builds the project,
- runs unit + functional tests,
- checks code quality,
- **deploys documentation on release** (merge into `main`).

If a pipeline fails:

> Fix your code and push again.

---

## 8. Documentation

Documentation is stored in:

```
doc/
```

Format: **Markdown**

You may contribute by:

- fixing typos,
- improving explanations,
- adding examples or clarifications.

---

## 9. Reporting Bugs & Suggesting Features

Please use GitHub issues.

For bugs:

- clear description,
- reproduction steps,
- input file (if relevant),
- logs.

For features:

- explain the need,
- why it is useful,
- how it fits inside Glados.

---

## 10. Expected Behavior

- Be respectful and constructive.
- Review your code before submitting.
- Never force-push to critical branches.
- Never break the `preprod` build.

---

## 11. Conclusion

Thank you for contributing to Glados!
If you have questions, open an issue or contact a maintainer.
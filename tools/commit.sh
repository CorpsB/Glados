#!/usr/bin/env bash
##
## EPITECH PROJECT, 2025
## Glados
## File description:
## commit
##

set -e

BOLD="$(tput bold 2>/dev/null || true)"; RESET="$(tput sgr0 2>/dev/null || true)"

echo "Select your Gitmoji:"
echo
echo "${BOLD}=== Code-related commits ===${RESET}"
echo " 1) 🏗️  Architecture  – Structural changes to modules/project"
echo " 2) ⚡   Performance   – Improve speed or efficiency"
echo " 3) 🔥  Cleanup       – Remove code or files"
echo " 4) 🐛  Fix           – Fix one or more bugs"
echo " 5) ✨  Feature       – Add a new feature"
echo " 6) 🚧  WIP           – Incomplete feature / work in progress"
echo " 7) ♻️  Refactor      – Improve code without changing behavior"
echo " 8) 🚚  Move/Rename   – Move or rename files/folders"
echo
echo "${BOLD}=== CI/CD-related commits ===${RESET}"
echo " 9) 👷  CI/CD         – Update workflows/pipelines (GitHub Actions, runners, config)"
echo "10) ✅  Tests (pass)  – Add or update tests that pass"
echo "11) 🧪  Tests (fail)  – Add failing test(s) to reproduce a bug or for TDD"
echo
echo "${BOLD}=== Repository maintenance commits ===${RESET}"
echo "12) 📝  Documentation – Add or update documentation"
echo "13) ➖  Dependencies  – Remove a dependency"
echo "14) ➕  Dependencies  – Add a dependency"
echo "15) 🔨  Scripts       – Update project scripts (build/dev tooling)"
echo "16) 🔀  Merge         – Merge a branch or pull request"
echo "17) 📄  License       – Add or update license files"
echo

while true; do
  read -r -p "Select an option [1-17]: " CHOICE
  [[ "$CHOICE" =~ ^([1-9]|1[0-7])$ ]] && break
  echo "Invalid choice. Please enter a number between 1 and 17."
done

case "$CHOICE" in
  1)  EMOJI="🏗️" ;;
  2)  EMOJI="⚡"  ;;
  3)  EMOJI="🔥"  ;;
  4)  EMOJI="🐛" ;;
  5)  EMOJI="✨" ;;
  6)  EMOJI="🚧" ;;
  7)  EMOJI="♻️" ;;
  8)  EMOJI="🚚" ;;
  9)  EMOJI="👷" ;;
  10) EMOJI="✅" ;;
  11) EMOJI="🧪" ;;
  12) EMOJI="📝" ;;
  13) EMOJI="➖" ;;
  14) EMOJI="➕" ;;
  15) EMOJI="🔨" ;;
  16) EMOJI="🔀" ;;
  17) EMOJI="📄" ;;
esac

read -r -p "Short description: " SUBJECT
SUBJECT="$(printf '%s' "$SUBJECT" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"

MSG="${EMOJI} ${SUBJECT}"
echo
echo "Commit message:"
echo "${BOLD}${MSG}${RESET}"
read -r -p "Proceed with commit? [Y/n]: " CONFIRM
if [[ -z "$CONFIRM" || "$CONFIRM" =~ ^[Yy]$ ]]; then
  git commit -m "$MSG"
  echo "Committed."
else
  echo "Aborted."
fi

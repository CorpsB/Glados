#!/usr/bin/env bash

MIN_COV=80

COV=$(stack test --coverage --no-terminal 2>/dev/null \
      | grep 'expressions used' \
      | tail -n1 \
      | awk '{print $1}' \
      | tr -d '%')

echo "Coverage = ${COV}%"

if [ -z "$COV" ]; then
  echo "Error: could not determine coverage."
  exit 1
fi

if [ "$COV" -lt "$MIN_COV" ]; then
  echo "Coverage too low: ${COV}% (required: ${MIN_COV}%)"
  exit 1
else
  echo "Coverage OK: ${COV}%"
  exit 0
fi

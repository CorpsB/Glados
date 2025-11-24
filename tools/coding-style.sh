#!/usr/bin/env bash
##
## EPITECH PROJECT, 2025
## Glados
## File description:
## coding-style
##

check.sh "$(pwd)" "$(pwd)"

error=false

if test -s coding-style-reports.log; then
    echo "Coding-style-report.log found ! Searching exceptions ..."

    while IFS= read -r line; do

        if [[ "$line" == *"./app/Main.hs"* ]]; then
            continue
        elif [[ "$line" == *"./test/Unit/"* ]]; then
            continue
        else 
            echo "$line"
            error=true
        fi

    done < "coding-style-reports.log"

    if [[ "$error" == true ]]; then
        exit 1
    else
        echo "All errors was ignored. Good work !"
        exit 0
    fi

else
    exit 0
fi
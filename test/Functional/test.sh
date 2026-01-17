#!/usr/bin/env bash

# declare -A test1=( [titre]="Basic : " [fichier]=".scm" [exitcode]="0" [output]="")

# Basic
declare -A test1=( [titre]="Basic : foo" [fichier]="test/Functional/Basic/foo.scm" [exitcode]="0" [output]="42")


# Test func
RED="\e[31m"
GREEN="\e[32m"
YELLOW="\e[33m"
BLUE="\e[34m"
BOLD="\e[1m"
RESET="\e[0m"

run_test() {
    local test_name="$1"
    declare -n test="$test_name"

    local titre="${test[titre]}"
    local fichier="${test[fichier]}"
    local expected_code="${test[exitcode]}"
    local expected_output="${test[output]}"

    output=$(./glados < "$fichier" 2>/dev/null)
    ret=$?
    output=${output%$'\n'}

    local has_error=0
    local err_msg=""

    if [[ "$ret" -ne "$expected_code" ]]; then
        has_error=1
        err_msg+="Exit code attendu : $expected_code\n"
        err_msg+="Exit code obtenu : $ret\n"
    fi

    if [[ -n "$expected_output" ]]; then
        if [[ "$output" != "$expected_output" ]]; then
            has_error=1
            err_msg+="Output attendu : '$expected_output'\n"
            err_msg+="Output obtenu  : '$output'\n"
        fi
    fi

    if [[ $has_error -eq 0 ]]; then
        echo -e "[${GREEN}OK${RESET}] ${titre}"
        return 0 
    else
        echo -e "[${RED}KO${RESET}] ${titre}"
        echo -e "${YELLOW}----------------------------------------${RESET}"
        echo -e "$err_msg" | sed 's/^/    /'
        echo -e "${YELLOW}----------------------------------------${RESET}"
        return 1
    fi
}

run_all_tests() {
local tests=(

)



    local total=${#tests[@]}
    local passed=0
    local failed=0

    for t in "${tests[@]}"; do
        if run_test "$t"; then
            ((passed++))
        else
            failed=1
        fi
    done

    echo
    echo "Résultat : $passed / $total tests OK"

    return $failed
}

run_all_tests
exit $?
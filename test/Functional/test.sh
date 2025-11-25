#!/usr/bin/env bash

# declare -A test1=( [titre]="Basic : " [fichier]=".scm" [exitcode]="0" [output]="")

# Basic
declare -A test1=( [titre]="Basic : foo" [fichier]="test/Functional/Basic/foo.scm" [exitcode]="0" [output]="42")
declare -A test2=( [titre]="Basic : error" [fichier]="test/Functional/Basic/error.scm" [exitcode]="84" [output]="")
declare -A test3=( [titre]="Basic : call" [fichier]="test/Functional/Basic/call.scm" [exitcode]="0" [output]="5")
declare -A test4=( [titre]="Basic : lambda1" [fichier]="test/Functional/Basic/lambda1.scm" [exitcode]="0" [output]="#<\procedure\>")
declare -A test5=( [titre]="Basic : lambda2" [fichier]="test/Functional/Basic/lambda2.scm" [exitcode]="0" [output]="3")
declare -A test6=( [titre]="Basic : lambda3" [fichier]="test/Functional/Basic/lambda3.scm" [exitcode]="0" [output]="7")
declare -A test7=( [titre]="Basic : fucntion" [fichier]="test/Functional/Basic/function1.scm" [exitcode]="0" [output]="7")
declare -A test8=( [titre]="Basic : if1" [fichier]="test/Functional/Basic/if1.scm" [exitcode]="0" [output]="1")
declare -A test9=( [titre]="Basic : if2" [fichier]="test/Functional/Basic/if2.scm" [exitcode]="0" [output]="2")
declare -A test10=( [titre]="Basic : if3" [fichier]="test/Functional/Basic/if3.scm" [exitcode]="0" [output]="21")
declare -A test11=( [titre]="Basic : builtins1" [fichier]="test/Functional/Basic/builtins1.scm" [exitcode]="0" [output]="11")
declare -A test12=( [titre]="Basic : builtins2" [fichier]="test/Functional/Basic/builtins2.scm" [exitcode]="0" [output]="#t")
declare -A test13=( [titre]="Basic : builtins3" [fichier]="test/Functional/Basic/builtins3.scm" [exitcode]="0" [output]="#f")
declare -A test14=( [titre]="Basic : superior" [fichier]="test/Functional/Basic/superior.scm" [exitcode]="0" [output]="#t")
declare -A test15=( [titre]="Basic : factorial" [fichier]="test/Functional/Basic/factorial.scm" [exitcode]="0" [output]="3628800")


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

    output=$(./glados < "$fichier")
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
        test1 test2 test3 test4 test5
        test6 test7 test8 test9 test10
        test11 test12 test13 test14 test15
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

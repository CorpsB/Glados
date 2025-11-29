#!/usr/bin/env bash

# declare -A test1=( [titre]="Basic : " [fichier]=".scm" [exitcode]="0" [output]="")

# Basic
declare -A test1=( [titre]="Basic : foo" [fichier]="test/Functional/Basic/foo.scm" [exitcode]="0" [output]="42")
declare -A test2=( [titre]="Basic : error" [fichier]="test/Functional/Basic/error.scm" [exitcode]="84" [output]="")
declare -A test3=( [titre]="Basic : call" [fichier]="test/Functional/Basic/call.scm" [exitcode]="0" [output]="5")
declare -A test4=( [titre]="Basic : lambda1" [fichier]="test/Functional/Basic/lambda1.scm" [exitcode]="0" [output]="#\<procedure\>")
declare -A test5=( [titre]="Basic : lambda2" [fichier]="test/Functional/Basic/lambda2.scm" [exitcode]="0" [output]="3")
declare -A test6=( [titre]="Basic : lambda3" [fichier]="test/Functional/Basic/lambda3.scm" [exitcode]="0" [output]="7")
declare -A test7=( [titre]="Basic : function" [fichier]="test/Functional/Basic/function1.scm" [exitcode]="0" [output]="7")
declare -A test8=( [titre]="Basic : if1" [fichier]="test/Functional/Basic/if1.scm" [exitcode]="0" [output]="1")
declare -A test9=( [titre]="Basic : if2" [fichier]="test/Functional/Basic/if2.scm" [exitcode]="0" [output]="2")
declare -A test10=( [titre]="Basic : if3" [fichier]="test/Functional/Basic/if3.scm" [exitcode]="0" [output]="21")
declare -A test11=( [titre]="Basic : builtins1" [fichier]="test/Functional/Basic/builtins1.scm" [exitcode]="0" [output]="11")
declare -A test12=( [titre]="Basic : builtins2" [fichier]="test/Functional/Basic/builtins2.scm" [exitcode]="0" [output]="#t")
declare -A test13=( [titre]="Basic : builtins3" [fichier]="test/Functional/Basic/builtins3.scm" [exitcode]="0" [output]="#f")
declare -A test14=( [titre]="Basic : superior" [fichier]="test/Functional/Basic/superior.scm" [exitcode]="0" [output]="#t")
declare -A test15=( [titre]="Basic : factorial" [fichier]="test/Functional/Basic/factorial.scm" [exitcode]="0" [output]="3628800")

# Simple : Parsing
declare -A test16=( [titre]="Simple : Bad file extension" [fichier]="test/Functional/Simple/bad_extension.txt" [exitcode]="0" [output]="")
declare -A test17=( [titre]="Simple : Empty file" [fichier]="test/Functional/Simple/empty_file.scm" [exitcode]="0" [output]="")
declare -A test18=( [titre]="Simple : No extension file" [fichier]="test/Functional/Simple/no_extention" [exitcode]="0" [output]="")
declare -A test19=( [titre]="Simple : Not enough parentheses" [fichier]="test/Functional/Simple/error1.scm" [exitcode]="84" [output]="")
declare -A test20=( [titre]="Simple : Too many parentheses" [fichier]="test/Functional/Simple/error2.scm" [exitcode]="84" [output]="")
declare -A test21=( [titre]="Simple : Bad file" [fichier]="test/Functional/Simple/error3.scm" [exitcode]="84" [output]="")
declare -A test22=( [titre]="Simple : Just space" [fichier]="test/Functional/Simple/error4.scm" [exitcode]="0" [output]="")
declare -A test23=( [titre]="Simple : Just multi-space" [fichier]="test/Functional/Simple/error5.scm" [exitcode]="0" [output]="")
declare -A test24=( [titre]="Simple : Just newline" [fichier]="test/Functional/Simple/error6.scm" [exitcode]="0" [output]="")
declare -A test27=( [titre]="Simple : Only closing parenthesis" [fichier]="test/Functional/Simple/only_closing_paren.scm" [exitcode]="84" [output]="")
declare -A test28=( [titre]="Simple : Truncated expression" [fichier]="test/Functional/Simple/truncated_expr.scm" [exitcode]="84" [output]="")

# Simple : Builtin eq?
declare -A test29=( [titre]="Simple : Simple true" [fichier]="test/Functional/Simple/bu_eq1.scm" [exitcode]="0" [output]="#t")
declare -A test30=( [titre]="Simple : Simple negative true" [fichier]="test/Functional/Simple/bu_eq2.scm" [exitcode]="0" [output]="#t")
declare -A test31=( [titre]="Simple : Simple false" [fichier]="test/Functional/Simple/bu_eq3.scm" [exitcode]="0" [output]="#f")
declare -A test32=( [titre]="Simple : Compare positive and negative integers" [fichier]="test/Functional/Simple/bu_eq4.scm" [exitcode]="0" [output]="#f")
declare -A test33=( [titre]="Simple : Compare two negative zero numbers" [fichier]="test/Functional/Simple/bu_eq5.scm" [exitcode]="0" [output]="#t")
declare -A test34=( [titre]="Simple : Compare zero and negative zero" [fichier]="test/Functional/Simple/bu_eq6.scm" [exitcode]="0" [output]="#t")
declare -A test35=( [titre]="Simple : eq? #t #t" [fichier]="test/Functional/Simple/bu_eq_bool1.scm" [exitcode]="0" [output]="#t")
declare -A test36=( [titre]="Simple : eq? #t #f" [fichier]="test/Functional/Simple/bu_eq_bool2.scm" [exitcode]="0" [output]="#f")
declare -A test37=( [titre]="Simple : eq? bool expr" [fichier]="test/Functional/Simple/bu_eq_bool3.scm" [exitcode]="0" [output]="#t")
declare -A test38=( [titre]="Simple : eq? deux expressions arithmétiques" [fichier]="test/Functional/Simple/bu_eq_expr1.scm" [exitcode]="0" [output]="#t")
declare -A test39=( [titre]="Simple : eq? sans arguments" [fichier]="test/Functional/Simple/bu_eq_arity0.scm" [exitcode]="84" [output]="")
declare -A test40=( [titre]="Simple : eq? un seul argument" [fichier]="test/Functional/Simple/bu_eq_arity1.scm" [exitcode]="84" [output]="")
declare -A test41=( [titre]="Simple : eq? trop d'arguments" [fichier]="test/Functional/Simple/bu_eq_arity3.scm" [exitcode]="84" [output]="")

# Simple : Builtin <
declare -A test42=( [titre]="Simple : Simple true case" [fichier]="test/Functional/Simple/bu_<1.scm" [exitcode]="0" [output]="#t")
declare -A test43=( [titre]="Simple : Simple false case" [fichier]="test/Functional/Simple/bu_<2.scm" [exitcode]="0" [output]="#f")
declare -A test44=( [titre]="Simple : Negative integer" [fichier]="test/Functional/Simple/bu_<3.scm" [exitcode]="0" [output]="#t")
declare -A test45=( [titre]="Simple : String and int" [fichier]="test/Functional/Simple/bu_<4.scm" [exitcode]="84" [output]="")
declare -A test46=( [titre]="Simple : Same numbers" [fichier]="test/Functional/Simple/bu_<5.scm" [exitcode]="0" [output]="#f")
declare -A test47=( [titre]="Simple : Bool and int" [fichier]="test/Functional/Simple/bu_<6.scm" [exitcode]="84" [output]="")
declare -A test48=( [titre]="Simple : < arity 0" [fichier]="test/Functional/Simple/bu_<arity0.scm" [exitcode]="84" [output]="")
declare -A test49=( [titre]="Simple : < arity 1" [fichier]="test/Functional/Simple/bu_<arity1.scm" [exitcode]="84" [output]="")
declare -A test50=( [titre]="Simple : < arity 3" [fichier]="test/Functional/Simple/bu_<arity3.scm" [exitcode]="84" [output]="")
declare -A test51=( [titre]="Simple : < with arithmetic expressions" [fichier]="test/Functional/Simple/bu_<expr1.scm" [exitcode]="0" [output]="#t")
declare -A test52=( [titre]="Simple : < expr false" [fichier]="test/Functional/Simple/bu_<expr2.scm" [exitcode]="0" [output]="#f")
declare -A test53=( [titre]="Simple : Bool and bool in <" [fichier]="test/Functional/Simple/bu_<boolbool.scm" [exitcode]="84" [output]="")


# Simple : Builtin +
declare -A test54=( [titre]="Simple : Simple addition" [fichier]="test/Functional/Simple/bu_+1.scm" [exitcode]="0" [output]="2")
declare -A test55=( [titre]="Simple : Addition with negative integer" [fichier]="test/Functional/Simple/bu_+2.scm" [exitcode]="0" [output]="1")
declare -A test56=( [titre]="Simple : Addition of two negative numbers" [fichier]="test/Functional/Simple/bu_+3.scm" [exitcode]="0" [output]="-4")
declare -A test57=( [titre]="Simple : Add boolean with integer" [fichier]="test/Functional/Simple/bu_+4.scm" [exitcode]="84" [output]="")
declare -A test58=( [titre]="Simple : Add string with integer" [fichier]="test/Functional/Simple/bu_+5.scm" [exitcode]="84" [output]="")
declare -A test59=( [titre]="Simple : Add string with string" [fichier]="test/Functional/Simple/bu_+6.scm" [exitcode]="84" [output]="")
declare -A test60=( [titre]="Simple : Addition with 0 arguments" [fichier]="test/Functional/Simple/bu_+arity0.scm" [exitcode]="84" [output]="")
declare -A test61=( [titre]="Simple : Addition with 1 argument" [fichier]="test/Functional/Simple/bu_+arity1.scm" [exitcode]="84" [output]="")
declare -A test62=( [titre]="Simple : Addition with 3 arguments" [fichier]="test/Functional/Simple/bu_+arity3.scm" [exitcode]="84" [output]="")
declare -A test63=( [titre]="Simple : Addition with arithmetic expressions" [fichier]="test/Functional/Simple/bu_+expr1.scm" [exitcode]="0" [output]="9")
declare -A test64=( [titre]="Simple : Add boolean with boolean" [fichier]="test/Functional/Simple/bu_+boolbool.scm" [exitcode]="84" [output]="")

# Simple : Builtin -
declare -A test65=( [titre]="Simple : Simple subtract" [fichier]="test/Functional/Simple/bu_-1.scm" [exitcode]="0" [output]="0")
declare -A test66=( [titre]="Simple : Subtract with negative integer" [fichier]="test/Functional/Simple/bu_-2.scm" [exitcode]="0" [output]="-3")
declare -A test67=( [titre]="Simple : Subtract two negative numbers" [fichier]="test/Functional/Simple/bu_-3.scm" [exitcode]="0" [output]="0")
declare -A test68=( [titre]="Simple : Subtract boolean with integer" [fichier]="test/Functional/Simple/bu_-4.scm" [exitcode]="84" [output]="")
declare -A test69=( [titre]="Simple : Subtract string with integer" [fichier]="test/Functional/Simple/bu_-5.scm" [exitcode]="84" [output]="")
declare -A test70=( [titre]="Simple : Subtract string with string" [fichier]="test/Functional/Simple/bu_-6.scm" [exitcode]="84" [output]="")
declare -A test71=( [titre]="Simple : Subtract with 0 arguments" [fichier]="test/Functional/Simple/bu_-arity0.scm" [exitcode]="84" [output]="")
declare -A test72=( [titre]="Simple : Subtract with 1 argument" [fichier]="test/Functional/Simple/bu_-arity1.scm" [exitcode]="84" [output]="")
declare -A test73=( [titre]="Simple : Subtract with 3 arguments" [fichier]="test/Functional/Simple/bu_-arity3.scm" [exitcode]="84" [output]="")
declare -A test74=( [titre]="Simple : Subtract with arithmetic expressions" [fichier]="test/Functional/Simple/bu_-expr1.scm" [exitcode]="0" [output]="0")
declare -A test75=( [titre]="Simple : Subtract boolean with boolean" [fichier]="test/Functional/Simple/bu_-boolbool.scm" [exitcode]="84" [output]="")

# Simple : Builtin *
declare -A test76=( [titre]="Simple : Simple multiplication" [fichier]="test/Functional/Simple/bu_x1.scm" [exitcode]="0" [output]="2")
declare -A test77=( [titre]="Simple : Multiply with negative integer" [fichier]="test/Functional/Simple/bu_x2.scm" [exitcode]="0" [output]="1")
declare -A test78=( [titre]="Simple : Multiply two negative numbers" [fichier]="test/Functional/Simple/bu_x3.scm" [exitcode]="0" [output]="-4")
declare -A test79=( [titre]="Simple : Multiply boolean with integer" [fichier]="test/Functional/Simple/bu_x4.scm" [exitcode]="84" [output]="")
declare -A test80=( [titre]="Simple : Multiply string with integer" [fichier]="test/Functional/Simple/bu_x5.scm" [exitcode]="84" [output]="")
declare -A test81=( [titre]="Simple : Multiply string with string" [fichier]="test/Functional/Simple/bu_x6.scm" [exitcode]="84" [output]="")
declare -A test82=( [titre]="Simple : Multiply with 0 arguments" [fichier]="test/Functional/Simple/bu_xarity0.scm" [exitcode]="84" [output]="")
declare -A test83=( [titre]="Simple : Multiply with 1 argument" [fichier]="test/Functional/Simple/bu_xarity1.scm" [exitcode]="84" [output]="")
declare -A test84=( [titre]="Simple : Multiply with 3 arguments" [fichier]="test/Functional/Simple/bu_xarity3.scm" [exitcode]="84" [output]="")
declare -A test85=( [titre]="Simple : Multiply with arithmetic expressions" [fichier]="test/Functional/Simple/bu_xexpr1.scm" [exitcode]="0" [output]="9")
declare -A test86=( [titre]="Simple : Multiply boolean with boolean" [fichier]="test/Functional/Simple/bu_xboolbool.scm" [exitcode]="84" [output]="")

# Simple : Builtin /
declare -A test87=( [titre]="Simple : Simple division" [fichier]="test/Functional/Simple/bu_:1.scm" [exitcode]="0" [output]="2")
declare -A test88=( [titre]="Simple : Division with negative integer" [fichier]="test/Functional/Simple/bu_:2.scm" [exitcode]="0" [output]="1")
declare -A test89=( [titre]="Simple : Division of two negative numbers" [fichier]="test/Functional/Simple/bu_:3.scm" [exitcode]="0" [output]="-4")
declare -A test90=( [titre]="Simple : Division of boolean with integer" [fichier]="test/Functional/Simple/bu_:4.scm" [exitcode]="84" [output]="")
declare -A test91=( [titre]="Simple : Division of string with integer" [fichier]="test/Functional/Simple/bu_:5.scm" [exitcode]="84" [output]="")
declare -A test92=( [titre]="Simple : Division of string with string" [fichier]="test/Functional/Simple/bu_:6.scm" [exitcode]="84" [output]="")
declare -A test93=( [titre]="Simple : Division by zero" [fichier]="test/Functional/Simple/bu_:7.scm" [exitcode]="84" [output]="")
declare -A test94=( [titre]="Simple : Division with 0 arguments" [fichier]="test/Functional/Simple/bu_:arity0.scm" [exitcode]="84" [output]="")
declare -A test95=( [titre]="Simple : Division with 1 argument" [fichier]="test/Functional/Simple/bu_:arity1.scm" [exitcode]="84" [output]="")
declare -A test96=( [titre]="Simple : Division with 3 arguments" [fichier]="test/Functional/Simple/bu_:arity3.scm" [exitcode]="84" [output]="")
declare -A test97=( [titre]="Simple : Division with arithmetic expressions" [fichier]="test/Functional/Simple/bu_:expr1.scm" [exitcode]="0" [output]="2")
declare -A test98=( [titre]="Simple : Division of boolean with boolean" [fichier]="test/Functional/Simple/bu_:boolbool.scm" [exitcode]="84" [output]="")




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
    test1 test2 test3 test4 test5
    test6 test7 test8 test9 test10
    test11 test12 test13 test14 test15

    test16 test17 test18 test19 test20
    test21 test22 test23 test24 test25
    test26 test27 test28

    test29 test30 test31 test32 test33
    test34 test35 test36 test37 test38
    test39 test40 test41

    test42 test43 test44 test45 test46
    test47 test48 test49 test50 test51
    test52 test53

    test54 test55 test56 test57 test58
    test59 test60 test61 test62 test63
    test64

    test65 test66 test67 test68 test69
    test70 test71 test72 test73 test74
    test75

    test76 test77 test78 test79 test80
    test81 test82 test83 test84 test85
    test86

    test87 test88 test89 test90 test91
    test92 test93 test94 test95 test96
    test97 test98
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

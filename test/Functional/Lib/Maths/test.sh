#!/usr/bin/env bash

declare -A test_lib_maths_module_import_ok=([titre]="Lib Maths : module import ok" [fichier]="test/Functional/Lib/Maths/module/import_ok.npy" [exitcode]="0" [output]="42")

# basic.npy
declare -A test_lib_maths_basic_abs_ok=([titre]="Lib Maths : basic.abs(-42) == 42" [fichier]="test/Functional/Lib/Maths/basic/abs_ok.npy" [exitcode]="0" [output]="42")
declare -A test_lib_maths_basic_sign_neg_ok=([titre]="Lib Maths : basic.sign(-5) == -1" [fichier]="test/Functional/Lib/Maths/basic/sign_neg_ok.npy" [exitcode]="0" [output]="-1")
declare -A test_lib_maths_basic_min_ok=([titre]="Lib Maths : basic.min(3,7) == 3" [fichier]="test/Functional/Lib/Maths/basic/min_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lib_maths_basic_max_ok=([titre]="Lib Maths : basic.max(3,7) == 7" [fichier]="test/Functional/Lib/Maths/basic/max_ok.npy" [exitcode]="0" [output]="7")
declare -A test_lib_maths_basic_clamp_ok=([titre]="Lib Maths : basic.clamp(10,0,5) == 5" [fichier]="test/Functional/Lib/Maths/basic/clamp_ok.npy" [exitcode]="0" [output]="5")
declare -A test_lib_maths_basic_is_even_ok=([titre]="Lib Maths : basic.isEven(42) == True" [fichier]="test/Functional/Lib/Maths/basic/is_even_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_basic_is_odd_ok=([titre]="Lib Maths : basic.isOdd(42) == False" [fichier]="test/Functional/Lib/Maths/basic/is_odd_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_maths_basic_modpos_neg_ok=([titre]="Lib Maths : basic.modPos(-1,5) == 4" [fichier]="test/Functional/Lib/Maths/basic/modpos_neg_ok.npy" [exitcode]="0" [output]="4")
declare -A test_lib_maths_basic_modpos_bad_mod_ok=([titre]="Lib Maths : basic.modPos(10,0) == 0" [fichier]="test/Functional/Lib/Maths/basic/modpos_bad_mod_ok.npy" [exitcode]="0" [output]="0")

# combin.npy
declare -A test_lib_maths_combin_fact_ok=([titre]="Lib Maths : combin.fact(5) == 120" [fichier]="test/Functional/Lib/Maths/combin/fact_ok.npy" [exitcode]="0" [output]="120")
declare -A test_lib_maths_combin_fact_neg_ok=([titre]="Lib Maths : combin.fact(-1) == 0" [fichier]="test/Functional/Lib/Maths/combin/fact_neg_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_combin_perm_ok=([titre]="Lib Maths : combin.perm(5,2) == 20" [fichier]="test/Functional/Lib/Maths/combin/perm_ok.npy" [exitcode]="0" [output]="20")
declare -A test_lib_maths_combin_binom_ok=([titre]="Lib Maths : combin.binom(5,2) == 10" [fichier]="test/Functional/Lib/Maths/combin/binom_ok.npy" [exitcode]="0" [output]="10")

# div.npy
declare -A test_lib_maths_div_trunc_ok=([titre]="Lib Maths : div.divTrunc(7,2) == 3" [fichier]="test/Functional/Lib/Maths/div/divtrunc_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lib_maths_div_trunc_div0_ok=([titre]="Lib Maths : div.divTrunc(7,0) == 0" [fichier]="test/Functional/Lib/Maths/div/divtrunc_div0_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_div_ceil_ok=([titre]="Lib Maths : div.divCeil(7,2) == 4" [fichier]="test/Functional/Lib/Maths/div/divceil_ok.npy" [exitcode]="0" [output]="4")

# gcd.npy
declare -A test_lib_maths_gcd_ok=([titre]="Lib Maths : gcd.gcd(48,18) == 6" [fichier]="test/Functional/Lib/Maths/gcd/gcd_ok.npy" [exitcode]="0" [output]="6")
declare -A test_lib_maths_lcm_ok=([titre]="Lib Maths : gcd.lcm(48,18) == 144" [fichier]="test/Functional/Lib/Maths/gcd/lcm_ok.npy" [exitcode]="0" [output]="144")
declare -A test_lib_maths_coprime_ok=([titre]="Lib Maths : gcd.coPrime(35,64) == True" [fichier]="test/Functional/Lib/Maths/gcd/coprime_ok.npy" [exitcode]="0" [output]="True")

# ntheory.npy
declare -A test_lib_maths_phi_ok=([titre]="Lib Maths : ntheory.phi(9) == 6" [fichier]="test/Functional/Lib/Maths/ntheory/phi_ok.npy" [exitcode]="0" [output]="6")
declare -A test_lib_maths_modinv_ok=([titre]="Lib Maths : ntheory.modinv(3,11) == 4" [fichier]="test/Functional/Lib/Maths/ntheory/modinv_ok.npy" [exitcode]="0" [output]="4")

# pow2.npy
declare -A test_lib_maths_ispow2_true_ok=([titre]="Lib Maths : pow2.isPow2(8) == True" [fichier]="test/Functional/Lib/Maths/pow2/ispow2_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_ispow2_false_ok=([titre]="Lib Maths : pow2.isPow2(6) == False" [fichier]="test/Functional/Lib/Maths/pow2/ispow2_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_maths_nextpow2_ok=([titre]="Lib Maths : pow2.nextPow2(5) == 8" [fichier]="test/Functional/Lib/Maths/pow2/nextpow2_ok.npy" [exitcode]="0" [output]="8")

# power.npy
declare -A test_lib_maths_pow_ok=([titre]="Lib Maths : power.pow(2,10) == 1024" [fichier]="test/Functional/Lib/Maths/power/pow_ok.npy" [exitcode]="0" [output]="1024")
declare -A test_lib_maths_pow10_ok=([titre]="Lib Maths : power.pow10(3) == 1000" [fichier]="test/Functional/Lib/Maths/power/pow10_ok.npy" [exitcode]="0" [output]="1000")
declare -A test_lib_maths_modpow_ok=([titre]="Lib Maths : power.modPow(2,10,1000) == 24" [fichier]="test/Functional/Lib/Maths/power/modpow_ok.npy" [exitcode]="0" [output]="24")

# prime.npy
declare -A test_lib_maths_isprime_true_ok=([titre]="Lib Maths : prime.isPrime(29) == True" [fichier]="test/Functional/Lib/Maths/prime/isprime_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_isprime_false_ok=([titre]="Lib Maths : prime.isPrime(1) == False" [fichier]="test/Functional/Lib/Maths/prime/isprime_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_maths_nextprime_ok=([titre]="Lib Maths : prime.nextPrime(14) == 17" [fichier]="test/Functional/Lib/Maths/prime/nextprime_ok.npy" [exitcode]="0" [output]="17")

# range.npy
declare -A test_lib_maths_sumrange_ok=([titre]="Lib Maths : range.sumRange(1,5) == 15" [fichier]="test/Functional/Lib/Maths/range/sumrange_ok.npy" [exitcode]="0" [output]="15")
declare -A test_lib_maths_prodrange_ok=([titre]="Lib Maths : range.prodRange(1,5) == 120" [fichier]="test/Functional/Lib/Maths/range/prodrange_ok.npy" [exitcode]="0" [output]="120")

# sqrt.npy
declare -A test_lib_maths_issqrt_ok=([titre]="Lib Maths : sqrt.isSqrt(81) == 9" [fichier]="test/Functional/Lib/Maths/sqrt/issqrt_ok.npy" [exitcode]="0" [output]="9")
declare -A test_lib_maths_issquare_true_ok=([titre]="Lib Maths : sqrt.isSquare(81) == True" [fichier]="test/Functional/Lib/Maths/sqrt/issquare_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_issquare_false_ok=([titre]="Lib Maths : sqrt.isSquare(80) == False" [fichier]="test/Functional/Lib/Maths/sqrt/issquare_false_ok.npy" [exitcode]="0" [output]="False")

# basic/edge
declare -A test_lib_maths_basic_abs_zero_ok=([titre]="Lib Maths Edge : basic.abs(0) == 0" [fichier]="test/Functional/Lib/Maths/basic/edge/abs_zero_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_basic_abs_pos_ok=([titre]="Lib Maths Edge : basic.abs(7) == 7" [fichier]="test/Functional/Lib/Maths/basic/edge/abs_pos_ok.npy" [exitcode]="0" [output]="7")
declare -A test_lib_maths_basic_sign_zero_ok=([titre]="Lib Maths Edge : basic.sign(0) == 0" [fichier]="test/Functional/Lib/Maths/basic/edge/sign_zero_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_basic_sign_pos_ok=([titre]="Lib Maths Edge : basic.sign(9) == 1" [fichier]="test/Functional/Lib/Maths/basic/edge/sign_pos_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_basic_clamp_inside_ok=([titre]="Lib Maths Edge : basic.clamp(3,0,5) == 3" [fichier]="test/Functional/Lib/Maths/basic/edge/clamp_inside_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lib_maths_basic_clamp_lo_ok=([titre]="Lib Maths Edge : basic.clamp(0,0,5) == 0" [fichier]="test/Functional/Lib/Maths/basic/edge/clamp_lo_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_basic_clamp_hi_ok=([titre]="Lib Maths Edge : basic.clamp(5,0,5) == 5" [fichier]="test/Functional/Lib/Maths/basic/edge/clamp_hi_ok.npy" [exitcode]="0" [output]="5")
declare -A test_lib_maths_basic_modpos_pos_ok=([titre]="Lib Maths Edge : basic.modPos(6,5) == 1" [fichier]="test/Functional/Lib/Maths/basic/edge/modpos_pos_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_basic_modpos_m1_ok=([titre]="Lib Maths Edge : basic.modPos(123,1) == 0" [fichier]="test/Functional/Lib/Maths/basic/edge/modpos_m1_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_basic_modpos_mneg_ok=([titre]="Lib Maths Edge : basic.modPos(10,-3) == 0 (m<=0 => 0)" [fichier]="test/Functional/Lib/Maths/basic/edge/modpos_mneg_ok.npy" [exitcode]="0" [output]="0")

# combin/edge
declare -A test_lib_maths_combin_fact_zero_ok=([titre]="Lib Maths Edge : combin.fact(0) == 1" [fichier]="test/Functional/Lib/Maths/combin/edge/fact_zero_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_combin_perm_k0_ok=([titre]="Lib Maths Edge : combin.perm(5,0) == 1" [fichier]="test/Functional/Lib/Maths/combin/edge/perm_k0_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_combin_perm_invalid_ok=([titre]="Lib Maths Edge : combin.perm(5,6) == 0 (invalid => 0)" [fichier]="test/Functional/Lib/Maths/combin/edge/perm_invalid_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_combin_binom_k0_ok=([titre]="Lib Maths Edge : combin.binom(5,0) == 1" [fichier]="test/Functional/Lib/Maths/combin/edge/binom_k0_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_combin_binom_kn_ok=([titre]="Lib Maths Edge : combin.binom(5,5) == 1" [fichier]="test/Functional/Lib/Maths/combin/edge/binom_kn_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_combin_binom_sym_ok=([titre]="Lib Maths Edge : combin.binom(6,4) == 15 (symmetry)" [fichier]="test/Functional/Lib/Maths/combin/edge/binom_sym_ok.npy" [exitcode]="0" [output]="15")

# div/edge
declare -A test_lib_maths_div_ceil_exact_ok=([titre]="Lib Maths Edge : div.divCeil(8,2) == 4" [fichier]="test/Functional/Lib/Maths/div/edge/divceil_exact_ok.npy" [exitcode]="0" [output]="4")

# gcd/edge
declare -A test_lib_maths_gcd_a0_ok=([titre]="Lib Maths Edge : gcd.gcd(12,0) == 12" [fichier]="test/Functional/Lib/Maths/gcd/edge/gcd_a0_ok.npy" [exitcode]="0" [output]="12")
declare -A test_lib_maths_gcd_0b_ok=([titre]="Lib Maths Edge : gcd.gcd(0,18) == 18" [fichier]="test/Functional/Lib/Maths/gcd/edge/gcd_0b_ok.npy" [exitcode]="0" [output]="18")
declare -A test_lib_maths_lcm_zero_ok=([titre]="Lib Maths Edge : gcd.lcm(12,0) == 0" [fichier]="test/Functional/Lib/Maths/gcd/edge/lcm_zero_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_coprime_false_ok=([titre]="Lib Maths Edge : gcd.coPrime(12,18) == False" [fichier]="test/Functional/Lib/Maths/gcd/edge/coprime_false_ok.npy" [exitcode]="0" [output]="False")

# ntheory/edge
declare -A test_lib_maths_phi_1_ok=([titre]="Lib Maths Edge : ntheory.phi(1) == 1" [fichier]="test/Functional/Lib/Maths/ntheory/edge/phi_1_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_phi_prime_ok=([titre]="Lib Maths Edge : ntheory.phi(13) == 12" [fichier]="test/Functional/Lib/Maths/ntheory/edge/phi_prime_ok.npy" [exitcode]="0" [output]="12")

# pow2/edge
declare -A test_lib_maths_ispow2_1_ok=([titre]="Lib Maths Edge : pow2.isPow2(1) == True" [fichier]="test/Functional/Lib/Maths/pow2/edge/ispow2_1_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_ispow2_0_ok=([titre]="Lib Maths Edge : pow2.isPow2(0) == False" [fichier]="test/Functional/Lib/Maths/pow2/edge/ispow2_0_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_maths_nextpow2_1_ok=([titre]="Lib Maths Edge : pow2.nextPow2(1) == 1" [fichier]="test/Functional/Lib/Maths/pow2/edge/nextpow2_1_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_nextpow2_pow2_ok=([titre]="Lib Maths Edge : pow2.nextPow2(8) == 8" [fichier]="test/Functional/Lib/Maths/pow2/edge/nextpow2_pow2_ok.npy" [exitcode]="0" [output]="8")

# power/edge
declare -A test_lib_maths_pow_exp0_ok=([titre]="Lib Maths Edge : power.pow(7,0) == 1" [fichier]="test/Functional/Lib/Maths/power/edge/pow_exp0_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_pow_zero_base_ok=([titre]="Lib Maths Edge : power.pow(0,5) == 0" [fichier]="test/Functional/Lib/Maths/power/edge/pow_zero_base_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_modpow_m1_ok=([titre]="Lib Maths Edge : power.modPow(123,45,1) == 0" [fichier]="test/Functional/Lib/Maths/power/edge/modpow_m1_ok.npy" [exitcode]="0" [output]="0")

# prime/edge
declare -A test_lib_maths_isprime_2_ok=([titre]="Lib Maths Edge : prime.isPrime(2) == True" [fichier]="test/Functional/Lib/Maths/prime/edge/isprime_2_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_isprime_0_ok=([titre]="Lib Maths Edge : prime.isPrime(0) == False" [fichier]="test/Functional/Lib/Maths/prime/edge/isprime_0_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_maths_nextprime_prime_ok=([titre]="Lib Maths Edge : prime.nextPrime(17) == 17" [fichier]="test/Functional/Lib/Maths/prime/edge/nextprime_prime_ok.npy" [exitcode]="0" [output]="17")

# range/edge
declare -A test_lib_maths_sumrange_single_ok=([titre]="Lib Maths Edge : range.sumRange(5,5) == 5" [fichier]="test/Functional/Lib/Maths/range/edge/sumrange_single_ok.npy" [exitcode]="0" [output]="5")
declare -A test_lib_maths_prodrange_single_ok=([titre]="Lib Maths Edge : range.prodRange(5,5) == 5" [fichier]="test/Functional/Lib/Maths/range/edge/prodrange_single_ok.npy" [exitcode]="0" [output]="5")

# sqrt/edge
declare -A test_lib_maths_issqrt_0_ok=([titre]="Lib Maths Edge : sqrt.isSqrt(0) == 0" [fichier]="test/Functional/Lib/Maths/sqrt/edge/issqrt_0_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_maths_issqrt_1_ok=([titre]="Lib Maths Edge : sqrt.isSqrt(1) == 1" [fichier]="test/Functional/Lib/Maths/sqrt/edge/issqrt_1_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_maths_issquare_0_ok=([titre]="Lib Maths Edge : sqrt.isSquare(0) == True" [fichier]="test/Functional/Lib/Maths/sqrt/edge/issquare_0_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_issquare_1_ok=([titre]="Lib Maths Edge : sqrt.isSquare(1) == True" [fichier]="test/Functional/Lib/Maths/sqrt/edge/issquare_1_ok.npy" [exitcode]="0" [output]="True")

# gcd/props
declare -A test_lib_maths_gcd_commutative_ok=([titre]="Lib Maths Props : gcd.gcd(a,b) == gcd.gcd(b,a)" [fichier]="test/Functional/Lib/Maths/gcd/props/gcd_commutative_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_maths_gcd_lcm_relation_ok=([titre]="Lib Maths Props : gcd*lcm == a*b (a,b>0)" [fichier]="test/Functional/Lib/Maths/gcd/props/gcd_lcm_relation_ok.npy" [exitcode]="0" [output]="True")

# power/props
declare -A test_lib_maths_power_modpow_range_ok=([titre]="Lib Maths Props : modPow in [0, m-1] (m>0)" [fichier]="test/Functional/Lib/Maths/power/props/modpow_range_ok.npy" [exitcode]="0" [output]="True")

# ntheory/props
declare -A test_lib_maths_ntheory_modinv_correct_ok=([titre]="Lib Maths Props : (a*modinv(a,m)) mod m == 1" [fichier]="test/Functional/Lib/Maths/ntheory/props/modinv_correct_ok.npy" [exitcode]="0" [output]="1")


RED="\e[31m"
GREEN="\e[32m"
YELLOW="\e[33m"
BLUE="\e[34m"
BOLD="\e[1m"
RESET="\e[0m"

SHOW_ONLY_KO=0
for arg in "$@"; do
  case "$arg" in
    -ko) SHOW_ONLY_KO=1 ;;
  esac
done

run_test() {
    local test_name="$1"
    declare -n test="$test_name"

    local titre="${test[titre]}"
    local fichier="${test[fichier]}"
    local expected_code="${test[exitcode]}"
    local expected_output="${test[output]}"

    local bin
    bin="$(mktemp --tmpdir glados_test_XXXXXX.gla)"

    local compile_out compile_ret
    compile_out=$(./glados "$fichier" "$bin" 2>&1)
    compile_ret=$?

    local runtime_raw=""
    local output ret
    output=""
    ret=0

    if [[ "$compile_ret" -ne 0 ]]; then

        ret="$compile_ret"
        runtime_raw=""
    else
        runtime_raw=$(./glados-vm "$bin" 2>&1)
        ret=$?
    fi

    rm -f "$bin"

    output="$runtime_raw"
    output=${output%$'\n'}
    output=${output%$'\r'}

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

    if [[ $has_error -eq 1 ]]; then
        if [[ "$compile_ret" -ne 0 ]]; then
            if [[ -n "$compile_out" ]]; then
                err_msg+="Message du compilateur :\n$compile_out\n"
            fi
        else
            if [[ -n "$runtime_raw" ]]; then
                err_msg+="Message du programme :\n$runtime_raw\n"
            fi
        fi
    fi

    if [[ $has_error -eq 0 ]]; then
        if [[ $SHOW_ONLY_KO -eq 0 ]]; then
            echo -e "[${GREEN}OK${RESET}] ${titre}"
        fi
        return 0
    else
        if [[ $SHOW_ONLY_KO -eq 0 ]]; then
            echo -e "[${RED}KO${RESET}] ${titre}"
            echo -e "${YELLOW}----------------------------------------${RESET}"
            echo -e "$err_msg" | sed 's/^/    /'
            echo -e "${YELLOW}----------------------------------------${RESET}"
        else
            echo -e "[${RED}KO${RESET}] ${titre}"
            echo -e "${YELLOW}----------------------------------------${RESET}"
            echo -e "$err_msg" | sed 's/^/    /'
            echo -e "${YELLOW}----------------------------------------${RESET}"
        fi
        return 1
    fi
}

run_all_tests() {
    local tests=(
    test_lib_maths_module_import_ok

    test_lib_maths_basic_abs_ok
    test_lib_maths_basic_sign_neg_ok
    test_lib_maths_basic_min_ok
    test_lib_maths_basic_max_ok
    test_lib_maths_basic_clamp_ok
    test_lib_maths_basic_is_even_ok
    test_lib_maths_basic_is_odd_ok
    test_lib_maths_basic_modpos_neg_ok
    test_lib_maths_basic_modpos_bad_mod_ok

    test_lib_maths_combin_fact_ok
    test_lib_maths_combin_fact_neg_ok
    test_lib_maths_combin_perm_ok
    test_lib_maths_combin_binom_ok

    test_lib_maths_div_trunc_ok
    test_lib_maths_div_trunc_div0_ok
    test_lib_maths_div_ceil_ok

    test_lib_maths_gcd_ok
    test_lib_maths_lcm_ok
    test_lib_maths_coprime_ok

    test_lib_maths_phi_ok
    test_lib_maths_modinv_ok

    test_lib_maths_ispow2_true_ok
    test_lib_maths_ispow2_false_ok
    test_lib_maths_nextpow2_ok

    test_lib_maths_pow_ok
    test_lib_maths_pow10_ok
    test_lib_maths_modpow_ok

    test_lib_maths_isprime_true_ok
    test_lib_maths_isprime_false_ok
    test_lib_maths_nextprime_ok

    test_lib_maths_sumrange_ok
    test_lib_maths_prodrange_ok

    test_lib_maths_issqrt_ok
    test_lib_maths_issquare_true_ok
    test_lib_maths_issquare_false_ok

    test_lib_maths_basic_abs_zero_ok
    test_lib_maths_basic_abs_pos_ok
    test_lib_maths_basic_sign_zero_ok
    test_lib_maths_basic_sign_pos_ok
    test_lib_maths_basic_clamp_inside_ok
    test_lib_maths_basic_clamp_lo_ok
    test_lib_maths_basic_clamp_hi_ok
    test_lib_maths_basic_modpos_pos_ok
    test_lib_maths_basic_modpos_m1_ok
    test_lib_maths_basic_modpos_mneg_ok

    test_lib_maths_combin_fact_zero_ok
    test_lib_maths_combin_perm_k0_ok
    test_lib_maths_combin_perm_invalid_ok
    test_lib_maths_combin_binom_k0_ok
    test_lib_maths_combin_binom_kn_ok
    test_lib_maths_combin_binom_sym_ok

    test_lib_maths_div_ceil_exact_ok

    test_lib_maths_gcd_a0_ok
    test_lib_maths_gcd_0b_ok
    test_lib_maths_lcm_zero_ok
    test_lib_maths_coprime_false_ok

    test_lib_maths_phi_1_ok
    test_lib_maths_phi_prime_ok

    test_lib_maths_ispow2_1_ok
    test_lib_maths_ispow2_0_ok
    test_lib_maths_nextpow2_1_ok
    test_lib_maths_nextpow2_pow2_ok

    test_lib_maths_pow_exp0_ok
    test_lib_maths_pow_zero_base_ok
    test_lib_maths_modpow_m1_ok

    test_lib_maths_isprime_2_ok
    test_lib_maths_isprime_0_ok
    test_lib_maths_nextprime_prime_ok

    test_lib_maths_sumrange_single_ok
    test_lib_maths_prodrange_single_ok

    test_lib_maths_issqrt_0_ok
    test_lib_maths_issqrt_1_ok
    test_lib_maths_issquare_0_ok
    test_lib_maths_issquare_1_ok

    test_lib_maths_gcd_commutative_ok
    test_lib_maths_gcd_lcm_relation_ok

    test_lib_maths_power_modpow_range_ok

    test_lib_maths_ntheory_modinv_correct_ok

    )


    local total=${#tests[@]}
    local passed=0
    local failed=0

    local -a ko_blocks=()

    for t in "${tests[@]}"; do
        if [[ $SHOW_ONLY_KO -eq 1 ]]; then
            local out
            if out="$(run_test "$t")"; then
                ((passed++))
            else
                ko_blocks+=("$out")
                failed=1
            fi
        else
            if run_test "$t"; then
                ((passed++))
            else
                failed=1
            fi
        fi
    done

    if [[ $SHOW_ONLY_KO -eq 1 ]]; then
        if ((${#ko_blocks[@]} > 0)); then
            for b in "${ko_blocks[@]}"; do
                echo -e "$b"
            done
        fi
    fi

    echo
    echo "Résultat : $passed / $total tests OK"
    return $failed
}


run_all_tests
exit $?
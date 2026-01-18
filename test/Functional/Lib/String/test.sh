#!/usr/bin/env bash

# declare -A test1=( [titre]="Basic : " [fichier]=".scm" [exitcode]="0" [output]="")

# Basic
#!/usr/bin/env bash
# =========================
# Lib String (Functional)
# Location: test/Functional/Lib/String/...
# Imports: src/@lib/string/string_utils.npy
# =========================

# 01) module
declare -A test_lib_string_module_import_ok=([titre]="Lib String : module import ok" [fichier]="test/Functional/Lib/String/module/import_ok.npy" [exitcode]="0" [output]="42")

# 02-13) transform (12)
declare -A test_lib_string_transform_toUpper_basic_ok=([titre]="Lib String : toUpper('aBc!') == 'ABC!'" [fichier]="test/Functional/Lib/String/transform/toUpper_basic_ok.npy" [exitcode]="0" [output]="ABC!")
declare -A test_lib_string_transform_toUpper_already_ok=([titre]="Lib String : toUpper('ABC') == 'ABC'" [fichier]="test/Functional/Lib/String/transform/toUpper_already_ok.npy" [exitcode]="0" [output]="ABC")
declare -A test_lib_string_transform_toUpper_empty_ok=([titre]="Lib String : toUpper('') == ''" [fichier]="test/Functional/Lib/String/transform/toUpper_empty_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_transform_toLower_basic_ok=([titre]="Lib String : toLower('aBc!') == 'abc!'" [fichier]="test/Functional/Lib/String/transform/toLower_basic_ok.npy" [exitcode]="0" [output]="abc!")
declare -A test_lib_string_transform_capitalize_basic_ok=([titre]="Lib String : capitalize('hello') == 'Hello'" [fichier]="test/Functional/Lib/String/transform/capitalize_basic_ok.npy" [exitcode]="0" [output]="Hello")
declare -A test_lib_string_transform_capitalize_empty_ok=([titre]="Lib String : capitalize('') == ''" [fichier]="test/Functional/Lib/String/transform/capitalize_empty_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_transform_swapCase_basic_ok=([titre]="Lib String : swapCase('AbC') == 'aBc'" [fichier]="test/Functional/Lib/String/transform/swapCase_basic_ok.npy" [exitcode]="0" [output]="aBc")
declare -A test_lib_string_transform_reverse_basic_ok=([titre]="Lib String : reverse('abc') == 'cba'" [fichier]="test/Functional/Lib/String/transform/reverse_basic_ok.npy" [exitcode]="0" [output]="cba")
declare -A test_lib_string_transform_reverse_empty_ok=([titre]="Lib String : reverse('') == ''" [fichier]="test/Functional/Lib/String/transform/reverse_empty_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_transform_repeat_3_ok=([titre]="Lib String : repeat('ab',3) == 'ababab'" [fichier]="test/Functional/Lib/String/transform/repeat_3_ok.npy" [exitcode]="0" [output]="ababab")
declare -A test_lib_string_transform_repeat_1_ok=([titre]="Lib String : repeat('ab',1) == 'ab'" [fichier]="test/Functional/Lib/String/transform/repeat_1_ok.npy" [exitcode]="0" [output]="ab")
declare -A test_lib_string_transform_repeat_0_ok=([titre]="Lib String : repeat('ab',0) == ''" [fichier]="test/Functional/Lib/String/transform/repeat_0_ok.npy" [exitcode]="0" [output]="")

# 14-25) clean (12)
declare -A test_lib_string_clean_trim_spaces_ok=([titre]="Lib String : trim('  Hello World  ') == 'Hello World'" [fichier]="test/Functional/Lib/String/clean/trim_spaces_ok.npy" [exitcode]="0" [output]="Hello World")
declare -A test_lib_string_clean_trim_tabs_ok=([titre]="Lib String : trim('\\t\\tHello\\t') == 'Hello'" [fichier]="test/Functional/Lib/String/clean/trim_tabs_ok.npy" [exitcode]="0" [output]="Hello")
declare -A test_lib_string_clean_trim_mixed_ok=([titre]="Lib String : trim(mixed ws) == 'Hello'" [fichier]="test/Functional/Lib/String/clean/trim_mixed_ok.npy" [exitcode]="0" [output]="Hello")
declare -A test_lib_string_clean_ltrim_basic_ok=([titre]="Lib String : ltrim('   Hello') == 'Hello'" [fichier]="test/Functional/Lib/String/clean/ltrim_basic_ok.npy" [exitcode]="0" [output]="Hello")
declare -A test_lib_string_clean_rtrim_basic_ok=([titre]="Lib String : rtrim('Hello   ') == 'Hello'" [fichier]="test/Functional/Lib/String/clean/rtrim_basic_ok.npy" [exitcode]="0" [output]="Hello")
declare -A test_lib_string_clean_removeSpaces_basic_ok=([titre]="Lib String : removeSpaces('a b  c') == 'abc'" [fichier]="test/Functional/Lib/String/clean/removeSpaces_basic_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_clean_removeSpaces_none_ok=([titre]="Lib String : removeSpaces('abc') == 'abc'" [fichier]="test/Functional/Lib/String/clean/removeSpaces_none_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_clean_removeTabs_basic_ok=([titre]="Lib String : removeTabs('a\\tb\\tc') == 'abc'" [fichier]="test/Functional/Lib/String/clean/removeTabs_basic_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_clean_removeNewLines_basic_ok=([titre]="Lib String : removeNewLines('a\\nb\\r\\nc') == 'abc'" [fichier]="test/Functional/Lib/String/clean/removeNewLines_basic_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_clean_slugify_simple_ok=([titre]="Lib String : slugify('Hello World') == 'hello-world'" [fichier]="test/Functional/Lib/String/clean/slugify_simple_ok.npy" [exitcode]="0" [output]="hello-world")
declare -A test_lib_string_clean_slugify_multi_ws_ok=([titre]="Lib String : slugify(multi ws) == 'hello-world'" [fichier]="test/Functional/Lib/String/clean/slugify_multi_ws_ok.npy" [exitcode]="0" [output]="hello-world")
declare -A test_lib_string_clean_slugify_special_chars_ok=([titre]="Lib String : slugify('Hello, World!') == 'hello-world'" [fichier]="test/Functional/Lib/String/clean/slugify_special_chars_ok.npy" [exitcode]="0" [output]="hello-world")

# 26-39) slice/length (14)
declare -A test_lib_string_slice_length_empty_ok=([titre]="Lib String : length('') == 0" [fichier]="test/Functional/Lib/String/slice/length_empty_ok.npy" [exitcode]="0" [output]="0")
declare -A test_lib_string_slice_length_basic_ok=([titre]="Lib String : length('abc') == 3" [fichier]="test/Functional/Lib/String/slice/length_basic_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lib_string_slice_substring_mid_ok=([titre]="Lib String : substring('abcdef',2,3) == 'cde'" [fichier]="test/Functional/Lib/String/slice/substring_mid_ok.npy" [exitcode]="0" [output]="cde")
declare -A test_lib_string_slice_substring_len_over_ok=([titre]="Lib String : substring('abc',1,10) == 'bc'" [fichier]="test/Functional/Lib/String/slice/substring_len_over_ok.npy" [exitcode]="0" [output]="bc")
declare -A test_lib_string_slice_substring_start0_ok=([titre]="Lib String : substring('abcdef',0,2) == 'ab'" [fichier]="test/Functional/Lib/String/slice/substring_start0_ok.npy" [exitcode]="0" [output]="ab")
declare -A test_lib_string_slice_substring_start_oob_ok=([titre]="Lib String : substring('abc',10,2) == ''" [fichier]="test/Functional/Lib/String/slice/substring_start_oob_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_slice_slice_mid_ok=([titre]="Lib String : slice('abcdef',1,3) == 'bcd'" [fichier]="test/Functional/Lib/String/slice/slice_mid_ok.npy" [exitcode]="0" [output]="bcd")
declare -A test_lib_string_slice_slice_single_ok=([titre]="Lib String : slice('abcdef',2,2) == 'c'" [fichier]="test/Functional/Lib/String/slice/slice_single_ok.npy" [exitcode]="0" [output]="c")
declare -A test_lib_string_slice_slice_start_gt_end_ok=([titre]="Lib String : slice start>end returns ''" [fichier]="test/Functional/Lib/String/slice/slice_start_gt_end_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_slice_slice_end_over_ok=([titre]="Lib String : slice end overflow returns until end" [fichier]="test/Functional/Lib/String/slice/slice_end_over_ok.npy" [exitcode]="0" [output]="cdef")
declare -A test_lib_string_slice_left_basic_ok=([titre]="Lib String : left('abcdef',2) == 'ab'" [fichier]="test/Functional/Lib/String/slice/left_basic_ok.npy" [exitcode]="0" [output]="ab")
declare -A test_lib_string_slice_left_n0_ok=([titre]="Lib String : left('abcdef',0) == ''" [fichier]="test/Functional/Lib/String/slice/left_n0_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_slice_left_over_ok=([titre]="Lib String : left('abc',99) == 'abc'" [fichier]="test/Functional/Lib/String/slice/left_over_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_slice_right_basic_ok=([titre]="Lib String : right('abcdef',2) == 'ef'" [fichier]="test/Functional/Lib/String/slice/right_basic_ok.npy" [exitcode]="0" [output]="ef")
declare -A test_lib_string_slice_right_over_ok=([titre]="Lib String : right('abc',99) == 'abc'" [fichier]="test/Functional/Lib/String/slice/right_over_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_slice_right_n0_ok=([titre]="Lib String : right('abcdef',0) == ''" [fichier]="test/Functional/Lib/String/slice/right_n0_ok.npy" [exitcode]="0" [output]="")

# 40-47) split/join/lines (8)
declare -A test_lib_string_splitjoin_split_count3_ok=([titre]="Lib String : split('a,b,c',',') count == 3" [fichier]="test/Functional/Lib/String/splitjoin/split_count3_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lib_string_splitjoin_split_no_delim_count1_ok=([titre]="Lib String : split('abc',',') count == 1" [fichier]="test/Functional/Lib/String/splitjoin/split_no_delim_count1_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lib_string_splitjoin_split_space_count3_ok=([titre]="Lib String : split('a b c',' ') count == 3" [fichier]="test/Functional/Lib/String/splitjoin/split_space_count3_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lib_string_splitjoin_join_three_ok=([titre]="Lib String : join(['a','b','c'],',') == 'a,b,c'" [fichier]="test/Functional/Lib/String/splitjoin/join_three_ok.npy" [exitcode]="0" [output]="a,b,c")
declare -A test_lib_string_splitjoin_join_one_ok=([titre]="Lib String : join(['abc'],',') == 'abc'" [fichier]="test/Functional/Lib/String/splitjoin/join_one_ok.npy" [exitcode]="0" [output]="abc")
declare -A test_lib_string_splitjoin_join_empty_ok=([titre]="Lib String : join([],',') == ''" [fichier]="test/Functional/Lib/String/splitjoin/join_empty_ok.npy" [exitcode]="0" [output]="")
declare -A test_lib_string_splitjoin_split_join_roundtrip_ok=([titre]="Lib String : join(split('a,b,c',','),',') == 'a,b,c'" [fichier]="test/Functional/Lib/String/splitjoin/split_join_roundtrip_ok.npy" [exitcode]="0" [output]="a,b,c")
declare -A test_lib_string_splitjoin_lines_count3_ok=([titre]="Lib String : lines('a\\nb\\nc') count == 3" [fichier]="test/Functional/Lib/String/splitjoin/lines_count3_ok.npy" [exitcode]="0" [output]="3")

# 48-71) validate (24 incl hasOnly_empty_true)
declare -A test_lib_string_validate_isEmpty_empty_ok=([titre]="Lib String : isEmpty([]) == True" [fichier]="test/Functional/Lib/String/validate/isEmpty_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isEmpty_non_ok=([titre]="Lib String : isEmpty('a') == False" [fichier]="test/Functional/Lib/String/validate/isEmpty_non_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_isBlank_empty_ok=([titre]="Lib String : isBlank([]) == True" [fichier]="test/Functional/Lib/String/validate/isBlank_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isBlank_spaces_ok=([titre]="Lib String : isBlank('   ') == True" [fichier]="test/Functional/Lib/String/validate/isBlank_spaces_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isBlank_tabs_newlines_ok=([titre]="Lib String : isBlank('\\t\\n\\r') == True" [fichier]="test/Functional/Lib/String/validate/isBlank_tabs_newlines_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isBlank_nonblank_ok=([titre]="Lib String : isBlank('a b') == False" [fichier]="test/Functional/Lib/String/validate/isBlank_nonblank_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_isAlpha_empty_ok=([titre]="Lib String : isAlpha([]) == True" [fichier]="test/Functional/Lib/String/validate/isAlpha_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isAlpha_letters_ok=([titre]="Lib String : isAlpha('abcXYZ') == True" [fichier]="test/Functional/Lib/String/validate/isAlpha_letters_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isAlpha_with_space_false_ok=([titre]="Lib String : isAlpha('abc XYZ') == False" [fichier]="test/Functional/Lib/String/validate/isAlpha_with_space_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_isNumeric_empty_ok=([titre]="Lib String : isNumeric([]) == True" [fichier]="test/Functional/Lib/String/validate/isNumeric_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isNumeric_digits_ok=([titre]="Lib String : isNumeric('0123') == True" [fichier]="test/Functional/Lib/String/validate/isNumeric_digits_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isNumeric_with_letter_false_ok=([titre]="Lib String : isNumeric('12a3') == False" [fichier]="test/Functional/Lib/String/validate/isNumeric_with_letter_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_string_validate_isNumeric_negative_false_ok=([titre]="Lib String : isNumeric('-12') == False" [fichier]="test/Functional/Lib/String/validate/isNumeric_negative_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_isAlnum_empty_ok=([titre]="Lib String : isAlnum([]) == True" [fichier]="test/Functional/Lib/String/validate/isAlnum_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isAlnum_mix_ok=([titre]="Lib String : isAlnum('abc123XYZ') == True" [fichier]="test/Functional/Lib/String/validate/isAlnum_mix_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isAlnum_punct_false_ok=([titre]="Lib String : isAlnum('abc-123') == False" [fichier]="test/Functional/Lib/String/validate/isAlnum_punct_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_isUpper_empty_ok=([titre]="Lib String : isUpper([]) == True" [fichier]="test/Functional/Lib/String/validate/isUpper_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isUpper_true_ok=([titre]="Lib String : isUpper('ABC') == True" [fichier]="test/Functional/Lib/String/validate/isUpper_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isUpper_false_ok=([titre]="Lib String : isUpper('AbC') == False" [fichier]="test/Functional/Lib/String/validate/isUpper_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_isLower_empty_ok=([titre]="Lib String : isLower([]) == True" [fichier]="test/Functional/Lib/String/validate/isLower_empty_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isLower_true_ok=([titre]="Lib String : isLower('abc') == True" [fichier]="test/Functional/Lib/String/validate/isLower_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_isLower_false_ok=([titre]="Lib String : isLower('abC') == False" [fichier]="test/Functional/Lib/String/validate/isLower_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_lib_string_validate_hasOnly_true_ok=([titre]="Lib String : hasOnly('aaaa','a') == True" [fichier]="test/Functional/Lib/String/validate/hasOnly_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_validate_hasOnly_false_ok=([titre]="Lib String : hasOnly('aaab','a') == False" [fichier]="test/Functional/Lib/String/validate/hasOnly_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_string_validate_hasOnly_empty_true_ok=([titre]="Lib String : hasOnly([],'a') == True" [fichier]="test/Functional/Lib/String/validate/hasOnly_empty_true_ok.npy" [exitcode]="0" [output]="True")

# 72-80) convert (9 total including toBool lowercase)
declare -A test_lib_string_convert_toInt_positive_ok=([titre]="Lib String : toInt('123') == 123" [fichier]="test/Functional/Lib/String/convert/toInt_positive_ok.npy" [exitcode]="0" [output]="123")
declare -A test_lib_string_convert_toInt_negative_ok=([titre]="Lib String : toInt('-42') == -42" [fichier]="test/Functional/Lib/String/convert/toInt_negative_ok.npy" [exitcode]="0" [output]="-42")
declare -A test_lib_string_convert_toInt_leading_zeros_ok=([titre]="Lib String : toInt('0007') == 7" [fichier]="test/Functional/Lib/String/convert/toInt_leading_zeros_ok.npy" [exitcode]="0" [output]="7")
declare -A test_lib_string_convert_toBool_true_ok=([titre]="Lib String : toBool('True') == True" [fichier]="test/Functional/Lib/String/convert/toBool_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_lib_string_convert_toBool_false_ok=([titre]="Lib String : toBool('False') == False" [fichier]="test/Functional/Lib/String/convert/toBool_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_lib_string_convert_toBool_lowercase_false_ok=([titre]="Lib String : toBool('true') == False" [fichier]="test/Functional/Lib/String/convert/toBool_lowercase_false_ok.npy" [exitcode]="0" [output]="False")

# Test func
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

    # 1) Compile
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
        # 01) module
        test_lib_string_module_import_ok

        # 02-13) transform (12)
        test_lib_string_transform_toUpper_basic_ok
        test_lib_string_transform_toUpper_already_ok
        test_lib_string_transform_toUpper_empty_ok
        test_lib_string_transform_toLower_basic_ok
        test_lib_string_transform_capitalize_basic_ok
        test_lib_string_transform_capitalize_empty_ok
        test_lib_string_transform_swapCase_basic_ok
        test_lib_string_transform_reverse_basic_ok
        test_lib_string_transform_reverse_empty_ok
        test_lib_string_transform_repeat_3_ok
        test_lib_string_transform_repeat_1_ok
        test_lib_string_transform_repeat_0_ok

        # 14-25) clean (12)
        test_lib_string_clean_trim_spaces_ok
        test_lib_string_clean_trim_tabs_ok
        test_lib_string_clean_trim_mixed_ok
        test_lib_string_clean_ltrim_basic_ok
        test_lib_string_clean_rtrim_basic_ok
        test_lib_string_clean_removeSpaces_basic_ok
        test_lib_string_clean_removeSpaces_none_ok
        test_lib_string_clean_removeTabs_basic_ok
        test_lib_string_clean_removeNewLines_basic_ok
        test_lib_string_clean_slugify_simple_ok
        test_lib_string_clean_slugify_multi_ws_ok
        test_lib_string_clean_slugify_special_chars_ok

        # 26-39) slice/length (14)
        test_lib_string_slice_length_empty_ok
        test_lib_string_slice_length_basic_ok
        test_lib_string_slice_substring_mid_ok
        test_lib_string_slice_substring_len_over_ok
        test_lib_string_slice_substring_start0_ok
        test_lib_string_slice_substring_start_oob_ok
        test_lib_string_slice_slice_mid_ok
        test_lib_string_slice_slice_single_ok
        test_lib_string_slice_slice_start_gt_end_ok
        test_lib_string_slice_slice_end_over_ok
        test_lib_string_slice_left_basic_ok
        test_lib_string_slice_left_n0_ok
        test_lib_string_slice_left_over_ok
        test_lib_string_slice_right_basic_ok
        test_lib_string_slice_right_over_ok
        test_lib_string_slice_right_n0_ok

        # 40-47) split/join/lines (8)
        test_lib_string_splitjoin_split_count3_ok
        test_lib_string_splitjoin_split_no_delim_count1_ok
        test_lib_string_splitjoin_split_space_count3_ok
        test_lib_string_splitjoin_join_three_ok
        test_lib_string_splitjoin_join_one_ok
        test_lib_string_splitjoin_join_empty_ok
        test_lib_string_splitjoin_split_join_roundtrip_ok
        test_lib_string_splitjoin_lines_count3_ok

        # 48-71) validate (24)
        test_lib_string_validate_isEmpty_empty_ok
        test_lib_string_validate_isEmpty_non_ok
        test_lib_string_validate_isBlank_empty_ok
        test_lib_string_validate_isBlank_spaces_ok
        test_lib_string_validate_isBlank_tabs_newlines_ok
        test_lib_string_validate_isBlank_nonblank_ok
        test_lib_string_validate_isAlpha_empty_ok
        test_lib_string_validate_isAlpha_letters_ok
        test_lib_string_validate_isAlpha_with_space_false_ok
        test_lib_string_validate_isNumeric_empty_ok
        test_lib_string_validate_isNumeric_digits_ok
        test_lib_string_validate_isNumeric_with_letter_false_ok
        test_lib_string_validate_isNumeric_negative_false_ok
        test_lib_string_validate_isAlnum_empty_ok
        test_lib_string_validate_isAlnum_mix_ok
        test_lib_string_validate_isAlnum_punct_false_ok
        test_lib_string_validate_isUpper_empty_ok
        test_lib_string_validate_isUpper_true_ok
        test_lib_string_validate_isUpper_false_ok
        test_lib_string_validate_isLower_empty_ok
        test_lib_string_validate_isLower_true_ok
        test_lib_string_validate_isLower_false_ok
        test_lib_string_validate_hasOnly_true_ok
        test_lib_string_validate_hasOnly_false_ok
        test_lib_string_validate_hasOnly_empty_true_ok

        # 72-80) convert (6 shown here)
        test_lib_string_convert_toInt_positive_ok
        test_lib_string_convert_toInt_negative_ok
        test_lib_string_convert_toInt_leading_zeros_ok
        test_lib_string_convert_toBool_true_ok
        test_lib_string_convert_toBool_false_ok
        test_lib_string_convert_toBool_lowercase_false_ok
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
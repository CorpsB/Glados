#!/usr/bin/env bash

# declare -A test1=( [titre]="Basic : " [fichier]=".scm" [exitcode]="0" [output]="")

# Basic
declare -A test_basics_import_simple=([titre]="Basics : import simple" [fichier]="test/Functional/Basics/import_simple.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_function=([titre]="Basics : import function" [fichier]="test/Functional/Basics/import_function.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_multiple=([titre]="Basics : import multiple" [fichier]="test/Functional/Basics/import_multiple.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_missing_file=([titre]="Basics : import missing file" [fichier]="test/Functional/Basics/import_missing_file.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_symbol_visibility=([titre]="Basics : symbol visibility (no import)" [fichier]="test/Functional/Basics/import_symbol_visibility.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_unused=([titre]="Basics : import unused" [fichier]="test/Functional/Basics/import_unused.npy" [exitcode]="0" [output]="42")

#Syntax
declare -A test_syntax_semicolon_ok=([titre]="Syntax : semicolon ok" [fichier]="test/Functional/Syntax/semicolon_ok.npy" [exitcode]="0" [output]="42")
declare -A test_syntax_semicolon_missing=([titre]="Syntax : missing semicolon" [fichier]="test/Functional/Syntax/semicolon_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_block_ok=([titre]="Syntax : block braces ok" [fichier]="test/Functional/Syntax/block_ok.npy" [exitcode]="0" [output]="42")
declare -A test_syntax_block_missing=([titre]="Syntax : missing braces" [fichier]="test/Functional/Syntax/block_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_if_parentheses_missing=([titre]="Syntax : if missing parentheses" [fichier]="test/Functional/Syntax/if_parentheses_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_while_parentheses_missing=([titre]="Syntax : while missing parentheses" [fichier]="test/Functional/Syntax/while_parentheses_missing.npy" [exitcode]="84" [output]="")

#Declaration
declare -A test_declarations_infer_int_ok=([titre]="Declarations : infer int ok" [fichier]="test/Functional/Declarations/infer_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_declarations_explicit_int_ok=([titre]="Declarations : explicit int ok" [fichier]="test/Functional/Declarations/explicit_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_declarations_infer_bool_ok=([titre]="Declarations : infer bool ok" [fichier]="test/Functional/Declarations/infer_bool_ok.npy" [exitcode]="0" [output]="True")
declare -A test_declarations_explicit_bool_ok=([titre]="Declarations : explicit bool ok" [fichier]="test/Functional/Declarations/explicit_bool_ok.npy" [exitcode]="0" [output]="False")
declare -A test_declarations_void_declaration_ok=([titre]="Declarations : void declaration ok" [fichier]="test/Functional/Declarations/void_declaration_ok.npy" [exitcode]="0" [output]="42")
declare -A test_declarations_explicit_init_type_mismatch=([titre]="Declarations : explicit init type mismatch" [fichier]="test/Functional/Declarations/explicit_init_type_mismatch.npy" [exitcode]="84" [output]="")
declare -A test_declarations_infer_then_assign_wrong_type=([titre]="Declarations : infer then assign wrong type" [fichier]="test/Functional/Declarations/infer_then_assign_wrong_type.npy" [exitcode]="84" [output]="")
declare -A test_declarations_explicit_then_assign_wrong_type=([titre]="Declarations : explicit then assign wrong type" [fichier]="test/Functional/Declarations/explicit_then_assign_wrong_type.npy" [exitcode]="84" [output]="")
declare -A test_declarations_bool_then_assign_int=([titre]="Declarations : bool then assign int" [fichier]="test/Functional/Declarations/bool_then_assign_int.npy" [exitcode]="84" [output]="")

#Asign
declare -A test_assign_reassign_int_ok=([titre]="Assign : reassign int ok" [fichier]="test/Functional/Assign/reassign_int_ok.npy" [exitcode]="0" [output]="2")
declare -A test_assign_reassign_chain_ok=([titre]="Assign : reassign chain ok" [fichier]="test/Functional/Assign/reassign_chain_ok.npy" [exitcode]="0" [output]="42")
declare -A test_assign_for_header_init_update_ok=([titre]="Assign : for header init/update ok" [fichier]="test/Functional/Assign/for_header_init_update_ok.npy" [exitcode]="0" [output]="6")
declare -A test_assign_use_undeclared_var=([titre]="Assign : use undeclared var" [fichier]="test/Functional/Assign/use_undeclared_var.npy" [exitcode]="84" [output]="")
declare -A test_assign_reassign_type_mismatch=([titre]="Assign : reassign type mismatch" [fichier]="test/Functional/Assign/reassign_type_mismatch.npy" [exitcode]="84" [output]="")
declare -A test_assign_for_header_type_mismatch=([titre]="Assign : for header type mismatch" [fichier]="test/Functional/Assign/for_header_type_mismatch.npy" [exitcode]="84" [output]="")

#Literals
declare -A test_literals_int_positive_ok=([titre]="Literals : int positive" [fichier]="test/Functional/Literals/int_positive_ok.npy" [exitcode]="0" [output]="42")
declare -A test_literals_int_negative_ok=([titre]="Literals : int negative" [fichier]="test/Functional/Literals/int_negative_ok.npy" [exitcode]="0" [output]="-42")
declare -A test_literals_bool_true_ok=([titre]="Literals : bool True" [fichier]="test/Functional/Literals/bool_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_literals_bool_false_ok=([titre]="Literals : bool False" [fichier]="test/Functional/Literals/bool_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_literals_void_value_ok=([titre]="Literals : void literal value" [fichier]="test/Functional/Literals/void_value_ok.npy" [exitcode]="0" [output]="42")
declare -A test_literals_list_int_ok=([titre]="Literals : list of int" [fichier]="test/Functional/Literals/list_int_ok.npy" [exitcode]="0" [output]="2")
declare -A test_literals_list_nested_ok=([titre]="Literals : nested list" [fichier]="test/Functional/Literals/list_nested_ok.npy" [exitcode]="0" [output]="1")
declare -A test_literals_string_sugar_ok=([titre]="Literals : string sugar" [fichier]="test/Functional/Literals/string_sugar_ok.npy" [exitcode]="0" [output]="Noopy")

#Strings
declare -A test_strings_print_literal_ok=([titre]="Strings : print literal" [fichier]="test/Functional/Strings/print_literal_ok.npy" [exitcode]="0" [output]="Hello"])
declare -A test_strings_assign_to_char_list_ok=([titre]="Strings : assign to [char]" [fichier]="test/Functional/Strings/assign_to_char_list_ok.npy" [exitcode]="0" [output]="World"])
declare -A test_strings_pass_to_function_ok=([titre]="Strings : pass to function [char]" [fichier]="test/Functional/Strings/pass_to_function_ok.npy" [exitcode]="0" [output]="Noopy"])
declare -A test_strings_assign_string_to_int_list=([titre]="Strings : string to [int] error" [fichier]="test/Functional/Strings/assign_string_to_int_list.npy" [exitcode]="84" [output]=""])
declare -A test_strings_pass_string_to_wrong_param=([titre]="Strings : pass string to wrong param" [fichier]="test/Functional/Strings/pass_string_to_wrong_param.npy" [exitcode]="84" [output]=""])

#Lists
declare -A test_lists_literal_and_index_ok=([titre]="Lists : literal + index" [fichier]="test/Functional/Lists/literal_and_index_ok.npy" [exitcode]="0" [output]="10"])
declare -A test_lists_index_zero_based_ok=([titre]="Lists : index 0-based" [fichier]="test/Functional/Lists/index_zero_based_ok.npy" [exitcode]="0" [output]="1"])
declare -A test_lists_assign_element_to_var_ok=([titre]="Lists : element to var" [fichier]="test/Functional/Lists/assign_element_to_var_ok.npy" [exitcode]="0" [output]="3"])
declare -A test_lists_nested_index_ok=([titre]="Lists : nested index" [fichier]="test/Functional/Lists/nested_index_ok.npy" [exitcode]="0" [output]="1"])
declare -A test_lists_inconsistent_literal_type=([titre]="Lists : inconsistent literal type" [fichier]="test/Functional/Lists/inconsistent_literal_type.npy" [exitcode]="84" [output]=""])
declare -A test_lists_nested_inconsistent_type=([titre]="Lists : nested inconsistent type" [fichier]="test/Functional/Lists/nested_inconsistent_type.npy" [exitcode]="84" [output]=""])
declare -A test_lists_index_out_of_bounds=([titre]="Lists : index out of bounds" [fichier]="test/Functional/Lists/index_out_of_bounds.npy" [exitcode]="84" [output]=""])

#Expression
declare -A test_expr_add_sub_mul_ok=([titre]="Expressions : add sub mul" [fichier]="test/Functional/Expressions/add_sub_mul_ok.npy" [exitcode]="0" [output]="14"])
declare -A test_expr_div_operator_ok=([titre]="Expressions : div operator" [fichier]="test/Functional/Expressions/div_operator_ok.npy" [exitcode]="0" [output]="3"])
declare -A test_expr_div_keyword_ok=([titre]="Expressions : div keyword" [fichier]="test/Functional/Expressions/div_keyword_ok.npy" [exitcode]="0" [output]="3"])
declare -A test_expr_mod_operator_ok=([titre]="Expressions : mod operator" [fichier]="test/Functional/Expressions/mod_operator_ok.npy" [exitcode]="0" [output]="1"])
declare -A test_expr_mod_keyword_ok=([titre]="Expressions : mod keyword" [fichier]="test/Functional/Expressions/mod_keyword_ok.npy" [exitcode]="0" [output]="1"])
declare -A test_expr_operator_precedence_ok=([titre]="Expressions : operator precedence" [fichier]="test/Functional/Expressions/operator_precedence_ok.npy" [exitcode]="0" [output]="14"])
declare -A test_expr_division_by_zero=([titre]="Expressions : division by zero" [fichier]="test/Functional/Expressions/division_by_zero.npy" [exitcode]="84" [output]=""])
declare -A test_expr_modulo_by_zero=([titre]="Expressions : modulo by zero" [fichier]="test/Functional/Expressions/modulo_by_zero.npy" [exitcode]="84" [output]=""])

#Comparaison
declare -A test_cmp_eq_true_ok=([titre]="Comparisons : == true" [fichier]="test/Functional/Comparisons/eq_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_eq_false_ok=([titre]="Comparisons : == false" [fichier]="test/Functional/Comparisons/eq_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_cmp_neq_true_ok=([titre]="Comparisons : != true" [fichier]="test/Functional/Comparisons/neq_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_lt_ok=([titre]="Comparisons : <" [fichier]="test/Functional/Comparisons/lt_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_gt_ok=([titre]="Comparisons : >" [fichier]="test/Functional/Comparisons/gt_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_lte_ok=([titre]="Comparisons : <=" [fichier]="test/Functional/Comparisons/lte_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_gte_ok=([titre]="Comparisons : >=" [fichier]="test/Functional/Comparisons/gte_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_bool_eq_ok=([titre]="Comparisons : bool ==" [fichier]="test/Functional/Comparisons/bool_eq_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_mismatched_types_eq=([titre]="Comparisons : type mismatch ==" [fichier]="test/Functional/Comparisons/mismatched_types_eq.npy" [exitcode]="84" [output]="")
declare -A test_cmp_list_eq_disallowed=([titre]="Comparisons : list == disallowed" [fichier]="test/Functional/Comparisons/list_eq_disallowed.npy" [exitcode]="84" [output]="")

#Logical
declare -A test_logical_and_true_ok=([titre]="Logical : && true" [fichier]="test/Functional/Logical/and_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_logical_and_false_ok=([titre]="Logical : && false" [fichier]="test/Functional/Logical/and_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_logical_or_true_ok=([titre]="Logical : || true" [fichier]="test/Functional/Logical/or_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_logical_or_false_ok=([titre]="Logical : || false" [fichier]="test/Functional/Logical/or_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_logical_not_ok=([titre]="Logical : !" [fichier]="test/Functional/Logical/not_ok.npy" [exitcode]="0" [output]="False")
declare -A test_logical_compose_with_comparisons_ok=([titre]="Logical : compose with comparisons" [fichier]="test/Functional/Logical/compose_with_comparisons_ok.npy" [exitcode]="0" [output]="True")
declare -A test_logical_short_circuit_and_ok=([titre]="Logical : short-circuit &&" [fichier]="test/Functional/Logical/short_circuit_and_ok.npy" [exitcode]="0" [output]="False")
declare -A test_logical_short_circuit_or_ok=([titre]="Logical : short-circuit ||" [fichier]="test/Functional/Logical/short_circuit_or_ok.npy" [exitcode]="0" [output]="True")
declare -A test_logical_not_type_error=([titre]="Logical : ! type error" [fichier]="test/Functional/Logical/not_type_error.npy" [exitcode]="84" [output]="")
declare -A test_logical_and_type_error=([titre]="Logical : && type error" [fichier]="test/Functional/Logical/and_type_error.npy" [exitcode]="84" [output]="")

#Conditions
declare -A test_conditions_if_true_ok=([titre]="Conditions : if true" [fichier]="test/Functional/Conditions/if_true_ok.npy" [exitcode]="0" [output]="1")
declare -A test_conditions_if_false_ok=([titre]="Conditions : if false" [fichier]="test/Functional/Conditions/if_false_ok.npy" [exitcode]="0" [output]="2")
declare -A test_conditions_if_else_ok=([titre]="Conditions : if else" [fichier]="test/Functional/Conditions/if_else_ok.npy" [exitcode]="0" [output]="2")
declare -A test_conditions_else_if_chain_ok=([titre]="Conditions : else if chain" [fichier]="test/Functional/Conditions/else_if_chain_ok.npy" [exitcode]="0" [output]="B")
declare -A test_conditions_nested_if_ok=([titre]="Conditions : nested if" [fichier]="test/Functional/Conditions/nested_if_ok.npy" [exitcode]="0" [output]="3")
declare -A test_conditions_block_scope_ok=([titre]="Conditions : block scope" [fichier]="test/Functional/Conditions/block_scope_ok.npy" [exitcode]="0" [output]="1")
declare -A test_conditions_if_condition_type_error=([titre]="Conditions : if condition type error" [fichier]="test/Functional/Conditions/if_condition_type_error.npy" [exitcode]="84" [output]="")
declare -A test_conditions_scope_outside_block_error=([titre]="Conditions : scope outside block error" [fichier]="test/Functional/Conditions/scope_outside_block_error.npy" [exitcode]="84" [output]="")

#Loops
declare -A test_loops_while_basic_ok=([titre]="Loops : while basic" [fichier]="test/Functional/Loops/while_basic_ok.npy" [exitcode]="0" [output]="3")
declare -A test_loops_while_zero_iter_ok=([titre]="Loops : while zero iter" [fichier]="test/Functional/Loops/while_zero_iter_ok.npy" [exitcode]="0" [output]="0")
declare -A test_loops_for_basic_ok=([titre]="Loops : for basic sum" [fichier]="test/Functional/Loops/for_basic_sum_ok.npy" [exitcode]="0" [output]="6")
declare -A test_loops_for_side_effect_update_ok=([titre]="Loops : for side effect update" [fichier]="test/Functional/Loops/for_side_effect_update_ok.npy" [exitcode]="0" [output]="3")
declare -A test_loops_nested_loops_ok=([titre]="Loops : nested loops" [fichier]="test/Functional/Loops/nested_loops_ok.npy" [exitcode]="0" [output]="4")
declare -A test_loops_while_condition_type_error=([titre]="Loops : while condition type error" [fichier]="test/Functional/Loops/while_condition_type_error.npy" [exitcode]="84" [output]="")
declare -A test_loops_for_condition_type_error=([titre]="Loops : for condition type error" [fichier]="test/Functional/Loops/for_condition_type_error.npy" [exitcode]="84" [output]="")
declare -A test_loops_for_update_type_error=([titre]="Loops : for update type error" [fichier]="test/Functional/Loops/for_update_type_error.npy" [exitcode]="84" [output]="")

#Functions
declare -A test_functions_return_int_ok=([titre]="Functions : return int" [fichier]="test/Functional/Functions/return_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_functions_void_implicit_ok=([titre]="Functions : void implicit" [fichier]="test/Functional/Functions/void_implicit_ok.npy" [exitcode]="0" [output]="Hi")
declare -A test_functions_multiple_args_ok=([titre]="Functions : multiple args" [fichier]="test/Functional/Functions/multiple_args_ok.npy" [exitcode]="0" [output]="42")
declare -A test_functions_call_in_expression_ok=([titre]="Functions : call in expression" [fichier]="test/Functional/Functions/call_in_expression_ok.npy" [exitcode]="0" [output]="45")
declare -A test_functions_wrong_arity=([titre]="Functions : wrong arity" [fichier]="test/Functional/Functions/wrong_arity.npy" [exitcode]="84" [output]="")
declare -A test_functions_wrong_arg_type=([titre]="Functions : wrong arg type" [fichier]="test/Functional/Functions/wrong_arg_type.npy" [exitcode]="84" [output]="")
declare -A test_functions_return_type_mismatch=([titre]="Functions : return type mismatch" [fichier]="test/Functional/Functions/return_type_mismatch.npy" [exitcode]="84" [output]="")
declare -A test_functions_missing_ret_in_nonvoid=([titre]="Functions : missing ret in nonvoid" [fichier]="test/Functional/Functions/missing_ret_in_nonvoid.npy" [exitcode]="84" [output]="")

#Lambda
declare -A test_lambdas_basic_multiply_ok=([titre]="Lambdas : basic multiply" [fichier]="test/Functional/Lambdas/basic_multiply_ok.npy" [exitcode]="0" [output]="12")
declare -A test_lambdas_capture_var_ok=([titre]="Lambdas : capture var" [fichier]="test/Functional/Lambdas/capture_var_ok.npy" [exitcode]="0" [output]="42")
declare -A test_lambdas_return_used_in_expr_ok=([titre]="Lambdas : used in expression" [fichier]="test/Functional/Lambdas/used_in_expression_ok.npy" [exitcode]="0" [output]="45")
declare -A test_lambdas_nested_call_ok=([titre]="Lambdas : nested call" [fichier]="test/Functional/Lambdas/nested_call_ok.npy" [exitcode]="0" [output]="16")
declare -A test_lambdas_wrong_arity=([titre]="Lambdas : wrong arity" [fichier]="test/Functional/Lambdas/wrong_arity.npy" [exitcode]="84" [output]="")
declare -A test_lambdas_wrong_arg_type=([titre]="Lambdas : wrong arg type" [fichier]="test/Functional/Lambdas/wrong_arg_type.npy" [exitcode]="84" [output]="")
declare -A test_lambdas_non_callable_used_as_func=([titre]="Lambdas : non callable used as func" [fichier]="test/Functional/Lambdas/non_callable_used_as_func.npy" [exitcode]="84" [output]="")

#Struct
declare -A test_structs_basic_new_access_ok=([titre]="Structs : basic new + access" [fichier]="test/Functional/Structs/basic_new_access_ok.npy" [exitcode]="0" [output]="42")
declare -A test_structs_assign_struct_to_var_ok=([titre]="Structs : assign struct to var" [fichier]="test/Functional/Structs/assign_struct_to_var_ok.npy" [exitcode]="0" [output]="7")
declare -A test_structs_nested_struct_ok=([titre]="Structs : nested struct" [fichier]="test/Functional/Structs/nested_struct_ok.npy" [exitcode]="0" [output]="9")
declare -A test_structs_multiple_fields_ok=([titre]="Structs : multiple fields" [fichier]="test/Functional/Structs/multiple_fields_ok.npy" [exitcode]="0" [output]="3")
declare -A test_structs_missing_field_error=([titre]="Structs : missing field" [fichier]="test/Functional/Structs/missing_field_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_extra_field_error=([titre]="Structs : extra field" [fichier]="test/Functional/Structs/extra_field_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_wrong_field_type_error=([titre]="Structs : wrong field type" [fichier]="test/Functional/Structs/wrong_field_type_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_unknown_field_access_error=([titre]="Structs : unknown field access" [fichier]="test/Functional/Structs/unknown_field_access_error.npy" [exitcode]="84" [output]="")

#Builtins
declare -A test_builtins_print_int_ok=([titre]="Builtins : print int" [fichier]="test/Functional/Builtins/print_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_builtins_print_bool_ok=([titre]="Builtins : print bool" [fichier]="test/Functional/Builtins/print_bool_ok.npy" [exitcode]="0" [output]="True")
declare -A test_builtins_print_string_ok=([titre]="Builtins : print string" [fichier]="test/Functional/Builtins/print_string_ok.npy" [exitcode]="0" [output]="Noopy")
declare -A test_builtins_print_list_int_ok=([titre]="Builtins : print [int] (observe)" [fichier]="test/Functional/Builtins/print_list_int_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_print_nested_list_ok=([titre]="Builtins : print [[int]] (observe)" [fichier]="test/Functional/Builtins/print_nested_list_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_print_struct_ok=([titre]="Builtins : print struct (observe)" [fichier]="test/Functional/Builtins/print_struct_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_print_void_ok=([titre]="Builtins : print void (observe)" [fichier]="test/Functional/Builtins/print_void_ok.npy" [exitcode]="0" [output]="")


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
  # Basics
  test_basics_import_simple
  test_basics_import_function
  test_basics_import_multiple
  test_basics_import_missing_file
  test_basics_import_symbol_visibility
  test_basics_import_unused

  # Syntax
  test_syntax_semicolon_ok
  test_syntax_semicolon_missing
  test_syntax_block_ok
  test_syntax_block_missing
  test_syntax_if_parentheses_missing
  test_syntax_while_parentheses_missing

  # Declarations
  test_declarations_infer_int_ok
  test_declarations_explicit_int_ok
  test_declarations_infer_bool_ok
  test_declarations_explicit_bool_ok
  test_declarations_void_declaration_ok
  test_declarations_explicit_init_type_mismatch
  test_declarations_infer_then_assign_wrong_type
  test_declarations_explicit_then_assign_wrong_type
  test_declarations_bool_then_assign_int

  # Assign
  test_assign_reassign_int_ok
  test_assign_reassign_chain_ok
  test_assign_for_header_init_update_ok
  test_assign_use_undeclared_var
  test_assign_reassign_type_mismatch
  test_assign_for_header_type_mismatch

  # Literals
  test_literals_int_positive_ok
  test_literals_int_negative_ok
  test_literals_bool_true_ok
  test_literals_bool_false_ok
  test_literals_void_value_ok
  test_literals_list_int_ok
  test_literals_list_nested_ok
  test_literals_string_sugar_ok

  # Strings
  test_strings_print_literal_ok
  test_strings_assign_to_char_list_ok
  test_strings_pass_to_function_ok
  test_strings_assign_string_to_int_list
  test_strings_pass_string_to_wrong_param

  # Lists
  test_lists_literal_and_index_ok
  test_lists_index_zero_based_ok
  test_lists_assign_element_to_var_ok
  test_lists_nested_index_ok
  test_lists_inconsistent_literal_type
  test_lists_nested_inconsistent_type
  test_lists_index_out_of_bounds

  # Expressions
  test_expr_add_sub_mul_ok
  test_expr_div_operator_ok
  test_expr_div_keyword_ok
  test_expr_mod_operator_ok
  test_expr_mod_keyword_ok
  test_expr_operator_precedence_ok
  test_expr_division_by_zero
  test_expr_modulo_by_zero

  # Comparisons
  test_cmp_eq_true_ok
  test_cmp_eq_false_ok
  test_cmp_neq_true_ok
  test_cmp_lt_ok
  test_cmp_gt_ok
  test_cmp_lte_ok
  test_cmp_gte_ok
  test_cmp_bool_eq_ok
  test_cmp_mismatched_types_eq
  test_cmp_list_eq_disallowed

  # Logical
  test_logical_and_true_ok
  test_logical_and_false_ok
  test_logical_or_true_ok
  test_logical_or_false_ok
  test_logical_not_ok
  test_logical_compose_with_comparisons_ok
  test_logical_short_circuit_and_ok
  test_logical_short_circuit_or_ok
  test_logical_not_type_error
  test_logical_and_type_error

  # Conditions
  test_conditions_if_true_ok
  test_conditions_if_false_ok
  test_conditions_if_else_ok
  test_conditions_else_if_chain_ok
  test_conditions_nested_if_ok
  test_conditions_block_scope_ok
  test_conditions_if_condition_type_error
  test_conditions_scope_outside_block_error

  # Loops
  test_loops_while_basic_ok
  test_loops_while_zero_iter_ok
  test_loops_for_basic_ok
  test_loops_for_side_effect_update_ok
  test_loops_nested_loops_ok
  test_loops_while_condition_type_error
  test_loops_for_condition_type_error
  test_loops_for_update_type_error

  # Functions
  test_functions_return_int_ok
  test_functions_void_implicit_ok
  test_functions_multiple_args_ok
  test_functions_call_in_expression_ok
  test_functions_wrong_arity
  test_functions_wrong_arg_type
  test_functions_return_type_mismatch
  test_functions_missing_ret_in_nonvoid

  # Lambdas
  test_lambdas_basic_multiply_ok
  test_lambdas_capture_var_ok
  test_lambdas_return_used_in_expr_ok
  test_lambdas_nested_call_ok
  test_lambdas_wrong_arity
  test_lambdas_wrong_arg_type
  test_lambdas_non_callable_used_as_func

  # Structs
  test_structs_basic_new_access_ok
  test_structs_assign_struct_to_var_ok
  test_structs_nested_struct_ok
  test_structs_multiple_fields_ok
  test_structs_missing_field_error
  test_structs_extra_field_error
  test_structs_wrong_field_type_error
  test_structs_unknown_field_access_error

  # Builtins
  test_builtins_print_int_ok
  test_builtins_print_bool_ok
  test_builtins_print_string_ok
  test_builtins_print_list_int_ok
  test_builtins_print_nested_list_ok
  test_builtins_print_struct_ok
  test_builtins_print_void_ok
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
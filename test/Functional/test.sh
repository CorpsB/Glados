#!/usr/bin/env bash

# Basic
declare -A test_basics_import_simple=([titre]="Basics : import simple" [fichier]="test/Functional/Basics/import_simple.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_function=([titre]="Basics : import function" [fichier]="test/Functional/Basics/import_function.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_multiple=([titre]="Basics : import multiple" [fichier]="test/Functional/Basics/import_multiple.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_missing_file=([titre]="Basics : import missing file" [fichier]="test/Functional/Basics/import_missing_file.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_symbol_visibility=([titre]="Basics : symbol visibility (no import)" [fichier]="test/Functional/Basics/import_symbol_visibility.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_unused=([titre]="Basics : import unused" [fichier]="test/Functional/Basics/import_unused.npy" [exitcode]="0" [output]="42")

declare -A test_basics_import_duplicate_ok=([titre]="Basics : import duplicate (idempotent)" [fichier]="test/Functional/Basics/ImportAdvanced/import_duplicate_ok.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_circular_error=([titre]="Basics : import circular (A<->B) error" [fichier]="test/Functional/Basics/ImportAdvanced/import_circular_error.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_transitive_visibility_error=([titre]="Basics : import transitive visibility error" [fichier]="test/Functional/Basics/ImportAdvanced/import_transitive_visibility_error.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_name_conflict_error=([titre]="Basics : import name conflict error" [fichier]="test/Functional/Basics/ImportAdvanced/import_name_conflict_error.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_wrong_extension_error=([titre]="Basics : import wrong extension error" [fichier]="test/Functional/Basics/ImportAdvanced/import_wrong_extension_error.npy" [exitcode]="84" [output]="")
declare -A test_basics_import_relative_path_ok=([titre]="Basics : import relative path ok" [fichier]="test/Functional/Basics/ImportAdvanced/import_relative_path_ok.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_path_with_spaces_ok=([titre]="Basics : import path with spaces ok" [fichier]="test/Functional/Basics/ImportAdvanced/import_path_with_spaces_ok.npy" [exitcode]="0" [output]="42")
declare -A test_basics_import_module_with_error_error=([titre]="Basics : import module with internal error" [fichier]="test/Functional/Basics/ImportAdvanced/import_module_with_error_error.npy" [exitcode]="84" [output]="")
declare -A test_basics_open_inexistant_file=([titre]="Basics : compile an inexistant file" [fichier]="truc.npy" [exitcode]="84" [output]="")


# Casts
declare -A test_casts_int8_max_ok=([titre]="Casts : int8 max ok" [fichier]="test/Functional/Casts/int8_max_ok.npy" [exitcode]="0" [output]="127")
declare -A test_casts_int8_min_ok=([titre]="Casts : int8 min ok" [fichier]="test/Functional/Casts/int8_min_ok.npy" [exitcode]="0" [output]="-128")
declare -A test_casts_uint8_max_ok=([titre]="Casts : uint8 max ok" [fichier]="test/Functional/Casts/uint8_max_ok.npy" [exitcode]="0" [output]="255")
declare -A test_casts_int16_max_ok=([titre]="Casts : int16 max ok" [fichier]="test/Functional/Casts/int16_max_ok.npy" [exitcode]="0" [output]="32767")
declare -A test_casts_uint16_max_ok=([titre]="Casts : uint16 max ok" [fichier]="test/Functional/Casts/uint16_max_ok.npy" [exitcode]="0" [output]="65535")
declare -A test_casts_int32_max_ok=([titre]="Casts : int32 max ok" [fichier]="test/Functional/Casts/int32_max_ok.npy" [exitcode]="0" [output]="2147483647")
declare -A test_casts_uint32_max_ok=([titre]="Casts : uint32 max ok" [fichier]="test/Functional/Casts/uint32_max_ok.npy" [exitcode]="0" [output]="4294967295")
declare -A test_casts_int64_negative_ok=([titre]="Casts : int64 negative ok" [fichier]="test/Functional/Casts/int64_negative_ok.npy" [exitcode]="0" [output]="-42")
declare -A test_casts_uint64_positive_ok=([titre]="Casts : uint64 positive ok" [fichier]="test/Functional/Casts/uint64_positive_ok.npy" [exitcode]="0" [output]="42")
declare -A test_casts_char_A_ok=([titre]="Casts : char(65) prints A" [fichier]="test/Functional/Casts/char_A_ok.npy" [exitcode]="0" [output]="A")
declare -A test_casts_uchar_255_ok=([titre]="Casts : uchar max ok" [fichier]="test/Functional/Casts/uchar_255_ok.npy" [exitcode]="0" [output]="ÿ")
declare -A test_casts_string_from_char_list_ok=([titre]="Casts : string from [char(...)] ok" [fichier]="test/Functional/Casts/string_from_char_list_ok.npy" [exitcode]="0" [output]="Noopy")
# Casts (Out of range -> error 84)
declare -A test_casts_int8_overflow_error=([titre]="Casts : int8 overflow error" [fichier]="test/Functional/Casts/int8_overflow_error.npy" [exitcode]="0" [output]="-128")
declare -A test_casts_int8_underflow_error=([titre]="Casts : int8 underflow error" [fichier]="test/Functional/Casts/int8_underflow_error.npy" [exitcode]="0" [output]="127")
declare -A test_casts_uint8_overflow_error=([titre]="Casts : uint8 overflow error" [fichier]="test/Functional/Casts/uint8_overflow_error.npy" [exitcode]="0" [output]="0")
declare -A test_casts_uint8_negative_error=([titre]="Casts : uint8 negative error" [fichier]="test/Functional/Casts/uint8_negative_error.npy" [exitcode]="0" [output]="255")
declare -A test_casts_char_overflow_error=([titre]="Casts : char overflow error" [fichier]="test/Functional/Casts/char_overflow_error.npy" [exitcode]="0" [output]="")
declare -A test_casts_uint16_overflow_error=([titre]="Casts : uint16 overflow error" [fichier]="test/Functional/Casts/uint16_overflow_error.npy" [exitcode]="0" [output]="4464")
# Casts (Wrong type -> error 84)
declare -A test_casts_int8_bool_type_error=([titre]="Casts : int8(True) type error" [fichier]="test/Functional/Casts/int8_bool_type_error.npy" [exitcode]="84" [output]="")
declare -A test_casts_uint32_string_type_error=([titre]="Casts : uint32(\"Noopy\") type error" [fichier]="test/Functional/Casts/uint32_string_type_error.npy" [exitcode]="84" [output]="")
declare -A test_casts_char_list_type_error=([titre]="Casts : char([1,2]) type error" [fichier]="test/Functional/Casts/char_list_type_error.npy" [exitcode]="84" [output]="")

#Syntax
declare -A test_syntax_semicolon_ok=([titre]="Syntax : semilicon ok" [fichier]="test/Functional/Syntax/semicolon_ok.npy" [exitcode]="0" [output]="42")
declare -A test_syntax_semicolon_missing=([titre]="Syntax : missing semilicon" [fichier]="test/Functional/Syntax/semicolon_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_block_ok=([titre]="Syntax : Standard syntax block" [fichier]="test/Functional/Syntax/block_ok.npy" [exitcode]="0" [output]="42")
declare -A test_syntax_block_missing=([titre]="Syntax : Bad block syntax" [fichier]="test/Functional/Syntax/block_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_if_parentheses_missing=([titre]="Syntax : Missing parenthesis" [fichier]="test/Functional/Syntax/if_parentheses_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_while_parentheses_missing=([titre]="Syntax : While parenthesis missing" [fichier]="test/Functional/Syntax/while_parentheses_missing.npy" [exitcode]="84" [output]="")


# Syntax (Advanced)
declare -A test_syntax_for_parentheses_missing=([titre]="Syntax : for missing parentheses" [fichier]="test/Functional/Syntax/Advanced/for_parentheses_missing.npy" [exitcode]="84" [output]="")
declare -A test_syntax_for_header_missing_semicolons=([titre]="Syntax : for header missing semicolons" [fichier]="test/Functional/Syntax/Advanced/for_header_missing_semicolons.npy" [exitcode]="84" [output]="")
declare -A test_syntax_func_missing_name=([titre]="Syntax : func missing name" [fichier]="test/Functional/Syntax/Advanced/func_missing_name.npy" [exitcode]="84" [output]="")
declare -A test_syntax_func_missing_braces=([titre]="Syntax : func missing braces" [fichier]="test/Functional/Syntax/Advanced/func_missing_braces.npy" [exitcode]="84" [output]="")
declare -A test_syntax_func_malformed_arrow=([titre]="Syntax : func malformed arrow" [fichier]="test/Functional/Syntax/Advanced/func_malformed_arrow.npy" [exitcode]="84" [output]="")
declare -A test_syntax_func_param_missing_type=([titre]="Syntax : func param missing type" [fichier]="test/Functional/Syntax/Advanced/func_param_missing_type.npy" [exitcode]="0" [output]="42")
declare -A test_syntax_struct_field_missing_semicolon=([titre]="Syntax : struct field missing semicolon" [fichier]="test/Functional/Syntax/Advanced/struct_field_missing_semicolon.npy" [exitcode]="84" [output]="")
declare -A test_syntax_struct_field_missing_type=([titre]="Syntax : struct field missing type" [fichier]="test/Functional/Syntax/Advanced/struct_field_missing_type.npy" [exitcode]="84" [output]="")
declare -A test_syntax_struct_missing_closing_brace=([titre]="Syntax : struct missing closing brace" [fichier]="test/Functional/Syntax/Advanced/struct_missing_closing_brace.npy" [exitcode]="84" [output]="")
declare -A test_syntax_lambda_missing_arrow=([titre]="Syntax : lambda missing arrow" [fichier]="test/Functional/Syntax/Advanced/lambda_missing_arrow.npy" [exitcode]="84" [output]="")
declare -A test_syntax_lambda_trailing_comma=([titre]="Syntax : lambda trailing comma" [fichier]="test/Functional/Syntax/Advanced/lambda_trailing_comma.npy" [exitcode]="84" [output]="")
declare -A test_syntax_expr_missing_rhs=([titre]="Syntax : expr missing RHS" [fichier]="test/Functional/Syntax/Advanced/expr_missing_rhs.npy" [exitcode]="84" [output]="")
declare -A test_syntax_expr_unclosed_paren=([titre]="Syntax : expr unclosed parenthesis" [fichier]="test/Functional/Syntax/Advanced/expr_unclosed_paren.npy" [exitcode]="84" [output]="")
declare -A test_syntax_string_unclosed_quote=([titre]="Syntax : string unclosed quote" [fichier]="test/Functional/Syntax/Advanced/string_unclosed_quote.npy" [exitcode]="84" [output]="")

#Declaration
declare -A test_declarations_infer_int_ok=([titre]="Declarations : infer int ok" [fichier]="test/Functional/Declarations/infer_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_declarations_explicit_int_ok=([titre]="Declarations : explicit int ok" [fichier]="test/Functional/Declarations/explicit_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_declarations_infer_bool_ok=([titre]="Declarations : infer bool ok" [fichier]="test/Functional/Declarations/infer_bool_ok.npy" [exitcode]="0" [output]="True")
declare -A test_declarations_explicit_bool_ok=([titre]="Declarations : explicit bool ok" [fichier]="test/Functional/Declarations/explicit_bool_ok.npy" [exitcode]="0" [output]="False")
declare -A test_declarations_void_declaration_ok=([titre]="Declarations : void declaration ok" [fichier]="test/Functional/Declarations/void_declaration_ok.npy" [exitcode]="0" [output]="42")
declare -A test_declarations_explicit_init_type_mismatch=([titre]="Declarations : explicit init type mismatch" [fichier]="test/Functional/Declarations/explicit_init_type_mismatch.npy" [exitcode]="84" [output]="")
declare -A test_declarations_infer_then_assign_wrong_type=([titre]="Declarations : infer then assign wrong type" [fichier]="test/Functional/Declarations/infer_then_assign_wrong_type.npy" [exitcode]="0" [output]="True")
declare -A test_declarations_explicit_then_assign_wrong_type=([titre]="Declarations : explicit then assign wrong type" [fichier]="test/Functional/Declarations/explicit_then_assign_wrong_type.npy" [exitcode]="84" [output]="")
declare -A test_declarations_bool_then_assign_int=([titre]="Declarations : bool then assign int" [fichier]="test/Functional/Declarations/bool_then_assign_int.npy" [exitcode]="0" [output]="1")

#Asign
declare -A test_assign_reassign_int_ok=([titre]="Assign : reassign int ok" [fichier]="test/Functional/Assign/reassign_int_ok.npy" [exitcode]="0" [output]="2")
declare -A test_assign_reassign_chain_ok=([titre]="Assign : reassign chain ok" [fichier]="test/Functional/Assign/reassign_chain_ok.npy" [exitcode]="0" [output]="42")
declare -A test_assign_for_header_init_update_ok=([titre]="Assign : for header init/update ok" [fichier]="test/Functional/Assign/for_header_init_update_ok.npy" [exitcode]="0" [output]="6")
declare -A test_assign_use_undeclared_var=([titre]="Assign : use undeclared var" [fichier]="test/Functional/Assign/use_undeclared_var.npy" [exitcode]="84" [output]="")
declare -A test_assign_reassign_type_mismatch=([titre]="Assign : reassign type mismatch" [fichier]="test/Functional/Assign/reassign_type_mismatch.npy" [exitcode]="0" [output]="True")
declare -A test_assign_for_header_type_mismatch=([titre]="Assign : for header type mismatch" [fichier]="test/Functional/Assign/for_header_type_mismatch.npy" [exitcode]="84" [output]="")

#Literals
declare -A test_literals_int_positive_ok=([titre]="Literals : int positive" [fichier]="test/Functional/Literals/int_positive_ok.npy" [exitcode]="0" [output]="42")
declare -A test_literals_int_negative_ok=([titre]="Literals : int negative" [fichier]="test/Functional/Literals/int_negative_ok.npy" [exitcode]="0" [output]="-42")
declare -A test_literals_bool_true_ok=([titre]="Literals : bool True" [fichier]="test/Functional/Literals/bool_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_literals_bool_false_ok=([titre]="Literals : bool False" [fichier]="test/Functional/Literals/bool_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_literals_void_value_ok=([titre]="Literals : void literal value" [fichier]="test/Functional/Literals/void_value_ok.npy" [exitcode]="84" [output]="")
declare -A test_literals_list_int_ok=([titre]="Literals : list of int" [fichier]="test/Functional/Literals/list_int_ok.npy" [exitcode]="0" [output]="2")
declare -A test_literals_list_nested_ok=([titre]="Literals : nested list" [fichier]="test/Functional/Literals/list_nested_ok.npy" [exitcode]="0" [output]="1")
declare -A test_literals_string_sugar_ok=([titre]="Literals : string sugar" [fichier]="test/Functional/Literals/string_sugar_ok.npy" [exitcode]="0" [output]="Noopy")

#Strings
declare -A test_strings_print_literal_ok=([titre]="Strings : print literal" [fichier]="test/Functional/Strings/print_literal_ok.npy" [exitcode]="0" [output]="Hello")
declare -A test_strings_assign_to_char_list_ok=([titre]="Strings : assign to [char]" [fichier]="test/Functional/Strings/assign_to_char_list_ok.npy" [exitcode]="0" [output]="World")
declare -A test_strings_pass_to_function_ok=([titre]="Strings : pass to function [char]" [fichier]="test/Functional/Strings/pass_to_function_ok.npy" [exitcode]="0" [output]="Noopy")
declare -A test_strings_assign_string_to_int_list=([titre]="Strings : string to [int] error" [fichier]="test/Functional/Strings/assign_string_to_int_list.npy" [exitcode]="0" [output]="123")
declare -A test_strings_pass_string_to_wrong_param=([titre]="Strings : pass string to wrong param" [fichier]="test/Functional/Strings/pass_string_to_wrong_param.npy" [exitcode]="84" [output]="")

#Lists
declare -A test_lists_literal_and_index_ok=([titre]="Lists : literal + index" [fichier]="test/Functional/Lists/literal_and_index_ok.npy" [exitcode]="0" [output]="10")
declare -A test_lists_index_zero_based_ok=([titre]="Lists : index 0-based" [fichier]="test/Functional/Lists/index_zero_based_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lists_assign_element_to_var_ok=([titre]="Lists : element to var" [fichier]="test/Functional/Lists/assign_element_to_var_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lists_nested_index_ok=([titre]="Lists : nested index" [fichier]="test/Functional/Lists/nested_index_ok.npy" [exitcode]="0" [output]="1")
declare -A test_lists_inconsistent_literal_type=([titre]="Lists : inconsistent literal type" [fichier]="test/Functional/Lists/inconsistent_literal_type.npy" [exitcode]="84" [output]="")
declare -A test_lists_nested_inconsistent_type=([titre]="Lists : nested inconsistent type" [fichier]="test/Functional/Lists/nested_inconsistent_type.npy" [exitcode]="84" [output]="")
declare -A test_lists_index_out_of_bounds=([titre]="Lists : index out of bounds" [fichier]="test/Functional/Lists/index_out_of_bounds.npy" [exitcode]="84" [output]="")
# Lists (Advanced)
declare -A test_lists_index_bool_error=([titre]="Lists : index with bool error" [fichier]="test/Functional/Lists/Advanced/index_bool_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_index_string_error=([titre]="Lists : index with string error" [fichier]="test/Functional/Lists/Advanced/index_string_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_index_negative_error=([titre]="Lists : negative index error" [fichier]="test/Functional/Lists/Advanced/index_negative_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_index_on_int_error=([titre]="Lists : index on int error" [fichier]="test/Functional/Lists/Advanced/index_on_int_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_index_on_bool_error=([titre]="Lists : index on bool error" [fichier]="test/Functional/Lists/Advanced/index_on_bool_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_index_on_struct_error=([titre]="Lists : index on struct error" [fichier]="test/Functional/Lists/Advanced/index_on_struct_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_assign_element_ok=([titre]="Lists : assign element ok" [fichier]="test/Functional/Lists/Advanced/assign_element_ok.npy" [exitcode]="0" [output]="42")
declare -A test_lists_assign_element_type_mismatch_error=([titre]="Lists : assign element type mismatch" [fichier]="test/Functional/Lists/Advanced/assign_element_type_mismatch_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_assign_element_index_bool_error=([titre]="Lists : assign element index bool error" [fichier]="test/Functional/Lists/Advanced/assign_element_index_bool_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_assign_element_out_of_bounds_error=([titre]="Lists : assign element out of bounds error" [fichier]="test/Functional/Lists/Advanced/assign_element_out_of_bounds_error.npy" [exitcode]="84" [output]="")
declare -A test_lists_pass_to_function_sum_ok=([titre]="Lists : pass [int] to function sum ok" [fichier]="test/Functional/Lists/Advanced/pass_to_function_sum_ok.npy" [exitcode]="0" [output]="6")
declare -A test_lists_list_of_structs_ok=([titre]="Lists : list of structs ok" [fichier]="test/Functional/Lists/Advanced/list_of_structs_ok.npy" [exitcode]="0" [output]="7")
declare -A test_lists_struct_with_list_field_ok=([titre]="Lists : struct with list field ok" [fichier]="test/Functional/Lists/Advanced/struct_with_list_field_ok.npy" [exitcode]="0" [output]="3")
declare -A test_lists_struct_with_list_field_type_error=([titre]="Lists : struct with list field type error" [fichier]="test/Functional/Lists/Advanced/struct_with_list_field_type_error.npy" [exitcode]="84" [output]="")

#Expression
declare -A test_expr_add_sub_mul_ok=([titre]="Expressions : add sub mul" [fichier]="test/Functional/Expressions/add_sub_mul_ok.npy" [exitcode]="0" [output]="14")
declare -A test_expr_div_operator_ok=([titre]="Expressions : div operator" [fichier]="test/Functional/Expressions/div_operator_ok.npy" [exitcode]="0" [output]="3")
declare -A test_expr_div_keyword_ok=([titre]="Expressions : div keyword" [fichier]="test/Functional/Expressions/div_keyword_ok.npy" [exitcode]="0" [output]="3")
declare -A test_expr_mod_operator_ok=([titre]="Expressions : mod operator" [fichier]="test/Functional/Expressions/mod_operator_ok.npy" [exitcode]="0" [output]="1")
declare -A test_expr_mod_keyword_ok=([titre]="Expressions : mod keyword" [fichier]="test/Functional/Expressions/mod_keyword_ok.npy" [exitcode]="0" [output]="1")
declare -A test_expr_operator_precedence_ok=([titre]="Expressions : operator precedence" [fichier]="test/Functional/Expressions/operator_precedence_ok.npy" [exitcode]="0" [output]="14")
declare -A test_expr_division_by_zero=([titre]="Expressions : division by zero" [fichier]="test/Functional/Expressions/division_by_zero.npy" [exitcode]="84" [output]="")
declare -A test_expr_modulo_by_zero=([titre]="Expressions : modulo by zero" [fichier]="test/Functional/Expressions/modulo_by_zero.npy" [exitcode]="84" [output]="")

#Comparaison
declare -A test_cmp_eq_true_ok=([titre]="Comparisons : == true" [fichier]="test/Functional/Comparisons/eq_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_eq_false_ok=([titre]="Comparisons : == false" [fichier]="test/Functional/Comparisons/eq_false_ok.npy" [exitcode]="0" [output]="False")
declare -A test_cmp_neq_true_ok=([titre]="Comparisons : != true" [fichier]="test/Functional/Comparisons/neq_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_lt_ok=([titre]="Comparisons : <" [fichier]="test/Functional/Comparisons/lt_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_gt_ok=([titre]="Comparisons : >" [fichier]="test/Functional/Comparisons/gt_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_lte_ok=([titre]="Comparisons : <=" [fichier]="test/Functional/Comparisons/lte_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_gte_ok=([titre]="Comparisons : >=" [fichier]="test/Functional/Comparisons/gte_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_bool_eq_ok=([titre]="Comparisons : bool ==" [fichier]="test/Functional/Comparisons/bool_eq_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_mismatched_types_eq=([titre]="Comparisons : type mismatch ==" [fichier]="test/Functional/Comparisons/mismatched_types_eq.npy" [exitcode]="" [output]="False")
declare -A test_cmp_list_eq_disallowed=([titre]="Comparisons : list == disallowed" [fichier]="test/Functional/Comparisons/list_eq_disallowed.npy" [exitcode]="0" [output]="True")
# Comparisons (Advanced)
declare -A test_cmp_bool_neq_ok=([titre]="Comparisons : bool != ok" [fichier]="test/Functional/Comparisons/Advanced/bool_neq_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_bool_lt_disallowed=([titre]="Comparisons : bool < disallowed" [fichier]="test/Functional/Comparisons/Advanced/bool_lt_disallowed.npy" [exitcode]="84" [output]="")
declare -A test_cmp_bool_gte_disallowed=([titre]="Comparisons : bool >= disallowed" [fichier]="test/Functional/Comparisons/Advanced/bool_gte_disallowed.npy" [exitcode]="84" [output]="")
declare -A test_cmp_void_eq_disallowed=([titre]="Comparisons : void == disallowed" [fichier]="test/Functional/Comparisons/Advanced/void_eq_disallowed.npy" [exitcode]="0" [output]=$'1\n1\nTrue')
declare -A test_cmp_void_neq_disallowed=([titre]="Comparisons : void != disallowed" [fichier]="test/Functional/Comparisons/Advanced/void_neq_disallowed.npy" [exitcode]="0" [output]=$'1\nFalse')
declare -A test_cmp_struct_eq_disallowed=([titre]="Comparisons : struct == disallowed" [fichier]="test/Functional/Comparisons/Advanced/struct_eq_disallowed.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_struct_neq_disallowed=([titre]="Comparisons : struct != disallowed" [fichier]="test/Functional/Comparisons/Advanced/struct_neq_disallowed.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_list_lt_disallowed=([titre]="Comparisons : list < disallowed" [fichier]="test/Functional/Comparisons/Advanced/list_lt_disallowed.npy" [exitcode]="84" [output]="")
declare -A test_cmp_list_gte_disallowed=([titre]="Comparisons : list >= disallowed" [fichier]="test/Functional/Comparisons/Advanced/list_gte_disallowed.npy" [exitcode]="84" [output]="")
declare -A test_cmp_list_neq_disallowed=([titre]="Comparisons : list != disallowed" [fichier]="test/Functional/Comparisons/Advanced/list_neq_disallowed.npy" [exitcode]="0" [output]="False")

# Comparisons (Triple Eq ===)
declare -A test_cmp_strict_eq_int_true_ok=([titre]="Comparisons : === int true" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_int_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_strict_eq_int_false_ok=([titre]="Comparisons : === int false" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_int_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_cmp_strict_eq_bool_true_ok=([titre]="Comparisons : === bool true" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_bool_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_strict_eq_bool_false_ok=([titre]="Comparisons : === bool false" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_bool_false_ok.npy" [exitcode]="0" [output]="False")

declare -A test_cmp_strict_eq_string_true_ok=([titre]="Comparisons : === string true" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_string_true_ok.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_strict_eq_string_false_ok=([titre]="Comparisons : === string false" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_string_false_ok.npy" [exitcode]="0" [output]="False")

# Errors (proposés) : mismatch types / void / list / struct
declare -A test_cmp_strict_eq_mismatched_types_error=([titre]="Comparisons : === mismatched types error" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_mismatched_types_error.npy" [exitcode]="0" [output]="False")
declare -A test_cmp_strict_eq_list_disallowed_error=([titre]="Comparisons : === list disallowed" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_list_disallowed_error.npy" [exitcode]="0" [output]="True")
declare -A test_cmp_strict_eq_struct_disallowed_error=([titre]="Comparisons : === struct disallowed" [fichier]="test/Functional/Comparisons/Advanced/strict_eq_struct_disallowed_error.npy" [exitcode]="0" [output]="True")


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
# Functions (Advanced)
declare -A test_functions_recursion_factorial_ok=([titre]="Functions Adv : recursion factorial ok" [fichier]="test/Functional/Functions/Advanced/recursion_factorial_ok.npy" [exitcode]="0" [output]="120")
declare -A test_functions_shadowing_local_over_global_ok=([titre]="Functions Adv : shadowing local over global ok" [fichier]="test/Functional/Functions/Advanced/shadowing_local_over_global_ok.npy" [exitcode]="0" [output]="2")
declare -A test_functions_return_list_ok=([titre]="Functions Adv : return list ok" [fichier]="test/Functional/Functions/Advanced/return_list_ok.npy" [exitcode]="0" [output]="42")
declare -A test_functions_pass_struct_and_return_int_ok=([titre]="Functions Adv : pass struct and return int ok" [fichier]="test/Functional/Functions/Advanced/pass_struct_and_return_int_ok.npy" [exitcode]="0" [output]="7")
declare -A test_functions_return_struct_access_field_ok=([titre]="Functions Adv : return struct then access field ok" [fichier]="test/Functional/Functions/Advanced/return_struct_access_field_ok.npy" [exitcode]="0" [output]="9")
declare -A test_functions_void_used_in_expression_error=([titre]="Functions Adv : void used in expression error" [fichier]="test/Functional/Functions/Advanced/void_used_in_expression_error.npy" [exitcode]="84" [output]="")
declare -A test_functions_multi_path_returns_ok=([titre]="Functions Adv : multi-path returns ok" [fichier]="test/Functional/Functions/Advanced/multi_path_returns_ok.npy" [exitcode]="0" [output]="10")
declare -A test_functions_call_undefined_function_error=([titre]="Functions Adv : call undefined function error" [fichier]="test/Functional/Functions/Advanced/call_undefined_function_error.npy" [exitcode]="84" [output]="")


#Lambda
declare -A test_lambdas_basic_multiply_ok=([titre]="Lambdas : basic multiply" [fichier]="test/Functional/Lambdas/basic_multiply_ok.npy" [exitcode]="0" [output]="12")
declare -A test_lambdas_capture_var_ok=([titre]="Lambdas : capture var" [fichier]="test/Functional/Lambdas/capture_var_ok.npy" [exitcode]="0" [output]="42")
declare -A test_lambdas_return_used_in_expr_ok=([titre]="Lambdas : used in expression" [fichier]="test/Functional/Lambdas/used_in_expression_ok.npy" [exitcode]="0" [output]="45")
declare -A test_lambdas_nested_call_ok=([titre]="Lambdas : nested call" [fichier]="test/Functional/Lambdas/nested_call_ok.npy" [exitcode]="0" [output]="16")
declare -A test_lambdas_wrong_arity=([titre]="Lambdas : wrong arity" [fichier]="test/Functional/Lambdas/wrong_arity.npy" [exitcode]="84" [output]="")
declare -A test_lambdas_wrong_arg_type=([titre]="Lambdas : wrong arg type" [fichier]="test/Functional/Lambdas/wrong_arg_type.npy" [exitcode]="84" [output]="")
declare -A test_lambdas_non_callable_used_as_func=([titre]="Lambdas : non callable used as func" [fichier]="test/Functional/Lambdas/non_callable_used_as_func.npy" [exitcode]="84" [output]="")
# Lambdas (Advanced)
declare -A test_lambdas_hof_pass_as_param_ok=([titre]="Lambdas Adv : pass lambda as param ok" [fichier]="test/Functional/Lambdas/Advanced/hof_pass_as_param_ok.npy" [exitcode]="0" [output]="12")
declare -A test_lambdas_return_lambda_ok=([titre]="Lambdas Adv : return lambda ok" [fichier]="test/Functional/Lambdas/Advanced/return_lambda_ok.npy" [exitcode]="0" [output]="15")
# Capture semantics (keep only the one matching your implementation)
declare -A test_lambdas_capture_modified_after_ref_ok=([titre]="Lambdas Adv : capture modified after (by ref)" [fichier]="test/Functional/Lambdas/Advanced/capture_modified_after_ref_ok.npy" [exitcode]="0" [output]="20")
declare -A test_lambdas_capture_modified_after_val_ok=([titre]="Lambdas Adv : capture modified after (by value)" [fichier]="test/Functional/Lambdas/Advanced/capture_modified_after_val_ok.npy" [exitcode]="0" [output]="11")
declare -A test_lambdas_capture_out_of_scope_error=([titre]="Lambdas Adv : capture out of scope error" [fichier]="test/Functional/Lambdas/Advanced/capture_out_of_scope_error.npy" [exitcode]="84" [output]="")
declare -A test_lambdas_lambda_in_list_ok=([titre]="Lambdas Adv : lambda stored in list ok" [fichier]="test/Functional/Lambdas/Advanced/lambda_in_list_ok.npy" [exitcode]="0" [output]="9")
declare -A test_lambdas_lambda_in_list_type_error=([titre]="Lambdas Adv : lambda list element type error" [fichier]="test/Functional/Lambdas/Advanced/lambda_in_list_type_error.npy" [exitcode]="84" [output]="")


#Struct
declare -A test_structs_basic_new_access_ok=([titre]="Structs : basic new + access" [fichier]="test/Functional/Structs/basic_new_access_ok.npy" [exitcode]="0" [output]="42")
declare -A test_structs_assign_struct_to_var_ok=([titre]="Structs : assign struct to var" [fichier]="test/Functional/Structs/assign_struct_to_var_ok.npy" [exitcode]="0" [output]="7")
declare -A test_structs_nested_struct_ok=([titre]="Structs : nested struct" [fichier]="test/Functional/Structs/nested_struct_ok.npy" [exitcode]="0" [output]="9")
declare -A test_structs_multiple_fields_ok=([titre]="Structs : multiple fields" [fichier]="test/Functional/Structs/multiple_fields_ok.npy" [exitcode]="0" [output]="3")
declare -A test_structs_missing_field_error=([titre]="Structs : missing field" [fichier]="test/Functional/Structs/missing_field_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_extra_field_error=([titre]="Structs : extra field" [fichier]="test/Functional/Structs/extra_field_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_wrong_field_type_error=([titre]="Structs : wrong field type" [fichier]="test/Functional/Structs/wrong_field_type_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_unknown_field_access_error=([titre]="Structs : unknown field access" [fichier]="test/Functional/Structs/unknown_field_access_error.npy" [exitcode]="84" [output]="")
# Structs (Advanced)
declare -A test_structs_field_assign_ok=([titre]="Structs Adv : field assign ok" [fichier]="test/Functional/Structs/Advanced/field_assign_ok.npy" [exitcode]="0" [output]="10")
declare -A test_structs_field_assign_type_error=([titre]="Structs Adv : field assign type error" [fichier]="test/Functional/Structs/Advanced/field_assign_type_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_field_assign_unknown_field_error=([titre]="Structs Adv : field assign unknown field error" [fichier]="test/Functional/Structs/Advanced/field_assign_unknown_field_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_nested_field_assign_ok=([titre]="Structs Adv : nested field assign ok" [fichier]="test/Functional/Structs/Advanced/nested_field_assign_ok.npy" [exitcode]="0" [output]="42")
declare -A test_structs_nested_field_assign_type_error=([titre]="Structs Adv : nested field assign type error" [fichier]="test/Functional/Structs/Advanced/nested_field_assign_type_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_pass_struct_mutate_and_read_ok=([titre]="Structs Adv : pass struct mutate and read ok" [fichier]="test/Functional/Structs/Advanced/pass_struct_mutate_and_read_ok.npy" [exitcode]="0" [output]="99")
declare -A test_structs_return_struct_ok=([titre]="Structs Adv : return struct ok" [fichier]="test/Functional/Structs/Advanced/return_struct_ok.npy" [exitcode]="0" [output]="7")
declare -A test_structs_list_of_structs_index_ok=([titre]="Structs Adv : list of structs index ok" [fichier]="test/Functional/Structs/Advanced/list_of_structs_index_ok.npy" [exitcode]="0" [output]="3")
declare -A test_structs_list_of_structs_type_error=([titre]="Structs Adv : list of structs type error" [fichier]="test/Functional/Structs/Advanced/list_of_structs_type_error.npy" [exitcode]="84" [output]="")
declare -A test_structs_copy_vs_ref_observe_ok=([titre]="Structs Adv : copy vs ref observe" [fichier]="test/Functional/Structs/Advanced/copy_vs_ref_observe_ok.npy" [exitcode]="0" [output]="2")


#Builtins
declare -A test_builtins_print_int_ok=([titre]="Builtins : print int" [fichier]="test/Functional/Builtins/print_int_ok.npy" [exitcode]="0" [output]="42")
declare -A test_builtins_print_bool_ok=([titre]="Builtins : print bool" [fichier]="test/Functional/Builtins/print_bool_ok.npy" [exitcode]="0" [output]="True")
declare -A test_builtins_print_string_ok=([titre]="Builtins : print string" [fichier]="test/Functional/Builtins/print_string_ok.npy" [exitcode]="0" [output]="Noopy")
declare -A test_builtins_print_list_int_ok=([titre]="Builtins : print [int] (observe)" [fichier]="test/Functional/Builtins/print_list_int_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_print_nested_list_ok=([titre]="Builtins : print [[int]] (observe)" [fichier]="test/Functional/Builtins/print_nested_list_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_print_struct_ok=([titre]="Builtins : print struct (observe)" [fichier]="test/Functional/Builtins/print_struct_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_print_void_ok=([titre]="Builtins : print void (observe)" [fichier]="test/Functional/Builtins/print_void_ok.npy" [exitcode]="84" [output]="")
# Builtins (List + Exit)
declare -A test_builtins_exit_0_ok=([titre]="Builtins : exit(0) ok" [fichier]="test/Functional/Builtins/Advanced/exit_0_ok.npy" [exitcode]="0" [output]="")
declare -A test_builtins_exit_84_ok=([titre]="Builtins : exit(84) ok" [fichier]="test/Functional/Builtins/Advanced/exit_84_ok.npy" [exitcode]="84" [output]="")
declare -A test_builtins_exit_wrong_type_error=([titre]="Builtins : exit(True) type error" [fichier]="test/Functional/Builtins/Advanced/exit_wrong_type_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_head_int_list_ok=([titre]="Builtins : head([int]) ok" [fichier]="test/Functional/Builtins/Advanced/head_int_list_ok.npy" [exitcode]="0" [output]="10")
declare -A test_builtins_head_empty_list_error=([titre]="Builtins : head([]) error" [fichier]="test/Functional/Builtins/Advanced/head_empty_list_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_head_non_list_error=([titre]="Builtins : head(non-list) error" [fichier]="test/Functional/Builtins/Advanced/head_non_list_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_tail_then_head_ok=([titre]="Builtins : tail then head ok" [fichier]="test/Functional/Builtins/Advanced/tail_then_head_ok.npy" [exitcode]="0" [output]="20")
declare -A test_builtins_tail_empty_list_error=([titre]="Builtins : tail([]) error" [fichier]="test/Functional/Builtins/Advanced/tail_empty_list_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_tail_singleton_then_head_error=([titre]="Builtins : head(tail([x])) error" [fichier]="test/Functional/Builtins/Advanced/tail_singleton_then_head_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_tail_non_list_error=([titre]="Builtins : tail(non-list) error" [fichier]="test/Functional/Builtins/Advanced/tail_non_list_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_nth_ok=([titre]="Builtins : nth ok" [fichier]="test/Functional/Builtins/Advanced/nth_ok.npy" [exitcode]="0" [output]="30")
declare -A test_builtins_nth_out_of_bounds_error=([titre]="Builtins : nth out of bounds error" [fichier]="test/Functional/Builtins/Advanced/nth_out_of_bounds_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_nth_negative_index_error=([titre]="Builtins : nth negative index error" [fichier]="test/Functional/Builtins/Advanced/nth_negative_index_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_nth_non_int_index_error=([titre]="Builtins : nth non-int index error" [fichier]="test/Functional/Builtins/Advanced/nth_non_int_index_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_nth_empty_list_error=([titre]="Builtins : nth on empty list error" [fichier]="test/Functional/Builtins/Advanced/nth_empty_list_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_nth_non_list_error=([titre]="Builtins : nth(non-list, i) error" [fichier]="test/Functional/Builtins/Advanced/nth_non_list_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_cons_int_ok=([titre]="Builtins : cons(int, [int]) ok" [fichier]="test/Functional/Builtins/Advanced/cons_int_ok.npy" [exitcode]="0" [output]="5")
declare -A test_builtins_cons_type_mismatch_error=([titre]="Builtins : cons type mismatch error" [fichier]="test/Functional/Builtins/Advanced/cons_type_mismatch_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_cons_wrong_arity_error=([titre]="Builtins : cons wrong arity error" [fichier]="test/Functional/Builtins/Advanced/cons_wrong_arity_error.npy" [exitcode]="84" [output]="")
declare -A test_builtins_cons_non_list_second_arg_error=([titre]="Builtins : cons second arg non-list error" [fichier]="test/Functional/Builtins/Advanced/cons_non_list_second_arg_error.npy" [exitcode]="84" [output]="")

# Print format (non-observe)
declare -A test_print_expr_ok=([titre]="Print : expression format ok" [fichier]="test/Functional/Print/expr_ok.npy" [exitcode]="0" [output]="14")
declare -A test_print_list_int_format_ok=([titre]="Print : [int] format ok" [fichier]="test/Functional/Print/list_int_format_ok.npy" [exitcode]="0" [output]="[1, 2, 3]")
declare -A test_print_nested_list_format_ok=([titre]="Print : [[int]] format ok" [fichier]="test/Functional/Print/nested_list_format_ok.npy" [exitcode]="0" [output]="[[1, 2], [3]]")
declare -A test_print_struct_format_ok=([titre]="Print : struct format ok" [fichier]="test/Functional/Print/struct_format_ok.npy" [exitcode]="0" [output]="{1, 2}")
declare -A test_print_void_no_output_ok=([titre]="Print : void prints nothing (no newline) ok" [fichier]="test/Functional/Print/void_no_output_ok.npy" [exitcode]="0" [output]=$'0\n42')

# Syntactic sugar
declare -A test_sugar_postfix_increment_value_ok=([titre]="Sugar : postfix i++ value ok" [fichier]="test/Functional/Sugar/postfix_increment_value_ok.npy" [exitcode]="0" [output]="2")
declare -A test_sugar_prefix_increment_value_ok=([titre]="Sugar : prefix ++i value ok" [fichier]="test/Functional/Sugar/prefix_increment_value_ok.npy" [exitcode]="0" [output]="3")
declare -A test_sugar_postfix_decrement_value_ok=([titre]="Sugar : postfix i-- value ok" [fichier]="test/Functional/Sugar/postfix_decrement_value_ok.npy" [exitcode]="0" [output]="1")
declare -A test_sugar_prefix_decrement_value_ok=([titre]="Sugar : prefix --i value ok" [fichier]="test/Functional/Sugar/prefix_decrement_value_ok.npy" [exitcode]="0" [output]="0")
declare -A test_sugar_inc_in_expression_postfix_ok=([titre]="Sugar : i++ in expression ok" [fichier]="test/Functional/Sugar/inc_in_expression_postfix_ok.npy" [exitcode]="0" [output]="3")
declare -A test_sugar_inc_in_expression_prefix_ok=([titre]="Sugar : ++i in expression ok" [fichier]="test/Functional/Sugar/inc_in_expression_prefix_ok.npy" [exitcode]="0" [output]="4")
declare -A test_sugar_plus_equals_ok=([titre]="Sugar : += ok" [fichier]="test/Functional/Sugar/plus_equals_ok.npy" [exitcode]="0" [output]="12")
declare -A test_sugar_minus_equals_ok=([titre]="Sugar : -= ok" [fichier]="test/Functional/Sugar/minus_equals_ok.npy" [exitcode]="0" [output]="7")
declare -A test_sugar_times_equals_ok=([titre]="Sugar : *= ok" [fichier]="test/Functional/Sugar/times_equals_ok.npy" [exitcode]="0" [output]="20")
declare -A test_sugar_div_equals_ok=([titre]="Sugar : /= ok" [fichier]="test/Functional/Sugar/div_equals_ok.npy" [exitcode]="0" [output]="5")
declare -A test_sugar_mod_equals_ok=([titre]="Sugar : %= ok" [fichier]="test/Functional/Sugar/mod_equals_ok.npy" [exitcode]="0" [output]="1")
declare -A test_sugar_for_with_i_pp_ok=([titre]="Sugar : for with i++ ok" [fichier]="test/Functional/Sugar/for_with_i_pp_ok.npy" [exitcode]="0" [output]="3")
declare -A test_sugar_for_with_pp_i_ok=([titre]="Sugar : for with ++i ok" [fichier]="test/Functional/Sugar/for_with_pp_i_ok.npy" [exitcode]="0" [output]="3")
# Sugar errors (type)
declare -A test_sugar_inc_on_bool_error=([titre]="Sugar : True++ type error" [fichier]="test/Functional/Sugar/inc_on_bool_error.npy" [exitcode]="84" [output]="")
declare -A test_sugar_plus_equals_on_bool_error=([titre]="Sugar : bool += int type error" [fichier]="test/Functional/Sugar/plus_equals_on_bool_error.npy" [exitcode]="84" [output]="")
declare -A test_sugar_inc_on_list_error=([titre]="Sugar : list++ type error" [fichier]="test/Functional/Sugar/inc_on_list_error.npy" [exitcode]="84" [output]="")
declare -A test_sugar_plus_equals_on_struct_error=([titre]="Sugar : struct += int type error" [fichier]="test/Functional/Sugar/plus_equals_on_struct_error.npy" [exitcode]="84" [output]="")
# Sugar errors (zero)
declare -A test_sugar_div_equals_by_zero_error=([titre]="Sugar : /= by zero error" [fichier]="test/Functional/Sugar/div_equals_by_zero_error.npy" [exitcode]="84" [output]="")
declare -A test_sugar_mod_equals_by_zero_error=([titre]="Sugar : %= by zero error" [fichier]="test/Functional/Sugar/mod_equals_by_zero_error.npy" [exitcode]="84" [output]="")

# Integration (Combinatorial)
declare -A test_integ_import_struct_function_lambda_ok=([titre]="Integration : import + struct + function + lambda ok" [fichier]="test/Functional/Integration/import_struct_function_lambda_ok/main.npy" [exitcode]="0" [output]="15")
declare -A test_integ_loops_lists_builtins_ok=([titre]="Integration : loops + lists + builtins ok" [fichier]="test/Functional/Integration/loops_lists_builtins_ok.npy" [exitcode]="0" [output]="6")
declare -A test_integ_type_inference_chain_ok=([titre]="Integration : type inference chain ok" [fichier]="test/Functional/Integration/type_inference_chain_ok.npy" [exitcode]="0" [output]="3")
declare -A test_integ_lambda_closure_in_struct_ok=([titre]="Integration : lambda closure in struct ok" [fichier]="test/Functional/Integration/lambda_closure_in_struct_ok.npy" [exitcode]="0" [output]="12")
declare -A test_integ_import_transitive_then_call_error=([titre]="Integration : transitive import then call error" [fichier]="test/Functional/Integration/import_transitive_then_call_error/main.npy" [exitcode]="84" [output]="")
declare -A test_integ_multiple_errors_same_file_error=([titre]="Integration : multiple errors in same file" [fichier]="test/Functional/Integration/multiple_errors_same_file_error.npy" [exitcode]="84" [output]="")


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
        # Basics
        test_basics_import_simple
        test_basics_import_function
        test_basics_import_multiple
        test_basics_import_missing_file
        test_basics_import_symbol_visibility
        test_basics_import_unused
        test_basics_open_inexistant_file

        # Basics (Import Advanced)
        test_basics_import_duplicate_ok
        test_basics_import_circular_error
        test_basics_import_transitive_visibility_error
        test_basics_import_name_conflict_error
        test_basics_import_wrong_extension_error
        test_basics_import_relative_path_ok
        test_basics_import_path_with_spaces_ok
        test_basics_import_module_with_error_error

        # Casts
        test_casts_int8_max_ok
        test_casts_int8_min_ok
        test_casts_uint8_max_ok
        test_casts_int16_max_ok
        test_casts_uint16_max_ok
        test_casts_int32_max_ok
        test_casts_uint32_max_ok
        test_casts_int64_negative_ok
        test_casts_uint64_positive_ok
        test_casts_char_A_ok
        test_casts_uchar_255_ok
        test_casts_string_from_char_list_ok
        test_casts_int8_overflow_error
        test_casts_int8_underflow_error
        test_casts_uint8_overflow_error
        test_casts_uint8_negative_error
        test_casts_char_overflow_error
        test_casts_uint16_overflow_error
        test_casts_int8_bool_type_error
        test_casts_uint32_string_type_error
        test_casts_char_list_type_error

        # Syntax
        test_syntax_semicolon_ok
        test_syntax_semicolon_missing
        test_syntax_block_ok
        test_syntax_block_missing
        test_syntax_if_parentheses_missing
        test_syntax_while_parentheses_missing

        # Syntax (Advanced)
        test_syntax_for_parentheses_missing
        test_syntax_for_header_missing_semicolons
        test_syntax_func_missing_name
        test_syntax_func_missing_braces
        test_syntax_func_malformed_arrow
        test_syntax_func_param_missing_type
        test_syntax_struct_field_missing_semicolon
        test_syntax_struct_field_missing_type
        test_syntax_struct_missing_closing_brace
        test_syntax_lambda_missing_arrow
        test_syntax_lambda_trailing_comma
        test_syntax_expr_missing_rhs
        test_syntax_expr_unclosed_paren
        test_syntax_string_unclosed_quote

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

        # Lists (Advanced)
        test_lists_index_bool_error
        test_lists_index_string_error
        test_lists_index_negative_error
        test_lists_index_on_int_error
        test_lists_index_on_bool_error
        test_lists_index_on_struct_error
        test_lists_assign_element_ok
        test_lists_assign_element_type_mismatch_error
        test_lists_assign_element_index_bool_error
        test_lists_assign_element_out_of_bounds_error
        test_lists_pass_to_function_sum_ok
        test_lists_list_of_structs_ok
        test_lists_struct_with_list_field_ok

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

        test_cmp_strict_eq_int_true_ok
        test_cmp_strict_eq_int_false_ok
        test_cmp_strict_eq_bool_true_ok
        test_cmp_strict_eq_bool_false_ok
        test_cmp_strict_eq_string_true_ok
        test_cmp_strict_eq_string_false_ok
        test_cmp_strict_eq_mismatched_types_error
        test_cmp_strict_eq_list_disallowed_error
        test_cmp_strict_eq_struct_disallowed_error

        # Comparisons (Advanced)
        test_cmp_bool_neq_ok
        test_cmp_bool_lt_disallowed
        test_cmp_bool_gte_disallowed
        test_cmp_void_eq_disallowed
        test_cmp_void_neq_disallowed
        test_cmp_struct_eq_disallowed
        test_cmp_struct_neq_disallowed
        test_cmp_list_lt_disallowed
        test_cmp_list_gte_disallowed
        test_cmp_list_neq_disallowed

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

        # Functions (Advanced)
        test_functions_recursion_factorial_ok
        test_functions_shadowing_local_over_global_ok
        test_functions_return_list_ok
        test_functions_pass_struct_and_return_int_ok
        test_functions_return_struct_access_field_ok
        test_functions_void_used_in_expression_error
        test_functions_multi_path_returns_ok
        test_functions_call_undefined_function_error

        # Lambdas
        test_lambdas_basic_multiply_ok
        test_lambdas_capture_var_ok
        test_lambdas_return_used_in_expr_ok
        test_lambdas_nested_call_ok
        test_lambdas_wrong_arity
        test_lambdas_wrong_arg_type
        test_lambdas_non_callable_used_as_func

        # Lambdas (Advanced)
        test_lambdas_hof_pass_as_param_ok
        test_lambdas_return_lambda_ok
        test_lambdas_capture_modified_after_ref_ok
        # test_lambdas_capture_modified_after_val_ok
        test_lambdas_capture_out_of_scope_error
        test_lambdas_lambda_in_list_ok
        test_lambdas_lambda_in_list_type_error

        # Structs
        test_structs_basic_new_access_ok
        test_structs_assign_struct_to_var_ok
        test_structs_nested_struct_ok
        test_structs_multiple_fields_ok
        test_structs_missing_field_error
        test_structs_extra_field_error
        test_structs_wrong_field_type_error
        test_structs_unknown_field_access_error

        # Structs (Advanced)
        test_structs_field_assign_ok
        test_structs_field_assign_type_error
        test_structs_field_assign_unknown_field_error
        test_structs_nested_field_assign_ok
        test_structs_nested_field_assign_type_error
        test_structs_pass_struct_mutate_and_read_ok
        test_structs_return_struct_ok
        test_structs_list_of_structs_index_ok
        test_structs_list_of_structs_type_error
        test_structs_copy_vs_ref_observe_ok

        # Builtins
        test_builtins_print_int_ok
        test_builtins_print_bool_ok
        test_builtins_print_string_ok
        test_builtins_print_list_int_ok
        test_builtins_print_nested_list_ok
        test_builtins_print_struct_ok
        test_builtins_print_void_ok

        # Builtins (List + Exit)
        test_builtins_exit_0_ok
        test_builtins_exit_84_ok
        test_builtins_exit_wrong_type_error
        test_builtins_head_int_list_ok
        test_builtins_head_empty_list_error
        test_builtins_head_non_list_error
        test_builtins_tail_then_head_ok
        test_builtins_tail_empty_list_error
        test_builtins_tail_singleton_then_head_error
        test_builtins_tail_non_list_error
        test_builtins_nth_ok
        test_builtins_nth_out_of_bounds_error
        test_builtins_nth_negative_index_error
        test_builtins_nth_non_int_index_error
        test_builtins_nth_empty_list_error
        test_builtins_nth_non_list_error
        test_builtins_cons_int_ok
        test_builtins_cons_type_mismatch_error
        test_builtins_cons_wrong_arity_error
        test_builtins_cons_non_list_second_arg_error

        # Print format (non-observe)
        test_print_expr_ok
        test_print_list_int_format_ok
        test_print_nested_list_format_ok
        test_print_struct_format_ok
        test_print_void_no_output_ok

        # Sugar
        test_sugar_postfix_increment_value_ok
        test_sugar_prefix_increment_value_ok
        test_sugar_postfix_decrement_value_ok
        test_sugar_prefix_decrement_value_ok
        test_sugar_inc_in_expression_postfix_ok
        test_sugar_inc_in_expression_prefix_ok
        test_sugar_plus_equals_ok
        test_sugar_minus_equals_ok
        test_sugar_times_equals_ok
        test_sugar_div_equals_ok
        test_sugar_mod_equals_ok
        test_sugar_for_with_i_pp_ok
        test_sugar_for_with_pp_i_ok
        test_sugar_inc_on_bool_error
        test_sugar_plus_equals_on_bool_error
        test_sugar_inc_on_list_error
        test_sugar_plus_equals_on_struct_error
        test_sugar_div_equals_by_zero_error
        test_sugar_mod_equals_by_zero_error

        # Integration
        test_integ_import_struct_function_lambda_ok
        test_integ_loops_lists_builtins_ok
        test_integ_type_inference_chain_ok
        test_integ_lambda_closure_in_struct_ok
        test_integ_import_transitive_then_call_error
        test_integ_multiple_errors_same_file_error
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
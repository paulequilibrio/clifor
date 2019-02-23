program fruit_driver
  use fruit
  use test_clifor_mod_helpers
  use test_clifor_mod_program_info
  use test_clifor_mod_option
  use test_clifor_mod_list
  use test_clifor

  call init_fruit(1)
  call fruit_show_dots


  call reset_unit_name
  call test_clifor_mod_helpers_is_letter
  call test_clifor_mod_helpers_is_digit
  call test_clifor_mod_helpers_is_alphanumeric
  call test_clifor_mod_helpers_has_only_alphanumeric_and_dash
  call test_clifor_mod_helpers_is_valid_short_name
  call test_clifor_mod_helpers_is_valid_long_name
  call test_clifor_get_argument
  call test_clifor_get_argument_value

  call reset_unit_name
  call test_clifor_mod_program_info_minimum
  call test_clifor_mod_program_info_with_pretty_name
  call test_clifor_mod_program_info_with_description
  call test_clifor_mod_program_info_with_named_arguments

  call reset_unit_name
  call test_clifor_mod_option_create_option
  call test_clifor_mod_option_create_required_option
  call test_clifor_mod_option_create_need_value_option
  call test_clifor_mod_option_create_with_named_arguments
  call test_clifor_mod_option_has_short
  call test_clifor_mod_option_has_long
  call test_clifor_mod_option_provided
  call test_clifor_mod_option_value

  call reset_unit_name
  call test_clifor_mod_list_get_head
  call test_clifor_mod_list_add_first_option


  call fruit_summary
  call fruit_finalize
end program fruit_driver

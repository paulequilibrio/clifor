module test_clifor_mod_helpers
  use fruit

  implicit none

contains

  subroutine is_letter(expected, c)
    use clifor_mod_helpers, only: clifor_is_letter
    character, intent(in) :: c
    logical, intent(in) :: expected
    call set_case_name(c)
    call assert_equals(expected, clifor_is_letter(c))
  end subroutine is_letter

  subroutine test_clifor_mod_helpers_is_letter
    call set_unit_name('is letter')
    call is_letter(.true., 'A')
    call is_letter(.true., 'Z')
    call is_letter(.true., 'a')
    call is_letter(.true., 'z')
    call is_letter(.false., '@')
    call is_letter(.false., '[')
    call is_letter(.false., '`')
    call is_letter(.false., '{')
    call is_letter(.false., '-')
    call is_letter(.false., '_')
    call is_letter(.false., '.')
    call is_letter(.true., 'p')
    call is_letter(.true., 'y')
    call is_letter(.false., '0')
  end subroutine test_clifor_mod_helpers_is_letter


  subroutine test_clifor_mod_helpers_is_digit
    use clifor_mod_helpers, only: clifor_is_digit
    call set_unit_name('is digit')
    call set_case_name('digit')
    call assert_equals(.true., clifor_is_digit('3'))
    call set_case_name('letter')
    call assert_equals(.false., clifor_is_digit('a'))
    call set_case_name('space')
    call assert_equals(.false., clifor_is_digit(' '))
    call set_case_name('symbol')
    call assert_equals(.false., clifor_is_digit('>'))
  end subroutine test_clifor_mod_helpers_is_digit


  subroutine test_clifor_mod_helpers_is_alphanumeric
    use clifor_mod_helpers, only: clifor_is_alphanumeric
    call set_unit_name('is alphanumeric')
    call set_case_name('digit')
    call assert_equals(.true., clifor_is_alphanumeric('3'))
    call set_case_name('letter')
    call assert_equals(.true., clifor_is_alphanumeric('p'))
    call set_case_name('space')
    call assert_equals(.false., clifor_is_alphanumeric(' '))
    call set_case_name('symbol')
    call assert_equals(.false., clifor_is_alphanumeric('>'))
  end subroutine test_clifor_mod_helpers_is_alphanumeric


  subroutine test_clifor_mod_helpers_has_only_alphanumeric_and_dash
    use clifor_mod_helpers, only: clifor_has_only_alphanumeric_and_dash
    call set_unit_name('alphanumeric and dash')
    call set_case_name('with digit')
    call assert_equals(.true., clifor_has_only_alphanumeric_and_dash('42'))
    call set_case_name('with letter')
    call assert_equals(.true., clifor_has_only_alphanumeric_and_dash('clifor'))
    call set_case_name('with dash')
    call assert_equals(.true., clifor_has_only_alphanumeric_and_dash('clifor-42'))
    call set_case_name('with space')
    call assert_equals(.false., clifor_has_only_alphanumeric_and_dash('clifro test'))
    call set_case_name('with symbols')
    call assert_equals(.false., clifor_has_only_alphanumeric_and_dash('<clifor>'))
  end subroutine test_clifor_mod_helpers_has_only_alphanumeric_and_dash


  subroutine test_clifor_mod_helpers_is_valid_short_name
    use clifor_mod_helpers, only: clifor_is_valid_short_name
    call set_unit_name('short name')
    call assert_equals(.true., clifor_is_valid_short_name('v'))
    call assert_equals(.true., clifor_is_valid_short_name('H'))
    call assert_equals(.false., clifor_is_valid_short_name(' '))
    call assert_equals(.false., clifor_is_valid_short_name('1'))
    call assert_equals(.false., clifor_is_valid_short_name('fail'))
    call assert_equals(.false., clifor_is_valid_short_name('-'))
  end subroutine test_clifor_mod_helpers_is_valid_short_name


  subroutine test_clifor_mod_helpers_is_valid_long_name
    use clifor_mod_helpers, only: clifor_is_valid_long_name
    call set_unit_name('long name')
    call set_case_name('two letters');
    call assert_equals(.true., clifor_is_valid_long_name('ok'))
    call set_case_name('only letters')
    call assert_equals(.true., clifor_is_valid_long_name('help'))
    call set_case_name('has numbers')
    call assert_equals(.true., clifor_is_valid_long_name('mode3'))
    call set_case_name('letter/number start')
    call assert_equals(.true., clifor_is_valid_long_name('2way'))
    call set_case_name('has - symbol')
    call assert_equals(.true., clifor_is_valid_long_name('input-file'))
    call set_case_name('has others symbols')
    call assert_equals(.false., clifor_is_valid_long_name('fail?'))
    call set_case_name('non letter/number start')
    call assert_equals(.false., clifor_is_valid_long_name('?description'))
    call assert_equals(.false., clifor_is_valid_long_name('-description'))
    call assert_equals(.false., clifor_is_valid_long_name(' description'))
    call set_case_name('has space')
    call assert_equals(.false., clifor_is_valid_long_name('fail or not'))
  end subroutine test_clifor_mod_helpers_is_valid_long_name


  subroutine test_clifor_mod_helpers_get_argument
    use clifor_mod_helpers, only: clifor_get_argument
    call set_unit_name('get argument')
    call assert_equals('./driver.bin', clifor_get_argument(0))
  end subroutine test_clifor_mod_helpers_get_argument


  subroutine test_clifor_mod_helpers_get_argument_value
    use clifor_mod_helpers, only: clifor_get_argument_value
    call set_unit_name('get argument value')
    call set_case_name('-i')
    call assert_equals ('./data/input.json', clifor_get_argument_value('-i'))
    call set_case_name('-o')
    call assert_equals ('./data/output.json', clifor_get_argument_value('-o'))
  end subroutine test_clifor_mod_helpers_get_argument_value


end module test_clifor_mod_helpers

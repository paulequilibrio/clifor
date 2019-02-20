module test_clifor_mod_option
  use fruit

  implicit none

contains

  subroutine test_clifor_mod_option_create_option
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('create new option')
    call option%set('l', 'long', 'description')

    call set_case_name('short')
    call assert_equals('-l', option%get_short())

    call set_case_name('long')
    call assert_equals('--long', option%get_long())

    call set_case_name('description')
    call assert_equals('description', option%get_description())

    call set_case_name('required')
    call assert_equals(.false., option%get_required())

    call set_case_name('need_value')
    call assert_equals(.false., option%get_need_value())

    call set_case_name('value_name')
    call assert_equals('', option%get_value_name())

    call set_case_name('to_string')
    call assert_equals('-l, --long, description', option%to_string())
  end subroutine test_clifor_mod_option_create_option


  subroutine test_clifor_mod_option_create_required_option
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('create required option')
    call option%set('r', 'required', 'required option', .true.)

    call set_case_name('short')
    call assert_equals('-r', option%get_short())

    call set_case_name('long')
    call assert_equals('--required', option%get_long())

    call set_case_name('description')
    call assert_equals('required option', option%get_description())

    call set_case_name('required')
    call assert_equals(.true., option%get_required())

    call set_case_name('need_value')
    call assert_equals(.false., option%get_need_value())

    call set_case_name('value_name')
    call assert_equals('', option%get_value_name())

    call set_case_name('to_string')
    call assert_equals('*-r, --required, required option', option%to_string())
  end subroutine test_clifor_mod_option_create_required_option


  subroutine test_clifor_mod_option_create_need_value_option
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('create value option')
    call option%set('i', 'input-file', 'Path to file with input data', .true., .true., 'PATH')

    call set_case_name('short')
    call assert_equals('-i', option%get_short())

    call set_case_name('long')
    call assert_equals('--input-file', option%get_long())

    call set_case_name('description')
    call assert_equals('Path to file with input data', option%get_description())

    call set_case_name('required')
    call assert_equals(.true., option%get_required())

    call set_case_name('need_value')
    call assert_equals(.true., option%get_need_value())

    call set_case_name('value_name')
    call assert_equals('<PATH>', option%get_value_name())

    call set_case_name('to_string')
    call assert_equals('*-i, --input-file <PATH>, Path to file with input data', option%to_string())
  end subroutine test_clifor_mod_option_create_need_value_option


  subroutine test_clifor_mod_option_create_with_named_arguments
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('with named arguments')
    call option%set( &
      need_value=.true., &
      value_name='MODEL', &
      description='Model to use', &
      long='model', &
      required=.true., &
      short='m' &
    )

    call set_case_name('short')
    call assert_equals('-m', option%get_short())

    call set_case_name('long')
    call assert_equals('--model', option%get_long())

    call set_case_name('description')
    call assert_equals('Model to use', option%get_description())

    call set_case_name('required')
    call assert_equals(.true., option%get_required())

    call set_case_name('need_value')
    call assert_equals(.true., option%get_need_value())

    call set_case_name('value_name')
    call assert_equals('<MODEL>', option%get_value_name())

    call set_case_name('to_string')
    call assert_equals('*-m, --model <MODEL>, Model to use', option%to_string())
  end subroutine test_clifor_mod_option_create_with_named_arguments


  subroutine test_clifor_mod_option_has_short
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('has short')
    call option%set('i', 'input', 'description')
    call assert_equals(.true., option%has_short('i'))
    call assert_equals(.false., option%has_short('o'))
  end subroutine test_clifor_mod_option_has_short


  subroutine test_clifor_mod_option_has_long
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('has long')
    call option%set('o', 'output', 'description')
    call assert_equals(.true., option%has_long('output'))
    call assert_equals(.false., option%has_long('input'))
  end subroutine test_clifor_mod_option_has_long

end module test_clifor_mod_option

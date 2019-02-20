module test_clifor_mod_option
  use fruit
  use clifor_mod_helpers, only: TAB
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


  subroutine test_clifor_mod_option_is_equal
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option1, option2

    call set_unit_name('comparison equal options')

    call set_case_name('same short and long')
    call option1%set('o', 'option', 'irrelevant')
    call option2%set('o', 'option', 'irrelevant')
    call assert_equals(.true., option1 == option2)

    call set_case_name('only same short')
    call option2%set('o', 'op', 'irrelevant')
    call assert_equals(.true., option1 == option2)

    call set_case_name('only same long')
    call option2%set('p', 'option', 'irrelevant')
    call assert_equals(.true., option1 == option2)
  end subroutine test_clifor_mod_option_is_equal


  subroutine test_clifor_mod_option_is_different
    use clifor_mod_option, only: clifor_type_option
    type(clifor_type_option) :: option1, option2

    call set_unit_name('comparison different options')

    call set_case_name('different short and long')
    call option1%set('c', 'clifor', 'irrelevant')
    call option2%set('f', 'fortran', 'irrelevant')
    call assert_equals(.true., option1 /= option2)

    call set_case_name('only long different')
    call option2%set('c', 'fortran', 'irrelevant')
    call assert_equals(.false., option1 /= option2)

    call set_case_name('only short different')
    call option2%set('f', 'clifor', 'irrelevant')
    call assert_equals(.false., option1 /= option2)
  end subroutine test_clifor_mod_option_is_different


  ! subroutine test_clifor_mod_option_next
  !   use clifor_mod_option, only: clifor_type_option
  !   type(clifor_type_option) :: option, next, temp
  !
  !   call set_unit_name('option next')
  !   call option%set('a', 'actual', 'Actual option')
  !   call temp%set('n', 'next', 'Next option')
  !
  !   call set_case_name('starts with no next')
  !   call assert_equals(.false., option%has_next())
  !
  !   call set_case_name('set next')
  !   call option%set_next(temp)
  !   call assert_equals(.true., option%has_next())
  !
  !   call set_case_name('get next')
  !   next = option%get_next()
  !   call assert_equals(.true., next == temp)
  !
  !   call set_case_name('next starts with no next')
  !   call assert_equals(.false., next%has_next())
  !
  !   call set_case_name('is the right next')
  !   call assert_equals('--next', next%get_long())
  ! end subroutine test_clifor_mod_option_next

end module test_clifor_mod_option

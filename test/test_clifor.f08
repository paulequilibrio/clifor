module test_clifor
  use fruit

  implicit none

contains

  subroutine test_clifor_short_already_exist
    use clifor, only: short_already_exist, clifor_options, clifor_type_option, show_all
    type(clifor_type_option) :: option

    call set_unit_name('short already exist')
    call option%set('o', 'option-name', 'test short option name')
    call clifor_options%add(option)

    call set_case_name('existent')
    call assert_equals (.true., short_already_exist('o'))

    call set_case_name('nonexistent')
    call assert_equals (.false., short_already_exist('n'))

    call clifor_options%deallocate
  end subroutine test_clifor_short_already_exist


  subroutine test_clifor_long_already_exist
    use clifor, only: long_already_exist, clifor_options, clifor_type_option
    type(clifor_type_option) :: option

    call set_unit_name('long already exist')
    call option%set('l', 'long', 'test long option name')
    call clifor_options%add(option)

    call set_case_name('existent')
    call assert_equals (.true., long_already_exist('long'))

    call set_case_name('nonexistent')
    call assert_equals (.false., long_already_exist('nonexistent'))

    call clifor_options%deallocate()
  end subroutine test_clifor_long_already_exist


  subroutine test_clifor_create_option
    use clifor, only: clifor_options, clifor_create_option, short_already_exist, long_already_exist

    call set_unit_name('create option')
    call set_case_name('list starts empty')
    call assert_equals(0, clifor_options%len())

    call set_case_name('create first option')
    call assert_equals(.false., short_already_exist('h'))
    call clifor_create_option('h', 'help', 'Show this help message')
    call assert_equals(1, clifor_options%len())
    call assert_equals(.true., short_already_exist('h'))
    call assert_equals(.true., long_already_exist('help'))
    call clifor_options%deallocate()
  end subroutine test_clifor_create_option

end module test_clifor

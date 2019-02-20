module test_clifor
  use fruit

  implicit none

contains

  subroutine test_clifor_get_argument
    use clifor, only: clifor_get_argument
    call set_unit_name('get argument')
    call assert_equals('./driver.bin', clifor_get_argument(0))
  end subroutine test_clifor_get_argument


  subroutine test_clifor_get_argument_value
    use clifor, only: clifor_get_argument_value
    call set_unit_name('get argument value')
    call set_case_name('-i')
    call assert_equals ('./data/input.json', clifor_get_argument_value('-i'))
    call set_case_name('-o')
    call assert_equals ('./data/output.json', clifor_get_argument_value('-o'))
  end subroutine test_clifor_get_argument_value


  ! subroutine test_clifor_create_option
  !   use clifor_options, only: clifor_create_option, clifor_options_head, clifor_options_length, clifor_type_option
  !   type(clifor_type_option) :: option
  !   call set_unit_name('create option')
  !   call assert_equals(0, clifor_options_length)
  !   call clifor_create_option('h', 'help', 'Show this help message')
  !   call assert_equals(1, clifor_options_length)
  !   call assert_equals('-h', clifor_options_head%short)
  !   call assert_equals('--help', clifor_options_head%long)
  !   call assert_equals('Show this help message', clifor_options_head%description)
  !   call clifor_create_option('v', 'version', 'Show version and exit', .true., .true.)
  !   call assert_equals(2, clifor_options_length)
  !   call assert_equals('-h', clifor_options_head%short)
  !   call assert_equals('--help', clifor_options_head%long)
  !   call assert_equals('Show this help message', clifor_options_head%description)
  !   ! call run_test_case('tc', 'name')
  ! end subroutine test_clifor_create_option


  ! subroutine test_clifor_get_option
  !   use clifor_options, only: clifor_get_option, clifor_type_option, clifor_options_head, clifor_print_option
  !   type(clifor_type_option) :: option
  !   ! call clifor_print_option(clifor_options_head)
  !   option = clifor_get_option('-h')
  ! end subroutine test_clifor_get_option




  ! subroutine test_clifor_create_option
  !   use clifor_mod_options, only: clifor_create_option, clifor_type_option, clifor_options_list
  !   type(clifor_type_option) :: option
  !   call set_unit_name('create option')
  !   ! call assert_equals(0, clifor_options_list%len())
  !   ! call clifor_create_option('h', 'help', 'Show this help message')
  !   ! call assert_equals(1, clifor_options_length)
  !   ! call assert_equals('-h', clifor_options_head%short)
  !   ! call assert_equals('--help', clifor_options_head%long)
  !   ! call assert_equals('Show this help message', clifor_options_head%description)
  ! end subroutine test_clifor_create_option


end module test_clifor

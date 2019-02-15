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


  subroutine isletter(expected, c)
    use clifor_options, only: clifor_isletter
    character, intent(in) :: c
    logical, intent(in) :: expected
    call set_case_name(c)
    call assert_equals(expected, clifor_isletter(c))
  end subroutine isletter

  subroutine test_clifor_isletter
    call set_unit_name('is letter')
    call isletter(.true., 'A')
    call isletter(.true., 'Z')
    call isletter(.true., 'a')
    call isletter(.true., 'z')
    call isletter(.false., '@')
    call isletter(.false., '[')
    call isletter(.false., '`')
    call isletter(.false., '{')
    call isletter(.false., '-')
    call isletter(.false., '_')
    call isletter(.false., '.')
    call isletter(.true., 'p')
    call isletter(.true., 'y')
    call isletter(.false., '0')
  end subroutine test_clifor_isletter


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


  subroutine test_clifor_program_info
    use clifor, only: clifor_type_program_info, clifor_set_program_info, clifor_get_program_info
    type(clifor_type_program_info) :: info

    call set_unit_name('minimum info')
    call clifor_set_program_info('example1', '0.1.0')
    info = clifor_get_program_info()
    call set_case_name('get name')
    call assert_equals('example1', info%name)
    call set_case_name('get version')
    call assert_equals('0.1.0', info%version)

    call set_unit_name('info with pretty name')
    call clifor_set_program_info('example2', '0.2.0', 'Example of CLIFor use')
    info = clifor_get_program_info()
    call set_case_name('get name')
    call assert_equals('example2', info%name)
    call set_case_name('get version')
    call assert_equals('0.2.0', info%version)
    call set_case_name('get pretty name')
    call assert_equals('Example of CLIFor use', info%pretty_name)

    call set_unit_name('info with description')
    call clifor_set_program_info('example3', '0.3.0', description='A simple program to show how to use the CLIFor module.')
    info = clifor_get_program_info()
    call set_case_name('get description')
    call assert_equals('A simple program to show how to use the CLIFor module.', info%description)

    call set_unit_name('info with named args')
    call clifor_set_program_info( &
      pretty_name='Example 4', &
      version='0.4.0', &
      description='Simple example program', &
      name='example4' &
    )
    info = clifor_get_program_info()
    call set_case_name('get name')
    call assert_equals('example4', info%name)
    call set_case_name('get version')
    call assert_equals('0.4.0', info%version)
    call set_case_name('get pretty name')
    call assert_equals('Example 4', info%pretty_name)
    call set_case_name('get description')
    call assert_equals('Simple example program', info%description)
  end subroutine test_clifor_program_info


  subroutine test_clifor_create_option
    use clifor_options, only: clifor_create_option, clifor_options_head, clifor_options_length, clifor_type_option
    type(clifor_type_option) :: option
    call set_unit_name('create option')
    call assert_equals(0, clifor_options_length)
    call clifor_create_option('h', 'help', 'Show this help message')
    call assert_equals(1, clifor_options_length)
    call assert_equals('-h', clifor_options_head%short)
    call assert_equals('--help', clifor_options_head%long)
    call assert_equals('Show this help message', clifor_options_head%description)
  end subroutine test_clifor_create_option


end module test_clifor

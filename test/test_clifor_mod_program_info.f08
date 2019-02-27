module test_clifor_mod_type_program_info
  use fruit

  implicit none

contains

  subroutine test_clifor_mod_type_program_info_minimum
    use clifor_mod_type_program_info, only: clifor_type_program_info
    type(clifor_type_program_info) :: info
    call set_unit_name('minimum info')
    call info%set('minimum-example', '0.1.0')
    call set_case_name('get name')
    call assert_equals('minimum-example', info%get_name())
    call set_case_name('get version')
    call assert_equals('0.1.0', info%get_version())
  end subroutine test_clifor_mod_type_program_info_minimum


  subroutine test_clifor_mod_type_program_info_with_pretty_name
    use clifor_mod_type_program_info, only: clifor_type_program_info
    type(clifor_type_program_info) :: info
    call set_unit_name('info with pretty name')
    call info%set('example', '0.2.0', 'Example of CLIFor use')
    call set_case_name('get name')
    call assert_equals('example', info%get_name())
    call set_case_name('get version')
    call assert_equals('0.2.0', info%get_version())
    call set_case_name('get pretty name')
    call assert_equals('Example of CLIFor use', info%get_pretty_name())
  end subroutine test_clifor_mod_type_program_info_with_pretty_name


  subroutine test_clifor_mod_type_program_info_with_description
    use clifor_mod_type_program_info, only: clifor_type_program_info
    type(clifor_type_program_info) :: info
    call set_unit_name('info with description')
    call info%set('example-description', '0.3.0', description='A simple program to show how to use the CLIFor module.')
    call set_case_name('get name')
    call assert_equals('example-description', info%get_name())
    call set_case_name('get version')
    call assert_equals('0.3.0', info%get_version())
    call set_case_name('get description')
    call assert_equals('A simple program to show how to use the CLIFor module.', info%get_description())
  end subroutine test_clifor_mod_type_program_info_with_description


  subroutine test_clifor_mod_type_program_info_with_named_arguments
    use clifor_mod_type_program_info, only: clifor_type_program_info
    type(clifor_type_program_info) :: info
    call set_unit_name('info with named args')
    call info%set( &
      pretty_name='Example with named arguments', &
      version='0.4.0', &
      description='Simple example program using named arguments', &
      name='example-named' &
    )
    call set_case_name('get name')
    call assert_equals('example-named', info%get_name())
    call set_case_name('get version')
    call assert_equals('0.4.0', info%get_version())
    call set_case_name('get pretty name')
    call assert_equals('Example with named arguments', info%get_pretty_name())
    call set_case_name('get description')
    call assert_equals('Simple example program using named arguments', info%get_description())
  end subroutine test_clifor_mod_type_program_info_with_named_arguments

end module test_clifor_mod_type_program_info

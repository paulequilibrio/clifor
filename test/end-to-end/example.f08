program example
  use clifor

  implicit none

  ! clifor_set_program_info(
  !                          <character:executable_name>,
  !                          <character:version>,
  !                          [<character:Pretty Name>]
  !                          [<character:Short description>]
  !                        )
  ! call clifor_set_program_info(name, version, pretty_name, description)
  call clifor_set_program_info( &
    name='example', &
    version='0.1.0', &
    pretty_name='CLIFor example use', &
    description='Shows how to use CLIFor module for Fortran programs' &
  )

  ! Create the options that will be recognized by your program
  ! clifor_create_option(
  !                       <character:one letter short option name>,
  !                       <character:two or more letters long option name>,
  !                       <character:option description>
  !                       [<logical:this option is required?>]
  !                       [<logical:this option need a value?>]
  !                     )
  ! call clifor_create_option(short, long, description, required, needvalue)
  call clifor_create_option('h', 'help', 'Show this help message')
  call clifor_create_option('v', 'version', 'Show version and exit')
  call clifor_create_option('i', 'input-file', 'File with input data', &
    required=.true., &
    need_value=.true., &
    value_name='FILEPATH' &
  )

  ! call clifor_show_program_version
  call clifor_show_program_help
  call clifor_finalizer

  ! call clifor_create_option('i', 'input-file', 'The path to input file', )

  ! write(*,'(L1)') clifor_is_letter('`')
  ! call clifor_write_stdout('just a small text', '(a)')
  ! call clifor_write_stdout('a small text without format')
  ! call clifor_write_stdout(9876, '(i4)')
  ! call clifor_write_stdout(987654321)
  ! call clifor_write_stdout(123.12, '(f6.2)')
  ! call clifor_write_stdout(12345.123)
  !
  ! ! logical :: version, help, i, a
  ! call clifor_write_stderr('just a small text', '(a)')
  ! call clifor_write_stderr('a small text without format')
  ! call clifor_write_stderr(9876, '(i4)')
  ! call clifor_write_stderr(987654321)
  ! call clifor_write_stderr(123.12, '(f6.2)')
  ! call clifor_write_stderr(12345.123)

  ! logical :: version, help, i, a
  !
  !
  ! call clifor_set_program_version('0.1.0')
  ! call clifor_set_program_pretty_name('Example of CLIFor use')
  ! call clifor_set_program_name('example')
  !
  ! call clifor_create_option('v', 'version', 'Show version and exit')
  ! call clifor_create_option('h', 'help', 'Show this help message')
  ! call clifor_create_option('i', 'input-file', 'The path to input file')
  ! call clifor_create_option('a', 'a2', 'The path to input file')
  ! ! call clifor_create_option('h', '--', 'Show this help message')
  ! ! call clifor_create_option('z', 'help', 'Show version and exit')
  ! ! write(*,*) ichar('`')
  !
  ! call clifor_process_options()


  ! version = clifor_get_option('version')
  ! help = clifor_get_option('h')
  ! i = clifor_get_option('input-file')
  ! a = clifor_get_option('a2')
  ! ! write(*,*) 'version: ', v
  ! ! write(*,*) 'help: ', h
  ! ! write(*,*) 'input-file: ', i
  ! ! write(*,*) 'a2: ', a
  ! ! write(*,*) TRANSFER(4.0, 0 )
  !
  ! if (version) call clifor_show_program_version
  ! if (help) call clifor_show_program_help
  !
  ! ! call clifor_set_program_name('example')
  ! ! call clifor_set_program_version('0.1.0')
  ! ! call clifor_write_info(clifor_program_name)
  ! ! call clifor_write_warning(clifor_program_name)
  ! ! call clifor_write_stdout(clifor_program_version)
  ! ! call clifor_write_error(clifor_program_name)
  ! ! write(*,'(a)') clifor_program_version

end program example

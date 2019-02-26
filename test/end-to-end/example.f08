program example
  ! Import CLIFor module
  use clifor

  implicit none

  ! Create variables to save values from command line options
  character(len=:), allocatable :: input_file, output_file
  logical :: quiet

  ! Set info about your program
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
  !                       [<character:name of the value>]
  !                     )
  ! call clifor_create_option(short, long, description, required, need_value, value_name)
  call clifor_create_option('h', 'help', 'Show this help message')
  call clifor_create_option('v', 'version', 'Show version and exit')
  call clifor_create_option('i', 'input-file', 'File with input data', .true., .true., 'FILEPATH')
  call clifor_create_option( &
    required=.true., &
    need_value=.true., &
    value_name='FILEPATH', &
    description='File with ouput data', &
    short='o', &
    long='output-file' &
  )
  call clifor_create_option('q', 'quiet', 'Omit output')

  ! Read arguments from command line and look for valid options
  ! In case of invalid options, exit the program
  call clifor_read_command_line

  ! Check for early exit options (like --help and --version) and act on it
  if (clifor_flag_was_provided('help')) call clifor_show_program_help
  if (clifor_flag_was_provided('v')) call clifor_show_program_version

  ! Ensure that all required options have been provided, otherwise exit the program
  call clifor_ensure_required_options

  ! Save values from options
  quiet = clifor_flag_was_provided('q')
  allocate(character(0) :: input_file)
  input_file = clifor_get_value_from_option('input-file')
  allocate(character(0) :: output_file)
  output_file = clifor_get_value_from_option('o')

  ! After save flags and values, free the memory used by CLIFor
  call clifor_finalizer

  ! Use the saved values in your program
  if (.not. quiet) then
    write(*, '(2(a,1x))') 'input: '//input_file//', output: '//output_file
  end if

end program example

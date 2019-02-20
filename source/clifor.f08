module clifor
  use clifor_mod_helpers
  use clifor_mod_program_info
  use clifor_mod_option
  use clifor_mod_list

  implicit none

  type(clifor_type_program_info) :: clifor_program_info
  type(clifor_type_list) :: clifor_options
  integer :: largest_option_name

  private &
    largest_option_name, &
    get_largest_option_name, &
    print_to_help

contains

  subroutine clifor_finalizer
    call clifor_options%deallocate
    ! stop
  end subroutine clifor_finalizer


  subroutine clifor_set_program_info(name, version, pretty_name, description)
    character(len=*), intent(in) :: name, version
    character(len=*), intent(in), optional :: pretty_name, description
    if (index(name, ' ') /= 0) then
      call clifor_write_error('white space in program executable name (not allowed)')
      call clifor_finalizer
      stop
    end if
    call clifor_program_info%set(name, version, pretty_name, description)
  end subroutine clifor_set_program_info


  pure function clifor_get_program_info() result(info)
    type(clifor_type_program_info) :: info
    info = clifor_program_info
  end function clifor_get_program_info


  subroutine clifor_show_program_version
    call clifor_write_stdout(clifor_program_info%get_version())
    call clifor_finalizer
    stop
  end subroutine clifor_show_program_version


  subroutine clifor_create_option(short, long, description, required, need_value, value_name)
    character(len=*), intent(in) :: short, long, description
    logical, intent(in), optional :: required, need_value
    character(len=*), intent(in), optional :: value_name
    type(clifor_type_option) :: option
    call option%set(short, long, description, required, need_value, value_name)
    if (.not. already_exist(option)) call clifor_options%add(option)
  end subroutine clifor_create_option


  function already_exist(option) result(exist)
    type(clifor_type_option), intent(in) :: option
    type(clifor_type_node), pointer :: node
    logical :: exist
    node => clifor_options%get_head()
    exist = .false.
    do
      if(.not. associated(node)) exit
      select type (data => node%get_data())
        type is (clifor_type_option)
          if (option%get_short() == data%get_short()) then
            call clifor_write_error('Duplicated short option name: '//option%get_short())
            exist = .true.
          elseif (option%get_long() == data%get_long()) then
            call clifor_write_error('Duplicated long option name: '//option%get_long())
            exist = .true.
          end if
      end select
      if (exist) then
        call clifor_finalizer
        stop
      end if
      node => node%get_next()
    end do
  end function already_exist



  function clifor_option_is_present(name) result(present)
    character(len=*), intent(in) :: name
    logical :: present
    present = .false.
    ! call clifor_options%filter(is_present)
  end function clifor_option_is_present


  subroutine is_present(value, node, done)
    class(*), intent(in), pointer :: value
    type(clifor_type_node), intent(inout), pointer :: node
    logical, intent(out) :: done
    done = .false.
  end subroutine is_present


  ! TODO: improve clifor_show_program_usage
  subroutine clifor_show_program_usage
    call clifor_write_stdout('Usage: '//clifor_program_info%get_name()//' [options]'//NL)
  end subroutine clifor_show_program_usage


  subroutine clifor_show_program_help
    character(len=:), allocatable :: name
    type(clifor_type_program_info) :: i
    i = clifor_program_info
    allocate(character(0) :: name)
    name = i%get_name()
    if (i%get_pretty_name() /= '') name = i%get_pretty_name()

    call clifor_write_stdout(name//' (v'//i%get_version()//') - '//i%get_description()//NL)
    call clifor_show_program_usage
    call clifor_write_stdout('Available options (* = required):'//NL)

    largest_option_name = 0
    call clifor_options%for_each(get_largest_option_name)
    call clifor_options%for_each(print_to_help)
    call clifor_finalizer
  end subroutine clifor_show_program_help


  subroutine print_to_help(node, done)
    type(clifor_type_node), intent(inout), pointer :: node
    logical, intent(out) :: done
    character(len=largest_option_name) :: name
    character :: required
    character(len=:), allocatable :: line
    done = .false.
    allocate(character(0) :: line)
    select type (option => node%get_data())
      type is (clifor_type_option)
        name = option%get_short()//', '//option%get_long()//' '//option%get_value_name()
        required = merge('*', ' ', option%get_required())
        line = name//'    '//required//' '//option%get_description()
        call clifor_write_stdout(line, '(t3, a)')
    end select
    deallocate(line)
  end subroutine print_to_help


  subroutine get_largest_option_name(node, done)
    type(clifor_type_node), intent(inout), pointer :: node
    logical, intent(out) :: done
    character(len=:), allocatable :: name
    done = .false.
    allocate(character(0) :: name)
    select type (option => node%get_data())
      type is (clifor_type_option)
        name = option%get_short()//', '//option%get_long()//' '//option%get_value_name()
        if (len(name) > largest_option_name) largest_option_name = len(name)
    end select
    deallocate(name)
  end subroutine get_largest_option_name

end module clifor

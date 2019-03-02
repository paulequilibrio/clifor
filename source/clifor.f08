module clifor
  use clifor_mod_helpers
  use clifor_mod_type_program_info
  use clifor_mod_type_option
  use clifor_mod_type_list

  implicit none

  type(clifor_type_program_info), protected :: clifor_program_info
  type(clifor_type_list) :: clifor_options
  integer, protected :: clifor_largest_option_name
  integer :: clifor_description_shift

  private &
    check_if_already_created, &
    get_largest_option_name, &
    print_to_help

contains

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
    call check_if_already_created(option)
    call clifor_options%add(option)
  end subroutine clifor_create_option


  subroutine check_if_already_created(option)
    type(clifor_type_option), intent(in) :: option
    call clifor_options%for_each(already_created)
  contains
    subroutine already_created(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      select type(data => node%get_data())
        type is(clifor_type_option)
          if (option%get_short() == data%get_short()) then
            call clifor_write_error('It is not allowed to create two options with the same short name: '//option%get_short())
            done = .true.
          elseif (option%get_long() == data%get_long()) then
            call clifor_write_error('It is not allowed to create two options with the same long name: '//option%get_long())
            done = .true.
          end if
      end select
      if (done) then
        call clifor_finalizer
        stop
      end if
    end subroutine already_created
  end subroutine check_if_already_created


  function short_already_exist(short) result(exist)
    character(len=*), intent(in) :: short
    logical :: exist
    exist = .false.
    call clifor_options%for_each(check_short)
  contains
    subroutine check_short(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      select type(option => node%get_data())
        type is(clifor_type_option)
          exist = option%has_short(short)
          done = exist
      end select
    end subroutine check_short
  end function short_already_exist


  function long_already_exist(long) result(exist)
    character(len=*), intent(in) :: long
    logical :: exist
    call clifor_options%for_each(check_long)
  contains
    subroutine check_long(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      select type(option => node%get_data())
        type is(clifor_type_option)
          exist = option%has_long(long)
          done = exist
      end select
    end subroutine check_long
  end function long_already_exist


  subroutine clifor_read_command_line
    integer :: total, index
    character(len=:), allocatable :: argument, value
    type(clifor_type_option), pointer :: option

    total = command_argument_count()
    index = 1

    do
      if (index > total) exit
      argument = clifor_get_argument(index)

      if (len(argument) < 2) then
        call clifor_write_error('Unknow option: '//argument)
        call clifor_stop
      end if

      call clifor_options%for_each(check_provided)

      if (.not. associated(option)) then
        call clifor_write_error('Invalid option: '//argument)
        call clifor_stop
      end if

      if (option%get_provided()) then
        call clifor_write_error('Duplicated option: '//argument)
        call clifor_stop
      end if

      call option%set_provided(.true.)

      if (option%get_need_value()) then
        if (index + 1 > total) then
          call clifor_write_error("Missing required value for option: "//argument//" "//option%get_value_name())
          call clifor_stop
        end if

        allocate(character(1) :: value)
        value = clifor_get_argument(index + 1)

        if (value(1:1) == '-') then
          call clifor_write_error("Missing required value for option: "//argument//" "//option%get_value_name())
          call clifor_stop
        end if

        call option%set_value(value)
        deallocate(value)
        index = index + 2
      else
        index = index + 1
      end if

      nullify(option)
    end do

  contains
    subroutine check_provided(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      class(*), pointer :: actual_option
      actual_option => node%get_data()
      select type(actual_option)
        type is(clifor_type_option)
          if (argument(1:2) == '--') then
            if (actual_option%has_long(argument(3:))) then
              option => actual_option
              done = .true.
            end if
          elseif (argument(1:1) == '-') then
            if (actual_option%has_short(argument(2:))) then
              option => actual_option
              done = .true.
            end if
          else
            call clifor_write_error('Option not starting with dash(-): '//argument)
            call clifor_stop
          end if
      end select
    end subroutine check_provided
  end subroutine clifor_read_command_line


  subroutine clifor_ensure_required_options
    call clifor_options%for_each(required_was_provided)
  contains
    subroutine required_was_provided(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      done = .false.
      select type(option => node%get_data())
        type is(clifor_type_option)
          if (option%get_required() .and. .not. option%get_provided()) then
            call clifor_write_error('Missing required option: '//option%get_short()//' ('//option%get_long()//')')
            call clifor_stop
          end if
      end select
    end subroutine required_was_provided
  end subroutine clifor_ensure_required_options


  ! TODO: improve clifor_show_program_usage
  subroutine clifor_show_program_usage
    call clifor_write_stdout('Usage: '//clifor_program_info%get_name()//' [options]')
  end subroutine clifor_show_program_usage


  subroutine clifor_show_program_help
    call clifor_program_info%print
    write(*, *)
    call clifor_show_program_usage
    write(*, *)
    call clifor_write_stdout('Options (*required):'//NL)
    clifor_largest_option_name = 0
    call clifor_options%for_each(get_largest_option_name)
    call clifor_options%for_each(print_to_help)
    call clifor_stop
  end subroutine clifor_show_program_help


  subroutine print_to_help(node, done)
    type(clifor_type_node), intent(inout), pointer :: node
    logical, intent(out) :: done
    integer :: i
    done = .false.
    select type (option => node%get_data())
      type is (clifor_type_option)
        i = clifor_description_shift
        clifor_description_shift = merge(i, 4, i > 0)
        call option%print(clifor_description_shift, clifor_largest_option_name)
    end select
  end subroutine print_to_help


  subroutine get_largest_option_name(node, done)
    type(clifor_type_node), intent(inout), pointer :: node
    logical, intent(out) :: done
    integer :: length
    done = .false.
    select type (option => node%get_data())
      type is (clifor_type_option)
        length = option%get_name_length()
        if (length > clifor_largest_option_name) clifor_largest_option_name = length
    end select
  end subroutine get_largest_option_name


  subroutine show_all(list_name)
    type(clifor_type_list), intent(in), optional :: list_name
    type(clifor_type_list) :: list

    list = merge(list_name, clifor_options, present(list_name))

    if (list%len() == 0) then
      call clifor_write_stdout('Empty list')
    else
      call list%for_each(show)
    end if

  contains

    subroutine show(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      done = .false.
      select type (option => node%get_data())
        type is (clifor_type_option)
          write(*,*) option
      end select
    end subroutine show

  end subroutine show_all


  subroutine clifor_finalizer
    call clifor_options%deallocate
  end subroutine clifor_finalizer


  subroutine clifor_stop
    call clifor_options%deallocate
    stop
  end subroutine clifor_stop


  function clifor_flag_was_provided(flag_name) result(provided)
    character(len=*), intent(in) :: flag_name
    logical :: provided
    provided = .false.
    call clifor_options%for_each(is_provided)
  contains
    subroutine is_provided(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      select type(option => node%get_data())
        type is(clifor_type_option)
          if (option%has_short(flag_name) .or. option%has_long(flag_name)) then
            provided = option%get_provided()
            done = .true.
          end if
      end select
    end subroutine is_provided
  end function clifor_flag_was_provided


  function clifor_get_value_from_option(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    call clifor_options%for_each(get_value)
  contains
    subroutine get_value(node, done)
      type(clifor_type_node), intent(inout), pointer :: node
      logical, intent(out) :: done
      select type(option => node%get_data())
      type is(clifor_type_option)
        if (option%has_short(name) .or. option%has_long(name)) then
          value = option%get_value()
          done = .true.
        end if
      end select
    end subroutine get_value
  end function clifor_get_value_from_option


end module clifor

module clifor
  use clifor_mod_helpers
  use clifor_mod_program_info
  use clifor_mod_option
  use clifor_mod_list

  implicit none

  type(clifor_type_program_info), protected :: clifor_program_info
  type(clifor_type_list) :: clifor_options
  integer, protected :: clifor_largest_option_name
  integer :: clifor_description_shift

  private &
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
    ! if (.not. already_exist(option))
    call clifor_options%add(option)
  end subroutine clifor_create_option


  ! TODO: refactor using short_already_exist and long_already_exist
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


  function short_already_exist(short) result(exist)
    character(len=*), intent(in) :: short
    logical :: exist
    exist = .false.
    call clifor_options%for_each(check_short)
  contains
    subroutine check_short(node, done)
      type(clifor_type_node), intent(in), pointer :: node
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
      type(clifor_type_node), intent(in), pointer :: node
      logical, intent(out) :: done
      select type(option => node%get_data())
        type is(clifor_type_option)
          exist = option%has_long(long)
          done = exist
      end select
    end subroutine check_long
  end function long_already_exist


  ! TODO: refactor and create get_option and get_value
  function clifor_option_is_present(name) result(present)
    character(len=*), intent(in) :: name
    logical :: present
    present = short_already_exist(name)
    ! call clifor_options%filter(is_present)
  end function clifor_option_is_present


  ! subroutine is_present(value, node, done)
  !   class(*), intent(in), pointer :: value
  !   type(clifor_type_node), intent(inout), pointer :: node
  !   logical, intent(out) :: done
  !   done = .false.
  ! end subroutine is_present


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
    call clifor_finalizer
  end subroutine clifor_show_program_help


  subroutine print_to_help(node, done)
    type(clifor_type_node), intent(in), pointer :: node
    logical, intent(out) :: done
    integer :: i
    done = .false.
    select type (option => node%get_data())
      type is (clifor_type_option)
        i = clifor_description_shift
        clifor_description_shift = merge(i, 4, i > 0)
        call option%print(clifor_largest_option_name, clifor_description_shift)
    end select
  end subroutine print_to_help


  subroutine get_largest_option_name(node, done)
    type(clifor_type_node), intent(in), pointer :: node
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
      type(clifor_type_node), intent(in), pointer :: node
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
    ! stop
  end subroutine clifor_finalizer

end module clifor

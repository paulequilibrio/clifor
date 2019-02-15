module clifor_options
  use clifor_write
  use clifor_types

  implicit none

  type(clifor_type_option), pointer :: clifor_options_head => null()
  type(clifor_type_option), pointer :: clifor_options_tail => null()
  logical, dimension(:), allocatable :: clifor_options_present
  integer :: clifor_options_length

  type(clifor_type_list), pointer, private :: clifor_options_list => null()

contains


  function clifor_get_argument(index) result(argument)
    integer, intent(in) :: index
    integer :: argument_length
    character(len=:), allocatable :: argument
    call get_command_argument(index, length=argument_length)
    allocate(character(argument_length) :: argument)
    call get_command_argument(index, value=argument)
  end function clifor_get_argument


  function clifor_get_argument_value(argument_name) result(argument_value)
    character(len=*), intent(in) :: argument_name
    character(len=:), allocatable :: argument, argument_value
    integer :: index
    do index = 1, command_argument_count()
      argument = clifor_get_argument(index)
      flag: if ( argument_name == argument ) then
        argument_value = clifor_get_argument(index + 1)
        return
      end if flag
    end do
  end function clifor_get_argument_value


  subroutine clifor_create_option(short, long, description, required, needvalue, value)
    character(len=*), intent(in) :: short
    character(len=*), intent(in) :: long
    character(len=*), intent(in) :: description
    logical, intent(in), optional :: required
    logical, intent(in), optional :: needvalue
    character(len=*), intent(in), optional :: value
    integer :: status !, index
    ! type(clifor_type_option) :: option
    ! type(clifor_type_option), pointer :: option => null()

    if (len(short) /= 1 .or. .not. clifor_isletter(short)) then
      call clifor_write_error('The short option name should be a single letter')
      stop 1
    end if

    if (.not. clifor_isletter(long(1:1))) then
      call clifor_write_error('The long option should start with one letter. Invalid character: '//long(1:1))
      stop 2
    end if

    if (index(long, ' ') /= 0) then
      call clifor_write_error('The long option name should not contain any white space')
      stop 3
    end if

    if (len(long) < 2) then
      call clifor_write_error('The long option name should be at least 2 letter long')
      stop 4
    end if


    ! do index=1, len(long)
    !   if (.not. clifor_isletter(long(index:index))) then
    !     call clifor_write_error('The long option should contain only letters. Invalid character: '//long(index:index))
    !   end if
    ! end do

    if (.not. associated(clifor_options_head)) then
      clifor_options_length = 0
      allocate(clifor_options_head, stat=status)
      if (status /= 0) then
        call clifor_write_error('Failure when allocate the first CLI option')
        stop -1
      end if
      clifor_options_tail => clifor_options_head
    else
      allocate(clifor_options_tail%next, stat=status)
      if (status /= 0) then
        call clifor_write_error('Failure when allocate a new CLI option')
        stop -1
      end if
      clifor_options_tail => clifor_options_tail%next
    end if

    nullify(clifor_options_tail%next)
    clifor_options_tail%short = '-'//short
    clifor_options_tail%long = '--'//trim(adjustl(long))
    clifor_options_tail%description = trim(description)
    ! ternary: variable = merge(value if true, value if false, condition)
    clifor_options_tail%required = merge(required, .false., present(required))
    clifor_options_tail%needvalue = merge(needvalue, .false., present(needvalue))
    clifor_options_tail%value = merge(value, '', present(value))
    clifor_options_length = clifor_options_length + 1
    ! call clifor_print_option(clifor_options_tail)
  end subroutine clifor_create_option


  subroutine clifor_print_option(option)
    type(clifor_type_option), intent(in) :: option
    character :: required
    character(len=:), allocatable :: name, name_with_value
    allocate(character(1) :: name)
    name = option%short//', '//option%long
    if (option%needvalue) then
      allocate(character(1) :: name_with_value)
      name_with_value = name//' <value>'
    end if
    required = merge('*', ' ', option%required)
    write(*,'(a)') name//'   '//option%description//' '//required
    ! write(*,*) 'needvalue:', option%needvalue
    ! write(*,*) 'value:', option%value
  end subroutine clifor_print_option


  subroutine clifor_process_options
    ! type(clifor_type_option), pointer :: option => null()
    character(len=:), allocatable :: argument !, argument_value
    type(clifor_type_option), pointer :: iterator
    integer :: index, total, options_index
    logical :: present
    allocate(clifor_options_present(clifor_options_length))
    total = command_argument_count()
    index = 1
    do
      if (index > total) exit
      ! write(*,*) index, total, size(clifor_options_present)
      argument = clifor_get_argument(index)
      ! if (len(argument) < 2) then
      !   call clifor_write_warning('ignoring invalid option: '//argument)
      !   index = index + 1
      !   cycle
      ! end if
      ! write(*,*) 'argument:'//argument(3:)
      iterator => clifor_options_head
      present = .false.
      options_index = 1
      do
        if (.not. associated(iterator)) exit
        if (argument == iterator%short .or. argument == iterator%long) then
          present = .true.
          clifor_options_present(options_index) = .true.
        end if
        ! if (argument(1:2) == '--') then
        !   ! write(*,*) 'long:'//iterator%long
        !   if (argument(3:) == iterator%long) then
        !     write(*,'(a)') argument(3:)
        !   else
        !     call clifor_write_warning('ignoring invalid option: '//argument)
        !   end if
        ! elseif (argument(1:1) == '-') then
        !   if (argument(2:) == iterator%short) write(*,'(a)') argument(2:)
        !   ! write(*,*) argument//' '//iterator%short
        ! end if
        iterator => iterator%next
        options_index = options_index + 1
      end do
      if (.not. present) then
        call clifor_write_error('invalid option: '//argument)
        stop
      ! else
      !   write(*,*) 'present: ', argument, present
      end if
      index = index + 1
    end do

    ! do index=1,clifor_options_length
    !   write(*,*) clifor_options_present(index)
    ! end do

    ! deallocate(clifor_options_present)
  end subroutine clifor_process_options


  ! subroutine clifor_show_help
  ! end subroutine clifor_show_help


  ! IDEA maybe change to a subroutine
  function clifor_get_option(option_name) result(present)
    character(len=*), intent(in) :: option_name
    ! type(clifor_type_option) :: option
    logical :: created, present
    integer :: i
    type(clifor_type_option), pointer :: temp
    created = .false.
    present = .false.
    temp => clifor_options_head
    do i=1, clifor_options_length
      if (.not. associated(temp)) exit
      if (option_name == temp%short(2:2) .or. option_name == temp%long(3:)) then
        created = .true.
        if (clifor_options_present(i)) then
          present = .true.
          return
        end if
      end if
      temp => temp%next
    end do
    if (.not. created) then
      call clifor_write_error('Trying to get a not created option: '//option_name)
      stop -3
    end if
    nullify(temp)
  end function clifor_get_option


  function clifor_isletter(char)
    character(len=1), intent(in) :: char
    logical :: clifor_isletter
    integer :: i
    i = ichar(char)
    if (i >= 65 .and. i <= 90) then
      clifor_isletter = .true.
    elseif (i >= 97 .and. i <= 122) then
      clifor_isletter = .true.
    else
      clifor_isletter = .false.
    end if
  end function clifor_isletter

end module clifor_options

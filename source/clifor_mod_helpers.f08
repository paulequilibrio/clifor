module clifor_mod_helpers
  use clifor_mod_write
  implicit none

  character(len=1), parameter :: TAB = achar(9)
  character(len=3), parameter :: NL = achar(10)//achar(13)//achar(0)

contains


  function clifor_is_letter(char) result(is_letter)
    character(len=1), intent(in) :: char
    logical :: is_letter
    integer :: i
    i = iachar(char)
    is_letter = (i >= 65 .and. i <= 90) .or. (i >= 97 .and. i <= 122)
  end function clifor_is_letter


  function clifor_is_digit(char) result(is_digit)
    character(len=1), intent(in) :: char
    logical :: is_digit
    integer :: i
    i = iachar(char)
    is_digit = (i >= 48 .and. i <= 57)
  end function clifor_is_digit


  function clifor_is_alphanumeric(char) result(is_alphanumeric)
    character(len=1), intent(in) :: char
    logical :: is_alphanumeric
    is_alphanumeric = clifor_is_letter(char) .or. clifor_is_digit(char)
  end function clifor_is_alphanumeric


  function clifor_has_only_alphanumeric_and_dash(long) result(only)
    character(len=*), intent(in) :: long
    integer :: i
    logical :: only
    only = .true.
    do i=1, len(long)
      only = only .and. (clifor_is_alphanumeric(long(i:i)) .or. long(i:i) == '-')
    end do
  end function clifor_has_only_alphanumeric_and_dash


  function clifor_is_valid_short_name(short) result(valid)
    character(len=*), intent(in) :: short
    logical :: valid
    valid = (len(short) == 1) .and. clifor_is_letter(short)
  end function clifor_is_valid_short_name


  function clifor_is_valid_long_name(long) result(valid)
    character(len=*), intent(in) :: long
    logical :: valid
    valid = len(long) > 1 .and. &
            clifor_is_alphanumeric(long(1:1)) .and. &
            clifor_has_only_alphanumeric_and_dash(long(2:))
  end function clifor_is_valid_long_name


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

end module clifor_mod_helpers

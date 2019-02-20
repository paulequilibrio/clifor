module clifor_mod_option
  use clifor_mod_helpers
  implicit none

  type, public :: clifor_type_option
    private
    ! set when create
    character(len=2) :: short
    character(len=:), allocatable :: long, description, value_name
    logical :: required, need_value
    ! maybe set when process
    logical :: provided
    character(len=:), allocatable :: value
  contains
    procedure :: set => create_new_option
    procedure :: get_short, get_long, get_description
    procedure :: get_required, get_need_value, get_value_name, to_string
    procedure :: has_short, has_long
    procedure to_string_write
    generic :: write(unformatted) => to_string_write
  end type clifor_type_option

  private

contains

  subroutine create_new_option(option, short, long, description, required, need_value, value_name)
    class(clifor_type_option), target :: option
    character(len=*), intent(in) :: short, long, description
    character(len=*), intent(in), optional :: value_name
    logical, intent(in), optional :: required, need_value

    if (.not. clifor_is_valid_short_name(short)) then
      call clifor_write_error('The short option should be a single letter')
      stop
    end if

    if (.not. clifor_is_valid_long_name(long)) then
      if (len(long) < 2) then
        call clifor_write_error('The long option should be at least 2 characters long')
        stop
      elseif (.not. clifor_is_alphanumeric(long(1:1))) then
        call clifor_write_error('The long option should start with letter or digit.')
        stop
      elseif (.not. clifor_has_only_alphanumeric_and_dash(long(2:))) then
        call clifor_write_error('The long option should contain only letters, digits or dash(-)')
        stop
      end if
    end if

    option%short = '-'//short
    option%long = '--'//long
    option%description = trim(adjustl(description))
    ! ternary: variable = merge(value if true, value if false, condition)
    option%required = merge(required, .false., present(required))
    option%need_value = merge(need_value, .false., present(need_value))

    if (option%need_value) then
      if (present(value_name)) then
        option%value_name = '<'//value_name//'>'
      else
        option%value_name = '<value>'
      end if
    else
      option%value_name = ''
    end if
  end subroutine create_new_option


  pure function get_short(option) result(short)
    class(clifor_type_option), intent(in) :: option
    character(len=:), allocatable :: short
    short = option%short
  end function get_short


  pure function get_long(option) result(long)
    class(clifor_type_option), intent(in) :: option
    character(len=:), allocatable :: long
    long = option%long
  end function get_long


  pure function get_description(option) result(description)
    class(clifor_type_option), intent(in) :: option
    character(len=:), allocatable :: description
    description = option%description
  end function get_description


  pure function get_required(option) result(required)
    class(clifor_type_option), intent(in) :: option
    logical :: required
    required = option%required
  end function get_required


  pure function get_need_value(option) result(need_value)
    class(clifor_type_option), intent(in) :: option
    logical :: need_value
    need_value = option%need_value
  end function get_need_value


  pure function get_value_name(option) result(value_name)
    class(clifor_type_option), intent(in) :: option
    character(len=:), allocatable :: value_name
    value_name = option%value_name
  end function get_value_name


  pure function to_string(option) result(string)
    class(clifor_type_option), intent(in) :: option
    character :: required
    character(len=:), allocatable :: temp, name, string

    allocate(character(0) :: temp)
    temp = option%short//', '//option%long

    required = merge('*', ' ', option%required)

    allocate(character(0) :: name)
    if (option%need_value) then
      name = temp//' '//option%value_name
    else
      name = temp
    end if

    string = required//name//', '//option%description
  end function to_string


  pure function has_short(option, short)
    class(clifor_type_option), intent(in) :: option
    character(len=*), intent(in) :: short
    logical :: has_short
    has_short = option%get_short() == short
  end function has_short


  pure function has_long(option, long)
    class(clifor_type_option), intent(in) :: option
    character(len=*), intent(in) :: long
    logical :: has_long
    has_long = option%get_long() == long
  end function has_long


  subroutine to_string_write(option, unit, iostat, iomsg)
    class(clifor_type_option), intent(in) :: option
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    write(unit, iostat=iostat, iomsg=iomsg) option%to_string()
  end subroutine to_string_write

end module clifor_mod_option

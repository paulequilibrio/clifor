module clifor_mod_type_option
  use clifor_mod_helpers

  implicit none

  private

  type, public :: clifor_type_option
    private
    ! set when create
    character(len=2) :: short
    character(len=:), allocatable :: long, description, value_name
    logical :: required, need_value
    ! maybe set when process
    logical :: provided = .false.
    character(len=:), allocatable :: value
  contains
    procedure :: set => create_new_option
    procedure :: get_short, get_long, get_description
    procedure :: get_required, get_need_value, get_value_name
    procedure :: has_short, has_long
    procedure :: get_name, get_name_length
    procedure :: write, print
    generic :: write(formatted) => write
    procedure :: set_provided, get_provided, set_value, get_value
  end type clifor_type_option

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


  pure function get_name(option) result(name)
    class(clifor_type_option), intent(in) :: option
    character(len=:), allocatable :: name
    character :: required
    required = merge('*', ' ', option%required)
    if (option%need_value) then
      name = required//' '//option%short//', '//option%long//' '//option%value_name
    else
      name = required//' '//option%short//', '//option%long
    end if
  end function get_name


  pure function get_name_length(option) result(length)
    class(clifor_type_option), intent(in) :: option
    integer :: length
    length = len(option%get_name())
  end function get_name_length


  pure function has_short(option, short)
    class(clifor_type_option), intent(in) :: option
    character(len=*), intent(in) :: short
    logical :: has_short
    has_short = option%short == '-'//short
  end function has_short


  pure function has_long(option, long)
    class(clifor_type_option), intent(in) :: option
    character(len=*), intent(in) :: long
    logical :: has_long
    has_long = option%long == '--'//long
  end function has_long


  subroutine write(option, unit, iotype, format, iostat, iomsg)
    class(clifor_type_option), intent(in) :: option
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: format(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    character(len=:), allocatable :: formatted, name
    character(len=16) :: fmt
    integer :: name_length

    name_length = option%get_name_length()
    fmt = 'a,1x,a'

    if (iotype == 'DT') then
      select case (size(format))
        case (1)
          write(fmt, '(a,i3.3,a)') 'a,tr', format(1), ',a'
        case (2)
          name_length = merge(format(2), name_length, format(2) > name_length)
          write(fmt, '(2(a,i3.3),a)') 'a', name_length, ',tr', format(1), ',a'
      end select
    end if

    allocate(character(len=2+len(trim(fmt))) :: formatted)
    formatted = '(' // trim(fmt) // ')'

    allocate(character(len=name_length) :: name)
    write(name, '(a)') option%get_name()

    write(unit, fmt=formatted, iostat=iostat, iomsg=iomsg) name, option%description

    deallocate(formatted, name)
  end subroutine write


  subroutine print(option, description_shift, name_length)
    class(clifor_type_option), intent(in) :: option
    integer, intent(in), optional :: description_shift, name_length
    character(len=16) :: format

    if (present(description_shift)) then
      if (present(name_length)) then
        write(format, '(2(a,i3.3),a)') '(2x,dt(', description_shift, ',', name_length, '))'
      else
        write(format, '(a,i3.3,a)') '(2x,dt(', description_shift, '))'
      end if
    else
      write(format, '(a)') '(2x,dt)'
    end if

    write(*, format) option
  end subroutine print


  subroutine set_provided(option, provided)
    class(clifor_type_option), intent(inout) :: option
    logical, intent(in) :: provided
    option%provided = provided
  end subroutine set_provided


  pure function get_provided(option) result(provided)
    class(clifor_type_option), intent(in) :: option
    logical :: provided
    provided = option%provided
  end function get_provided


  subroutine set_value(option, value)
    class(clifor_type_option), intent(inout) :: option
    character(len=*), intent(in) :: value
    if (option%need_value) option%value = value
  end subroutine set_value


  pure function get_value(option) result(value)
    class(clifor_type_option), intent(in) :: option
    character(len=:), allocatable :: value
    if (option%need_value) then
      value = option%value
    else
      value = ''
    end if
  end function get_value


end module clifor_mod_type_option

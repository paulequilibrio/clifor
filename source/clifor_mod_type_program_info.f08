module clifor_mod_program_info

  implicit none

  private

  type, public :: clifor_type_program_info
    private
    character(len=:), allocatable :: name, version, pretty_name, description
  contains
    procedure :: set => set_program_info
    procedure :: get_name, get_version, get_pretty_name, get_description
    procedure :: write, print
    generic :: write(formatted) => write
  end type clifor_type_program_info

  ! TODO: add author info type

contains

  subroutine set_program_info(info, name, version, pretty_name, description)
    class(clifor_type_program_info), intent(inout) :: info
    character(len=*), intent(in) :: name, version
    character(len=*), intent(in), optional :: pretty_name, description
    info%name = name
    info%version = version
    if (present(pretty_name)) info%pretty_name = pretty_name
    if (present(description)) info%description = description
  end subroutine set_program_info


  pure function get_name(info) result(name)
    class(clifor_type_program_info), intent(in) :: info
    character(len=:), allocatable :: name
    name = info%name
  end function get_name


  pure function get_version(info) result(version)
    class(clifor_type_program_info), intent(in) :: info
    character(len=:), allocatable :: version
    version = info%version
  end function get_version


  pure function get_pretty_name(info) result(pretty_name)
    class(clifor_type_program_info), intent(in) :: info
    character(len=:), allocatable :: pretty_name
    pretty_name = info%pretty_name
  end function get_pretty_name


  pure function get_description(info) result(description)
    class(clifor_type_program_info), intent(in) :: info
    character(len=:), allocatable :: description
    description = info%description
  end function get_description


  subroutine write(info, unit, iotype, format, iostat, iomsg)
    class(clifor_type_program_info), intent(in) :: info
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: format(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    character(len=:), allocatable :: name
    if (iotype == 'DT' .and. size(format) == 0) continue

    allocate(character(0) :: name)
    name = info%name
    if (info%pretty_name /= '') name = info%pretty_name

    write(unit, fmt='(2(a,1x),/a)', iostat=iostat, iomsg=iomsg) &
      name, '(v'//info%version//')', info%description
  end subroutine write


  subroutine print(info)
    class(clifor_type_program_info), intent(in) :: info
    write(*, '(dt)') info
  end subroutine print

end module clifor_mod_program_info

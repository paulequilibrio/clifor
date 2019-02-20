module clifor_mod_program_info

  implicit none

  type :: clifor_type_program_info
    private
    character(len=:), allocatable :: &
      name, &
      version, &
      pretty_name, &
      description
  contains
    procedure :: set => set_program_info
    procedure :: get_name => get_program_name
    procedure :: get_version => get_program_version
    procedure :: get_pretty_name => get_program_pretty_name
    procedure :: get_description => get_program_description
  end type clifor_type_program_info

  private &
    set_program_info, &
    get_program_name, &
    get_program_version, &
    get_program_pretty_name, &
    get_program_description

contains

  subroutine set_program_info(info, name, version, pretty_name, description)
    class(clifor_type_program_info) :: info
    character(len=*), intent(in) :: name, version
    character(len=*), intent(in), optional :: pretty_name, description
    ! TODO: not allow spaces in name
    info%name = name
    info%version = version
    if (present(pretty_name)) info%pretty_name = pretty_name
    if (present(description)) info%description = description
  end subroutine set_program_info

  function get_program_name(info) result(name)
    class(clifor_type_program_info) :: info
    character(len=:), allocatable :: name
    name = info%name
  end function

  function get_program_version(info) result(version)
    class(clifor_type_program_info) :: info
    character(len=:), allocatable :: version
    version = info%version
  end function

  function get_program_pretty_name(info) result(pretty_name)
    class(clifor_type_program_info) :: info
    character(len=:), allocatable :: pretty_name
    pretty_name = info%pretty_name
  end function

  function get_program_description(info) result(description)
    class(clifor_type_program_info) :: info
    character(len=:), allocatable :: description
    description = info%description
  end function

end module clifor_mod_program_info

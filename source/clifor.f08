module clifor
  use clifor_types
  use clifor_write
  use clifor_options

  implicit none

  type(clifor_type_program_info) :: clifor_program_info

contains


  subroutine clifor_set_program_info(name, version, pretty_name, description)
    character(len=*), intent(in) :: name, version
    character(len=*), intent(in), optional :: pretty_name, description
    clifor_program_info%name = name
    clifor_program_info%version = version
    if (present(pretty_name)) clifor_program_info%pretty_name = pretty_name
    if (present(description)) clifor_program_info%description = description
  end subroutine clifor_set_program_info


  function clifor_get_program_info() result(info)
    type(clifor_type_program_info) :: info
    info = clifor_program_info
  end function clifor_get_program_info


  subroutine clifor_show_program_version
    call clifor_write_stdout(clifor_program_info%version)
    stop
  end subroutine clifor_show_program_version


  subroutine clifor_show_program_help
    character(len=:), allocatable :: name
    type(clifor_type_program_info) :: i
    i = clifor_program_info
    allocate(character(0) :: name)
    name = i%name
    if (i%pretty_name /= '') name = i%pretty_name
    call clifor_write_stdout(name//' (v'//i%version//') - '//i%description)
    stop
  end subroutine clifor_show_program_help

end module clifor

module clifor_mod_write
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit

  implicit none

  private clifor_write

contains

  subroutine clifor_write(unit, message, format_to_use)
    integer, intent(in) :: unit
    class(*), intent(in) :: message
    character(len=*), optional :: format_to_use
    character(len=:), allocatable :: format

    allocate(character(0) :: format)
    format = format_to_use

    select type (message)
      type is (character(len=*))
        if (format == '') format = '(a)'
        write(unit, format) message
      type is (integer)
        if (format == '') format = '(i9)'
        write(unit, format) message
      type is (real)
        if (format == '') format = '(f9.3)'
        write(unit, format) message
    end select
  end subroutine clifor_write


  subroutine clifor_write_stdout(message, format)
    class(*), intent(in) :: message
    character(len=*), intent(in), optional :: format
    call clifor_write(output_unit, message, format)
  end subroutine clifor_write_stdout

  subroutine clifor_write_stderr(message, format)
    class(*), intent(in) :: message
    character(len=*), intent(in), optional :: format
    call clifor_write(error_unit, message, format)
  end subroutine clifor_write_stderr


  subroutine clifor_write_info(message)
    character(len=*), intent(in) :: message
    call clifor_write_stdout('[ INFO ] '//message)
  end subroutine clifor_write_info

  subroutine clifor_write_warning(message)
    character(len=*), intent(in) :: message
    call clifor_write_stdout('[ WARNING ] '//message)
  end subroutine clifor_write_warning

  subroutine clifor_write_error(message)
    character(len=*), intent(in) :: message
    call clifor_write_stderr('[ ERROR ] '//message)
  end subroutine clifor_write_error

end module clifor_mod_write

module clifor_write

  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
  implicit none

  interface clifor_write_stderr
    module procedure clifor_write_stderr_string
    module procedure clifor_write_stderr_integer
  end interface clifor_write_stderr

contains

  subroutine clifor_write_stdout(message, format)
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: format
    if (present(format)) then
      write(output_unit, format) message
    else
      write(output_unit, '(a)') message
    end if
  end subroutine clifor_write_stdout

  subroutine clifor_write_stderr_string(message, format)
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: format
    if (present(format)) then
      write(error_unit, format) message
    else
      write(error_unit, '(a)') message
    end if
  end subroutine clifor_write_stderr_string

  subroutine clifor_write_stderr_integer(message, format)
    integer, intent(in) :: message
    character(len=*), intent(in), optional :: format
    if (present(format)) then
      write(error_unit, format) message
    else
      write(error_unit, '(i4)') message
    end if
  end subroutine clifor_write_stderr_integer

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

end module clifor_write

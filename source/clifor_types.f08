module clifor_types

  implicit none

  type :: clifor_type_program_info
    character(len=:), allocatable :: name, version, pretty_name, description
  end type clifor_type_program_info

  type :: clifor_type_option
    ! set when create
    ! private
    character(len=2) :: short
    character(len=:), allocatable :: long
    character(len=:), allocatable :: description
    logical :: required
    logical :: needvalue
    type(clifor_type_option), pointer :: next => null()
    ! maybe set when process
    logical :: provided
    character(len=:), allocatable :: value
  end type clifor_type_option

  type :: clifor_type_list
    private
    integer :: length = 0
    type(clifor_type_option), pointer :: head => null()
    type(clifor_type_option), pointer :: tail => null()
  contains
    procedure, pass :: deallocate => free_memory
  end type clifor_type_list

contains

  subroutine free_memory(this)
    class(clifor_type_list), intent(in) :: this
  end subroutine free_memory

end module clifor_types

module clifor_mod_list

  implicit none

  private

  type, public :: clifor_type_node
    private
    class(*), pointer :: data => null()
    type(clifor_type_node), pointer :: previous => null()
    type(clifor_type_node), pointer :: next => null()
  contains
    procedure :: get_data => node_get_data
    procedure :: get_previous => node_get_previous
    procedure :: get_next => node_get_next
    procedure :: deallocate => node_deallocate
    final :: node_finalizer
  end type clifor_type_node

  type, public :: clifor_type_list
    private
    integer :: length = 0
    type(clifor_type_node), pointer :: head => null()
    type(clifor_type_node), pointer :: tail => null()
  contains
    procedure :: get_head => list_get_head
    procedure :: len => list_length
    procedure :: add => list_add_node
    procedure :: for_each => list_for_each
    ! procedure :: filter => list_filter
    procedure :: deallocate => list_deallocate
    final :: list_finalizer
  end type clifor_type_list

  abstract interface
    subroutine iterator_each(node, done)
      import :: clifor_type_node
      type(clifor_type_node), intent(inout), pointer  :: node
      logical, intent(out) :: done
    end subroutine iterator_each
  end interface

contains

  subroutine list_for_each(list, subroutine_each)
    class(clifor_type_list), intent(inout) :: list
    procedure(iterator_each)  :: subroutine_each
    type(clifor_type_node), pointer :: node
    logical :: done
    done = .false.
    node => list%head
    do
      if (associated(node)) then
        call subroutine_each(node, done)
        if (done) exit
        node => node%next
      else
        exit
      end if
    end do
  end subroutine list_for_each


  function list_get_head(list) result(head)
    class(clifor_type_list) :: list
    type(clifor_type_node), pointer :: head
    ! allocate(head, source=list%head)
    head => list%head
  end function list_get_head


  function list_length(list)
    class(clifor_type_list) :: list
    integer :: list_length
    list_length = list%length
  end function list_length


  subroutine list_add_node(list, data)
    class(clifor_type_list), intent(inout) :: list
    class(*), intent(in) :: data
    class(*), pointer :: data_pointer
    type(clifor_type_node), pointer :: node

    allocate(data_pointer, source=data)

    if (.not. associated(list%tail)) then
      allocate(list%head)
      node => list%head
    else
      allocate(list%tail%next)
      node => list%tail%next
      node%previous => list%tail
    end if

    list%tail => node
    node%data => data_pointer

    list%length = list%length + 1
    nullify(data_pointer)
  end subroutine list_add_node


  subroutine list_deallocate(list)
    class(clifor_type_list), intent(inout) :: list
    type(clifor_type_node), pointer :: node, previous
    ! write(*,'(a)') 'Deallocating LIST...'
    node => list%tail
    do
      if (associated(node)) then
        previous => node%previous
        call node%deallocate
        node => previous
      else
        exit
      end if
    end do
    list%length = 0
  end subroutine list_deallocate


  subroutine list_finalizer(list)
    type(clifor_type_list), intent(inout) :: list
    call list%deallocate
  end subroutine list_finalizer


  function node_get_data(node) result(data)
    class(clifor_type_node), intent(in) :: node
    class(*), pointer :: data
    data => node%data
  end function node_get_data


  function node_get_previous(node) result(previous)
    class(clifor_type_node) :: node
    type(clifor_type_node), pointer :: previous
    previous => node%previous
  end function node_get_previous


  function node_get_next(node) result(next)
    class(clifor_type_node) :: node
    type(clifor_type_node), pointer :: next
    next => node%next
  end function node_get_next


  subroutine node_deallocate(node)
    class(clifor_type_node), intent(inout) :: node
    ! write(*,'(a)') 'Deallocating NODE...'
    if (associated(node%data)) deallocate(node%data)
    ! if (associated(node%previous)) deallocate(node%previous)
    ! if (associated(node%next)) deallocate(node%next)
    nullify(node%previous)
    nullify(node%next)
    nullify(node%data)
  end subroutine node_deallocate


  subroutine node_finalizer(node)
    type(clifor_type_node), intent(inout) :: node
    call node%deallocate
  end subroutine node_finalizer

end module clifor_mod_list

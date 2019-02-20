module clifor_mod_list

  implicit none

  type :: clifor_type_node
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

  private &
    node_get_data, node_get_previous, node_get_next, &
    node_deallocate, node_finalizer


  type :: clifor_type_list
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

  private &
    list_get_head, &
    list_length, &
    list_add_node, &
    list_for_each, &
    list_deallocate, &
    list_finalizer
    ! list_filter, &


  abstract interface
    subroutine iterator_each(node, done)
      import :: clifor_type_node
      type(clifor_type_node), intent(inout), pointer  :: node
      logical, intent(out) :: done
    end subroutine iterator_each

    ! subroutine iterator_filter(value, node, done)
    !   import :: clifor_type_node
    !   class(*), intent(in), pointer :: value
    !   type(clifor_type_node), intent(inout), pointer  :: node
    !   logical, intent(out) :: done
    ! end subroutine iterator_filter
  end interface

contains

! LIST PROCEDURES

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


  ! subroutine list_filter(list, subroutine_filter)
  !   class(clifor_type_list), intent(inout) :: list
  !   procedure(iterator_filter)  :: subroutine_filter
  !   class(*), pointer :: value
  !   type(clifor_type_node), pointer :: node
  !   logical :: done
  !   done = .false.
  !   node => list%head
  !   do
  !     if (associated(node)) then
  !       call subroutine_filter(value, node, done)
  !       if (done) exit
  !       node => node%next
  !     else
  !       exit
  !     end if
  !   end do
  ! end subroutine list_filter


  ! subroutine show_all(list)
  !   class(clifor_type_list), intent(inout) :: list
  !   write(*, '(a,i2,a)') NL//'=> The list has ', list%length, ' items:'//NL
  !   call list%for_each(show)
  ! end subroutine show_all
  !
  ! ! Can't be here
  ! subroutine show(actual_node, done)
  !   type(clifor_type_node), intent(inout), pointer :: actual_node
  !   logical, intent(out) :: done
  !   done = .false.
  !   select type (item => actual_node%data)
  !   type is (clifor_type_option)
  !     write(*,'(a)') item%to_string()
  !   end select
  ! end subroutine show


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
        list%length = list%length - 1
        node => previous
      else
        exit
      end if
    end do
  end subroutine list_deallocate


  subroutine list_finalizer(list)
    type(clifor_type_list), intent(inout) :: list
    call list%deallocate
  end subroutine list_finalizer



! NODE PROCEDURES

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

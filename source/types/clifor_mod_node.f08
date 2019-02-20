! module clifor_mod_node
!   implicit none
!
!   type :: clifor_type_node
!     ! private
!     class(*), pointer :: data => null()
!     type(clifor_type_node), pointer :: previous => null()
!     type(clifor_type_node), pointer :: next => null()
!   contains
!     procedure :: set, set_previous, set_next, get_data
!     ! procedure :: is_equal, is_different
!     ! generic :: operator(==) => is_equal
!     ! generic :: operator(/=) => is_different
!     ! procedure :: set_next, get_next, has_next
!     final :: deallocate_node
!   end type clifor_type_node
!
! contains
!
!   subroutine set(node, data)
!     class(clifor_type_node), intent(inout) :: node
!     class(*), intent(in), target :: data
!     node%data => data
!   end subroutine set
!
!   subroutine set_previous(node, previous)
!     class(clifor_type_node), intent(inout) :: node
!     type(clifor_type_node), intent(in), target :: previous
!     node%previous => previous
!   end subroutine set_previous
!
!   subroutine set_next(node, next)
!     class(clifor_type_node), intent(inout) :: node
!     type(clifor_type_node), intent(in), target :: next
!     node%next => next
!   end subroutine set_next
!
!
!   function get_data(node) result(data)
!     class(clifor_type_node), intent(in) :: node
!     class(*), pointer :: data
!     data => node%data
!   end function get_data
!
!
!   subroutine deallocate_node(node)
!     type(clifor_type_node), intent(inout) :: node
!     if (associated(node%previous)) deallocate(node%previous)
!     if (associated(node%next)) deallocate(node%next)
!     if (associated(node%data)) deallocate(node%data)
!
!     nullify(node%previous)
!     nullify(node%next)
!     nullify(node%data)
!   end subroutine deallocate_node
!
!
! end module clifor_mod_node

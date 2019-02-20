! module clifor_mod_options
!   use clifor_mod_write
!   ! use clifor_options_list
!
!   implicit none
!
!   ! type(clifor_type_option), pointer :: clifor_options_head => null()
!   ! type(clifor_type_option), pointer :: clifor_options_tail => null()
!   ! logical, dimension(:), allocatable :: clifor_options_present
!   ! integer :: clifor_options_length
!
!   ! type(clifor_type_list) :: clifor_list
!
! contains
!
!   ! subroutine clifor_process_options
!   !   ! type(clifor_type_option), pointer :: option => null()
!   !   character(len=:), allocatable :: argument !, argument_value
!   !   type(clifor_type_option), pointer :: iterator
!   !   integer :: index, total, options_index
!   !   logical :: present
!   !   allocate(clifor_options_present(clifor_options_length))
!   !   total = command_argument_count()
!   !   index = 1
!   !   do
!   !     if (index > total) exit
!   !     ! write(*,*) index, total, size(clifor_options_present)
!   !     argument = clifor_get_argument(index)
!   !     ! if (len(argument) < 2) then
!   !     !   call clifor_write_warning('ignoring invalid option: '//argument)
!   !     !   index = index + 1
!   !     !   cycle
!   !     ! end if
!   !     ! write(*,*) 'argument:'//argument(3:)
!   !     iterator => clifor_options_head
!   !     present = .false.
!   !     options_index = 1
!   !     do
!   !       if (.not. associated(iterator)) exit
!   !       if (argument == iterator%short .or. argument == iterator%long) then
!   !         present = .true.
!   !         clifor_options_present(options_index) = .true.
!   !       end if
!   !       ! if (argument(1:2) == '--') then
!   !       !   ! write(*,*) 'long:'//iterator%long
!   !       !   if (argument(3:) == iterator%long) then
!   !       !     write(*,'(a)') argument(3:)
!   !       !   else
!   !       !     call clifor_write_warning('ignoring invalid option: '//argument)
!   !       !   end if
!   !       ! elseif (argument(1:1) == '-') then
!   !       !   if (argument(2:) == iterator%short) write(*,'(a)') argument(2:)
!   !       !   ! write(*,*) argument//' '//iterator%short
!   !       ! end if
!   !       iterator => iterator%next
!   !       options_index = options_index + 1
!   !     end do
!   !     if (.not. present) then
!   !       call clifor_write_error('invalid option: '//argument)
!   !       stop
!   !     ! else
!   !     !   write(*,*) 'present: ', argument, present
!   !     end if
!   !     index = index + 1
!   !   end do
!   !
!   !   ! do index=1,clifor_options_length
!   !   !   write(*,*) clifor_options_present(index)
!   !   ! end do
!   !
!   !   ! deallocate(clifor_options_present)
!   ! end subroutine clifor_process_options
!
!
!   ! subroutine clifor_show_help
!   ! end subroutine clifor_show_help
!
!
!   ! IDEA maybe change to a subroutine
!   ! function clifor_get_option(option_name) result(present)
!   !   character(len=*), intent(in) :: option_name
!   !   ! type(clifor_type_option) :: option
!   !   logical :: created, present
!   !   integer :: i
!   !   type(clifor_type_option), pointer :: temp
!   !   created = .false.
!   !   present = .false.
!   !   temp => clifor_options_head
!   !   do i=1, clifor_options_length
!   !     if (.not. associated(temp)) exit
!   !     if (option_name == temp%short(2:2) .or. option_name == temp%long(3:)) then
!   !       created = .true.
!   !       if (clifor_options_present(i)) then
!   !         present = .true.
!   !         return
!   !       end if
!   !     end if
!   !     temp => temp%next
!   !   end do
!   !   if (.not. created) then
!   !     call clifor_write_error('Trying to get a not created option: '//option_name)
!   !     stop -3
!   !   end if
!   !   nullify(temp)
!   ! end function clifor_get_option
!
!
!
!
! end module clifor_mod_options

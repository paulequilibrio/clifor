module test_clifor_mod_type_list
  use fruit

  implicit none

contains

  subroutine test_clifor_mod_type_list_get_head
    use clifor_mod_type_list, only: clifor_type_node, clifor_type_list
    use clifor, only: show_all
    type(clifor_type_node) :: head
    type(clifor_type_list) :: list
    call set_unit_name('list get head')
    ! call assert_equals(.false., associated(head))
    ! head = list%get_head()
    ! call assert_equals(.false., associated(head))
    call assert_equals(0, list%len())
    ! call show_all(list)
  end subroutine test_clifor_mod_type_list_get_head


  subroutine test_clifor_mod_type_list_add_first_option
    use clifor_mod_type_list, only: clifor_type_node, clifor_type_list
    use clifor_mod_type_option, only: clifor_type_option
    use clifor, only: show_all
    type(clifor_type_option) :: option
    type(clifor_type_node) :: head, node
    type(clifor_type_list) :: list
    call set_unit_name('list add first option')

    call set_case_name('list starts empty')
    call assert_equals(0, list%len())

    call set_case_name('list has 1 item')
    call option%set('v', 'version', 'Display version and exit')
    call list%add(option)

    call set_case_name('list has 2 item')
    call option%set('x', 'xx', '')
    call list%add(option)
    call assert_equals(2, list%len())
    call option%set('y', 'yy', '')
    call list%add(option)
    call option%set('z', 'zz', '')
    call list%add(option)

    call set_case_name('list head has no next')
    ! head = list%get_head()
    ! write(*,*) same_type_as(head, node)
    ! call assert_equals(.false., head%has_next())
    ! ! call assert_equals(.true., option == option2)
    !
    ! call show_all(list)
  end subroutine test_clifor_mod_type_list_add_first_option

end module test_clifor_mod_type_list

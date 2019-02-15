program fruit_driver
  use fruit
  use test_clifor
  call init_fruit(1)
  call fruit_show_dots

  call test_clifor_program_info

  call test_clifor_get_argument
  call test_clifor_get_argument_value

  call test_clifor_isletter
  call test_clifor_create_option
  ! call test_clifor_get_option


  call fruit_summary
  call fruit_finalize
end program fruit_driver

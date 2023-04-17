module test_repot_args_unitTests_get
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string, get_all_actual_value
    use :: repot_args
    implicit none
    private
    public :: get_format_returns_component_format
    public :: get_filename_returns_component_filename

contains
    subroutine get_format_returns_component_format(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(repository_constructor_arguments_type) :: args

        args = new_args(format="test")

        call check(error, args%get_format() == "test", &
                   "expected 'test' but got '"//args%get_format()//"'")
    end subroutine get_format_returns_component_format

    subroutine get_filename_returns_component_filename(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(repository_constructor_arguments_type) :: args

        args = new_args(format="test", filename="wafejkf.fweioa")

        call check(error, args%get_filename() == "wafejkf.fweioa", &
                   "expected 'wafejkf.fweioa' but got '"//args%get_filename()//"'")
    end subroutine get_filename_returns_component_filename
end module test_repot_args_unitTests_get

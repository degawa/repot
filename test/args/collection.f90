module test_repot_args_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_repot_args_unitTests_get
    implicit none
    private
    public :: collect_args

contains
    subroutine collect_args(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("get_format(), "// &
                                  "it should return format, a component of repository_constructor_arguments_type", &
                                  get_format_returns_component_format) &
                     , new_unittest("get_filename(), "// &
                                    "it should return filename, a component of repository_constructor_arguments_type.", &
                                    get_filename_returns_component_filename) &
                     ]
    end subroutine collect_args
end module test_repot_args_collection

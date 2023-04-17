module repot_args
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: new_args

    type, public :: repository_constructor_arguments_type
        character(:), private, allocatable :: format
            !! repository format
        character(:), private, allocatable :: filename
            !! filename of the repository (ex: name of a configuration file)
    contains
        procedure, public, pass :: get_format
        !* returns repository format
        procedure, public, pass :: get_filename
        !* returns filename of the repository
    end type repository_constructor_arguments_type

    interface new_args
        procedure :: construct_args
        procedure :: construct_args_filename
    end interface
contains
    !> returns a `repository_constructor_arguments_type` instance
    !> of which the component `format` is set.
    function construct_args(format) result(args)
        implicit none
        character(*), intent(in) :: format
            !! repository format

        type(repository_constructor_arguments_type) :: args
            !! new instance

        args%format = trim(format)
    end function construct_args

    !> returns a `repository_constructor_arguments_type` instance.
    function construct_args_filename(format, filename) result(args)
        implicit none
        character(*), intent(in) :: format
            !! repository format
        character(*), intent(in) :: filename
            !! filename of the repoistory

        type(repository_constructor_arguments_type) :: args
            !! new instance

        args%format = trim(format)
        args%filename = trim(filename)
    end function construct_args_filename

    !> returns the repository format.
    function get_format(this) result(format)
        implicit none
        class(repository_constructor_arguments_type), intent(in) :: this
        character(:), allocatable :: format

        if (.not. allocated(this%format)) then
            format = ""
            return
        end if

        format = this%format
    end function get_format

    !> returns the filename of the repository
    function get_filename(this) result(filename)
        implicit none
        class(repository_constructor_arguments_type), intent(in) :: this
        character(:), allocatable :: filename

        if (.not. allocated(this%format)) then
            filename = ""
            return
        end if

        filename = this%filename
    end function get_filename
end module repot_args

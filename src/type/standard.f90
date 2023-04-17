module repot_type_standard
    use, intrinsic :: iso_fortran_env
    use :: repot_args
    implicit none
    private

    type, public, abstract :: repository_atype
    contains
        procedure(IConstruct), public, pass, deferred :: construct
        !* constructs extended type

        procedure(IFind), public, pass, deferred :: find
        !* returns presence status of the value identified based on the ID

        procedure(IGet_int32), public, pass, deferred :: get_int32
        !* returns the int32 value identified based on the ID
        procedure(IGet_real64), public, pass, deferred :: get_real64
        !* returns the real64 value identified based on the ID
        procedure(IGet_logical), public, pass, deferred :: get_logical
        !* returns the logical value identified based on the ID
        procedure(IGet_string_alloc), public, pass, deferred :: get_string_alloc
        !* returns the string value identified based on the ID

        generic :: get => &
            get_int32, &
            get_real64, &
            get_logical, &
            get_string_alloc
    end type repository_atype

    abstract interface
        subroutine IConstruct(this, args)
            import repository_atype
            import repository_constructor_arguments_type
            class(repository_atype), intent(inout) :: this
            class(repository_constructor_arguments_type), intent(in) :: args
        end subroutine IConstruct

        function IFind(this, id) result(found)
            import repository_atype
            class(repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            logical :: found
        end function IFind

        subroutine IGet_int32(this, id, val)
            use, intrinsic :: iso_fortran_env
            import repository_atype
            class(repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            integer(int32), intent(out) :: val
        end subroutine IGet_int32

        subroutine IGet_real64(this, id, val)
            use, intrinsic :: iso_fortran_env
            import repository_atype
            class(repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            real(real64), intent(out) :: val
        end subroutine IGet_real64

        subroutine IGet_logical(this, id, val)
            import repository_atype
            class(repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            logical, intent(out) :: val
        end subroutine IGet_logical

        subroutine IGet_string_alloc(this, id, val)
            import repository_atype
            class(repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            character(:), allocatable, intent(out) :: val
        end subroutine IGet_string_alloc
    end interface
end module repot_type_standard

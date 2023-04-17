module repot_type_base
    use, intrinsic :: iso_fortran_env
    use :: repot_args
    implicit none
    private

    type, public, abstract :: base_repository_atype
    contains
        procedure(IConstruct), public, pass, deferred :: construct
        !* constructs extended type

        procedure(IFind), public, pass, deferred :: find
        !* returns presence status of the value identified based on the ID

        procedure(IGet_int32), public, pass, deferred :: get_int32
        !* returns the int32 value identified based on the ID
        procedure(IGet_real64), public, pass, deferred :: get_real64
        !* returns the real64 value identified based on the ID

        generic :: get => &
            get_int32, &
            get_real64
    end type base_repository_atype

    abstract interface
        subroutine IConstruct(this, args)
            import base_repository_atype
            import repository_constructor_arguments_type
            class(base_repository_atype), intent(inout) :: this
            class(repository_constructor_arguments_type), intent(in) :: args
        end subroutine IConstruct

        function IFind(this, id) result(found)
            import base_repository_atype
            class(base_repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            logical :: found
        end function IFind

        subroutine IGet_int32(this, id, val)
            use, intrinsic :: iso_fortran_env
            import base_repository_atype
            class(base_repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            integer(int32), intent(out) :: val
        end subroutine IGet_int32

        subroutine IGet_real64(this, id, val)
            use, intrinsic :: iso_fortran_env
            import base_repository_atype
            class(base_repository_atype), intent(inout) :: this
            character(*), intent(in) :: id
            real(real64), intent(out) :: val
        end subroutine IGet_real64
    end interface
end module repot_type_base

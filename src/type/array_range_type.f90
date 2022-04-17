!| The `type_array_range` module provides user-defined type related to a lower and upper bound of an array.
!
module type_array_range
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !| base type to store array range
    type, public :: array_range_type
        integer(int32), public :: lower = 0
            !! lower bound of an array
        integer(int32), public :: upper = 0
            !! upper bound of an array
    end type array_range_type

end module type_array_range

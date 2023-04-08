!>This module provides a user-defined type related to an array's lower and upper bound.
!>
module arrayRange_type
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !>base type for storing array range
    type, public :: array_range_type
        integer(int32), public :: lower = 0
            !! lower bound of an array
        integer(int32), public :: upper = 0
            !! upper bound of an array
    end type array_range_type
end module arrayRange_type

!>This module provides user-defined types and constructors related to 1d array bounds.
!>
module arrayRange_type_1d
    use, intrinsic :: iso_fortran_env
    use :: arrayRange_type
    implicit none
    private
    public :: to_array1d_range
    public :: to_scalar1d_range
    public :: to_vector1d_range

    integer(int32), private, parameter :: dim = 1
        !! a parameter defining dimensionality

    !>a type for storing the lower and upper bound of a 1d array
    type, public :: array1d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bound of rank-1 array
    contains
        procedure, public, pass :: array1d_range_to_array
        !* returns the lower and upper bound as a 1d array.
        generic :: to_array => array1d_range_to_array
        !* generic interface to call like `bounds(:) = range%to_array()`
    end type array1d_range_type

    !>a type for storing the lower and upper bound of a 1d scalar variable
    type, public :: scalar1d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bound of rank-1 array
    end type scalar1d_range_type

    !>a type for storing the lower and upper bound of a 1d vector variable
    type, public :: vector1d_range_type
        type(array1d_range_type), public :: x
            !! the array range of a 1d vector variable
    end type vector1d_range_type

contains
    !>returns the lower and upper bound as a 1d array.
    !>The array has 2 elements;
    !>the bounds are arranged as `[lower bound, upper bound]`.
    !>
    !>### Example
    !>```Fortran
    !>   real(real64) :: array(-10:10)
    !>   type(array1d_range_type) :: range
    !>   integer :: bounds(2)
    !>
    !>   ! get bounds of `array` and
    !>   ! create an array1d_range_type object
    !>   range = to_array1d_range([lbound(array),ubound(array)])
    !>
    !>   bounds = range%to_array()
    !>   print *, bounds ! -10 10
    !>```
    !> the bounds are arranged as `[lower bound, upper bound]`
    function array1d_range_to_array(this) result(new_range_array)
        implicit none
        class(array1d_range_type), intent(in) :: this
            !! passed dummy argument

        integer(int32) :: new_range_array(dim*2)
            !! a 1d array containing lower bound and upper bound
            !! which are arranged as `[lower bound, upper bound]`.

        !&<
        new_range_array(    1:dim  ) = this%rank(:)%lower
        new_range_array(dim+1:dim*2) = this%rank(:)%upper
        !&>
    end function array1d_range_to_array

    !>returns an `array1d_range_type` instance
    !>from an array containing the lower and upper bound.
    function to_array1d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bound of 1d array.<br>
            !! `bounds` must have 2 elements, and the bounds must be
            !! arranged as `[lower bound, upper bound]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(1d array), ubound(1d array)]`

        type(array1d_range_type) :: new_range
            !! an `array1d_range_type` instance.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_array1d_range

    !>returns a `scalar1d_range_type` instance
    !>from an array containing the lower and upper bound.
    function to_scalar1d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bound of a scalar variable.<br>
            !! `bounds` must have 2 elements, and the bounds must be
            !! arranged as `[lower bound, upper bound]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(1d array), ubound(1d array)]`

        type(scalar1d_range_type) :: new_range
            !! a `scalar1d_range_type` instance.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_scalar1d_range

    !>returns a `vector1d_range_type` instance
    !>from an array containing the lower and upper bound.
    function to_vector1d_range(x_bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: x_bounds(dim*2)
            !! the lower and upper bound of the x-component of a 1d vector.<br>
            !! `bounds` must have 2 elements, and the bounds must be
            !! arranged as `[lower bound, upper bound]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(1d array), ubound(1d array)]`

        type(vector1d_range_type) :: new_range
            !! an array1d_range_type object as the return value.

        !&<
        new_range%x%rank(:)%lower = x_bounds(    1:dim  )
        new_range%x%rank(:)%upper = x_bounds(dim+1:dim*2)
        !&>
    end function to_vector1d_range
end module arrayRange_type_1d

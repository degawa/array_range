!| The `type_array1d_range` module provides user-defined type and constructor related to 1d-array bounds.
!
module type_array1d_range
    use, intrinsic :: iso_fortran_env
    use :: type_array_range
    implicit none
    private
    public :: to_array1d_range

    integer(int32), private, parameter :: dim = 1
        !! a parameter defining dimensionality

    !| a type to store lower and upper bound of a 1d-array
    type, public :: array1d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bound of rank-1 array
    contains
        procedure, public, pass :: array1d_range_to_array
            !! returns lower and upper bound as 1d-array.
            !! the array has 2 elements
            !! and the bounds are arranged as
            !! `[lower bound, upper bound]`.
        generic :: as_array => array1d_range_to_array
            !! generic interface to be able to call
            !! like `bounds(:) = range%as_array()`
    end type array1d_range_type

contains
    !| returns lower and upper bound as 1d-array.
    !### Example
    !```Fortran
    !   real(real64) :: array(-10:10)
    !   type(array1d_range_type) :: range
    !   integer :: bounds(2)
    !
    !   ! get bounds of `array` and
    !   ! create an array1d_range_type object
    !   range = to_array1d_range([lbound(array),ubound(array)])
    !
    !   bounds = range%as_array()
    !   print *, bounds ! -10 10
    !```
    ! the bounds are arranged as `[lower bound, upper bound]`
    function array1d_range_to_array(this) result(new_range_array)
        implicit none
        class(array1d_range_type), intent(in) :: this
            !! passed dummy argument

        integer(int32) :: new_range_array(dim*2)
            !! 1d-array as the return value.<br>
            !! The retval contains lower bound and upper bound
            !! which are arranged as `[lower bound, upper bound]`.

        !&<
        new_range_array(    1:dim  ) = this%rank(:)%lower
        new_range_array(dim+1:dim*2) = this%rank(:)%upper
        !&>
    end function array1d_range_to_array

    !| returns an array1d_range_type object
    ! from an array containing bounds.
    function to_array1d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! lower and upper bounds of 1d-array.<br>
            !! `bounds` must have 2 elements and the bounds must be
            !! arranged as `[lower bound, upper bound]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(1d-array), ubound(1d-array)]`

        type(array1d_range_type) :: new_range
            !! an array1d_range_type object as the return value.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_array1d_range
end module type_array1d_range

!>This module provides user-defined types and constructors related to 2d array bounds.
!>
module arrayRange_type_2d
    use, intrinsic :: iso_fortran_env
    use :: arrayRange_type
    implicit none
    private
    public :: to_array2d_range
    public :: to_scalar2d_range
    public :: to_vector2d_range
    public :: to_tensor2d_range

    integer(int32), private, parameter :: dim = 2
        !! a parameter defining dimensionality

    !>a type for storing the lower and upper bound of a 2d array
    type, public :: array2d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bounds of rank-2 array
    contains
        procedure, public, pass :: array2d_range_to_array
        !* returns the lower and upper bounds as a 1d array.
        generic :: to_array => array2d_range_to_array
        !* generic interface to call like `bounds(:) = range%to_array()`
    end type array2d_range_type

    !>a type for storing the lower and upper bound of a 2d scalar variable
    type, public :: scalar2d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bounds of rank-2 array
    end type scalar2d_range_type

    !>a type for storing the lower and upper bound of a 2d vector variable
    type, public :: vector2d_range_type
        type(array2d_range_type), public :: x
            !! the array range of the x-component of a 2d vector variable
        type(array2d_range_type), public :: y
            !! the array range of the y-component of a 2d vector variable
    end type vector2d_range_type

    !>a type for storing the lower and upper bound of a 2d tensor variable
    type, public :: tensor2d_range_type
        type(array2d_range_type), public :: xx
            !! the array range of the xx-component of a 2d tensor variable
        type(array2d_range_type), public :: xy
            !! the array range of the xy-component of a 2d tensor variable
        type(array2d_range_type), public :: yx
            !! the array range of the yx-component of a 2d tensor variable
        type(array2d_range_type), public :: yy
            !! the array range of the yy-component of a 2d tensor variable
    end type tensor2d_range_type

    !>generic interface for defining `to_vector2d_range`
    interface to_vector2d_range
        procedure :: to_vector2d_range_bounds
        procedure :: to_vector2d_range_bounds_component
    end interface

    !>generic interface for defining `to_tensor2d_range`
    interface to_tensor2d_range
        procedure :: to_tensor2d_range_bounds
        procedure :: to_tensor2d_range_bounds_component
    end interface
contains
    !>returns the lower and upper bound as a 1d array.
    !>The array has 4 elements;
    !>the bounds are arranged as
    !>`[lower bound of rank1, lower bound of rank2,
    !>  upper bound of rank1, upper bound of rank2]`.
    !>
    !>### Example
    !>```Fortran
    !>   real(real64) :: array(-10:10, -20:20)
    !>   type(array2d_range_type) :: range
    !>   integer :: bounds(4)
    !>
    !>   ! get bounds of `array` and
    !>   ! create an array1d_range_type object
    !>   range = to_array2d_range([lbound(array),ubound(array)])
    !>
    !>   bounds = range%to_array()
    !>   print *, bounds ! -10 -20 10 20
    !>```
    function array2d_range_to_array(this) result(new_range_array)
        implicit none
        class(array2d_range_type), intent(in) :: this
            !! passed dummy argument

        integer(int32) :: new_range_array(dim*2)
            !! a 1d array containing the lower and upper bounds
            !! which are arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`.

        !&<
        new_range_array(    1:dim  ) = this%rank(:)%lower
        new_range_array(dim+1:dim*2) = this%rank(:)%upper
        !&>
    end function array2d_range_to_array

    !>returns an `array2d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_array2d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of 1d array.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`

        type(array2d_range_type) :: new_range
            !! an `array2d_range_type` instance.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_array2d_range

    !>returns a `scalar2d_range_type` instance
    !>from an array containing the lower and upper bound.
    function to_scalar2d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of a scalar variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`

        type(scalar2d_range_type) :: new_range
            !! a `scalar2d_range_type` instance.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_scalar2d_range

    !>returns a `vector2d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_vector2d_range_bounds(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of a vector variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`

        type(vector2d_range_type) :: new_range
            !! a `vector2d_range_type` instance.

        !&<
        new_range%x%rank(:)%lower = bounds(    1:dim  )
        new_range%x%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%y%rank(:)%lower = bounds(    1:dim  )
        new_range%y%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_vector2d_range_bounds

    !>returns a `vector2d_range_type` instance
    !>from arrays containing the lower and upper bound of each component.
    function to_vector2d_range_bounds_component(x_bounds, y_bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: x_bounds(dim*2)
            !! the lower and upper bounds of the x-component of a vector variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`
        integer(int32), intent(in) :: y_bounds(dim*2)
            !! the lower and upper bounds of the y-component of a vector variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`

        type(vector2d_range_type) :: new_range
            !! a `vector2d_range_type` instance.

        !&<
        new_range%x%rank(:)%lower = x_bounds(    1:dim  )
        new_range%x%rank(:)%upper = x_bounds(dim+1:dim*2)
        new_range%y%rank(:)%lower = y_bounds(    1:dim  )
        new_range%y%rank(:)%upper = y_bounds(dim+1:dim*2)
        !&>
    end function to_vector2d_range_bounds_component

    !>returns a `tensor2d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_tensor2d_range_bounds(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of a tensor variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`

        type(tensor2d_range_type) :: new_range
            !! a `tensor2d_range_type` instance.

        !&<
        new_range%xx%rank(:)%lower = bounds(    1:dim  )
        new_range%xx%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%xy%rank(:)%lower = bounds(    1:dim  )
        new_range%xy%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%yx%rank(:)%lower = bounds(    1:dim  )
        new_range%yx%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%yy%rank(:)%lower = bounds(    1:dim  )
        new_range%yy%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_tensor2d_range_bounds

    !>returns a `tensor2d_range_type` instance
    !>from arrays containing the lower and upper bound of each component.
    function to_tensor2d_range_bounds_component(xx_bounds, xy_bounds, yx_bounds, yy_bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: xx_bounds(dim*2)
            !! the lower and upper bounds of the xx-component of a tensor variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`
        integer(int32), intent(in) :: xy_bounds(dim*2)
            !! the lower and upper bounds of the xy-component of a tensor variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`
        integer(int32), intent(in) :: yx_bounds(dim*2)
            !! the lower and upper bounds of the yx-component of a tensor variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`
        integer(int32), intent(in) :: yy_bounds(dim*2)
            !! the lower and upper bounds of the yy-component of a tensor variable.<br>
            !! `bounds` must have 4 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2,
            !!   upper bound of rank1, upper bound of rank2]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(2d-array), ubound(2d-array)]`

        type(tensor2d_range_type) :: new_range
            !! a `tensor2d_range_type` instance.

        !&<
        new_range%xx%rank(:)%lower = xx_bounds(    1:dim  )
        new_range%xx%rank(:)%upper = xx_bounds(dim+1:dim*2)
        new_range%xy%rank(:)%lower = xy_bounds(    1:dim  )
        new_range%xy%rank(:)%upper = xy_bounds(dim+1:dim*2)
        new_range%yx%rank(:)%lower = yx_bounds(    1:dim  )
        new_range%yx%rank(:)%upper = yx_bounds(dim+1:dim*2)
        new_range%yy%rank(:)%lower = yy_bounds(    1:dim  )
        new_range%yy%rank(:)%upper = yy_bounds(dim+1:dim*2)
        !&>
    end function to_tensor2d_range_bounds_component
end module arrayRange_type_2d

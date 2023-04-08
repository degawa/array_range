!>This module provides user-defined types and constructors related to 3d array bounds.
!>
module arrayRange_type_3d
    use, intrinsic :: iso_fortran_env
    use :: arrayRange_type
    implicit none
    private
    public :: to_array3d_range
    public :: to_scalar3d_range
    public :: to_vector3d_range
    public :: to_tensor3d_range

    integer(int32), private, parameter :: dim = 3
        !! a parameter defining dimensionality

    !>a type for storing the lower and upper bound of a 3d array
    type, public :: array3d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bounds of rank-3 array
    contains
        procedure, public, pass :: array3d_range_to_array
        !* returns the lower and upper bounds as a 1d array.
        generic :: to_array => array3d_range_to_array
        !* generic interface to call like `bounds(:) = range%to_array()`
    end type array3d_range_type

    !>a type for storing the lower and upper bound of a 3d scalar variable
    type, public :: scalar3d_range_type
        type(array_range_type), public :: rank(dim)
            !! lower and upper bounds of rank-3 array
    end type scalar3d_range_type

    !>a type for storing the lower and upper bound of a 3d vector variable
    type, public :: vector3d_range_type
        type(array3d_range_type), public :: x
            !! the array range of the x-component of a 3d vector variable
        type(array3d_range_type), public :: y
            !! the array range of the y-component of a 3d vector variable
        type(array3d_range_type), public :: z
            !! the array range of the z-component of a 3d vector variable
    end type vector3d_range_type

    !>a type for storing the lower and upper bound of a 3d tensor variable
    type, public :: tensor3d_range_type
        type(array3d_range_type), public :: xx
            !! the array range of the xx-component of a 3d tensor variable
        type(array3d_range_type), public :: xy
            !! the array range of the xy-component of a 3d tensor variable
        type(array3d_range_type), public :: xz
            !! the array range of the xz-component of a 3d tensor variable
        type(array3d_range_type), public :: yx
            !! the array range of the yx-component of a 3d tensor variable
        type(array3d_range_type), public :: yy
            !! the array range of the yy-component of a 3d tensor variable
        type(array3d_range_type), public :: yz
            !! the array range of the yz-component of a 3d tensor variable
        type(array3d_range_type), public :: zx
            !! the array range of the zx-component of a 3d tensor variable
        type(array3d_range_type), public :: zy
            !! the array range of the zy-component of a 3d tensor variable
        type(array3d_range_type), public :: zz
            !! the array range of the zz-component of a 3d tensor variable
    end type tensor3d_range_type

    !>generic interface for defining `to_vector3d_range`
    interface to_vector3d_range
        procedure :: to_vector3d_range_bounds
        procedure :: to_vector3d_range_bounds_component
    end interface

    !>generic interface for defining `to_tensor3d_range`
    interface to_tensor3d_range
        procedure :: to_tensor3d_range_bounds
        procedure :: to_tensor3d_range_bounds_component
    end interface
contains
    !>returns lower and upper bounds as 1d-array.
    !>the array has 6 elements;
    !>and the bounds are arranged as
    !>`[lower bound of rank1, lower bound of rank2, lower bound of rank3,
    !>  upper bound of rank1, upper bound of rank2, upper bound of rank3]`.
    !>
    !>### Example
    !>```Fortran
    !>   real(real64) :: array(-10:10, -20:20, -30:30)
    !>   type(array3d_range_type) :: range
    !>   integer :: bounds(6)
    !>
    !>   ! get bounds of `array` and
    !>   ! create an array1d_range_type object
    !>   range = to_array3d_range([lbound(array),ubound(array)])
    !>
    !>   bounds = range%to_array()
    !>   print *, bounds ! -10 -20 -30 10 20 30
    !>```
    function array3d_range_to_array(range) result(new_range_array)
        implicit none
        class(array3d_range_type), intent(in) :: range
            !! passed dummy argument

        integer(int32) :: new_range_array(dim*2)
            !! a 1d array containing the lower and upper bounds
            !! which are arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`.

        !&<
        new_range_array(    1:dim  ) = range%rank(:)%lower
        new_range_array(dim+1:dim*2) = range%rank(:)%upper
        !&>
    end function array3d_range_to_array

    !>returns an `array3d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_array3d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of 1d array.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`

        type(array3d_range_type) :: new_range
            !! an `array3d_range_type` instance.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_array3d_range

    !>returns a `scalar3d_range_type` instance
    !>from an array containing the lower and upper bound.
    function to_scalar3d_range(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of 1d array.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`

        type(scalar3d_range_type) :: new_range
            !! a `scalar3d_range_type` instance.

        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_scalar3d_range

    !>returns a `vector3d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_vector3d_range_bounds(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of a vector variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`

        type(vector3d_range_type) :: new_range
            !! a `vector3d_range_type` instance.

        !&<
        new_range%x%rank(:)%lower = bounds(    1:dim  )
        new_range%x%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%y%rank(:)%lower = bounds(    1:dim  )
        new_range%y%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%z%rank(:)%lower = bounds(    1:dim  )
        new_range%z%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_vector3d_range_bounds

    !>returns a `vector2d_range_type` instance
    !>from arrays containing the lower and upper bound of each component.
    function to_vector3d_range_bounds_component(x_bounds, y_bounds, z_bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: x_bounds(dim*2)
            !! the lower and upper bounds of the x-component of a vector variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: y_bounds(dim*2)
            !! the lower and upper bounds of the y-component of a vector variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: z_bounds(dim*2)
            !! the lower and upper bounds of the z-component of a vector variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`

        type(vector3d_range_type) :: new_range
            !! a `vector3d_range_type` instance.

        !&<
        new_range%x%rank(:)%lower = x_bounds(    1:dim  )
        new_range%x%rank(:)%upper = x_bounds(dim+1:dim*2)
        new_range%y%rank(:)%lower = y_bounds(    1:dim  )
        new_range%y%rank(:)%upper = y_bounds(dim+1:dim*2)
        new_range%z%rank(:)%lower = z_bounds(    1:dim  )
        new_range%z%rank(:)%upper = z_bounds(dim+1:dim*2)
        !&>
    end function to_vector3d_range_bounds_component

    !>returns a `tensor3d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_tensor3d_range_bounds(bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: bounds(dim*2)
            !! the lower and upper bounds of a vector variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`

        type(tensor3d_range_type) :: new_range
            !! a `tensor3d_range_type` instance.

        !&<
        new_range%xx%rank(:)%lower = bounds(    1:dim  )
        new_range%xx%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%xy%rank(:)%lower = bounds(    1:dim  )
        new_range%xy%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%xz%rank(:)%lower = bounds(    1:dim  )
        new_range%xz%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%yx%rank(:)%lower = bounds(    1:dim  )
        new_range%yx%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%yy%rank(:)%lower = bounds(    1:dim  )
        new_range%yy%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%yz%rank(:)%lower = bounds(    1:dim  )
        new_range%yz%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%zx%rank(:)%lower = bounds(    1:dim  )
        new_range%zx%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%zy%rank(:)%lower = bounds(    1:dim  )
        new_range%zy%rank(:)%upper = bounds(dim+1:dim*2)
        new_range%zz%rank(:)%lower = bounds(    1:dim  )
        new_range%zz%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function to_tensor3d_range_bounds

    !>returns a `tensor3d_range_type` instance
    !>from an array containing the lower and upper bounds.
    function to_tensor3d_range_bounds_component(xx_bounds, xy_bounds, xz_bounds, &
                                                yx_bounds, yy_bounds, yz_bounds, &
                                                zx_bounds, zy_bounds, zz_bounds) result(new_range)
        implicit none
        integer(int32), intent(in) :: xx_bounds(dim*2)
            !! the lower and upper bounds of the xx-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: xy_bounds(dim*2)
            !! the lower and upper bounds of the xy-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: xz_bounds(dim*2)
            !! the lower and upper bounds of the xz-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: yx_bounds(dim*2)
            !! the lower and upper bounds of the yx-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: yy_bounds(dim*2)
            !! the lower and upper bounds of the yy-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: yz_bounds(dim*2)
            !! the lower and upper bounds of the yz-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: zx_bounds(dim*2)
            !! the lower and upper bounds of the zx-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: zy_bounds(dim*2)
            !! the lower and upper bounds of the zy-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`
        integer(int32), intent(in) :: zz_bounds(dim*2)
            !! the lower and upper bounds of the zz-component of a tensor variable.<br>
            !! `bounds` must have 6 elements, and the bounds must be
            !! arranged as
            !! `[lower bound of rank1, lower bound of rank2, lower bound of rank3,
            !!   upper bound of rank1, upper bound of rank2, upper bound of rank3]`,
            !! which assumes that `bounds` is created as
            !! `bounds = [lbound(3d-array), ubound(3d-array)]`

        type(tensor3d_range_type) :: new_range
            !! a `tensor3d_range_type` instance.

        !&<
        new_range%xx%rank(:)%lower = xx_bounds(    1:dim  )
        new_range%xx%rank(:)%upper = xx_bounds(dim+1:dim*2)
        new_range%xy%rank(:)%lower = xy_bounds(    1:dim  )
        new_range%xy%rank(:)%upper = xy_bounds(dim+1:dim*2)
        new_range%xz%rank(:)%lower = xz_bounds(    1:dim  )
        new_range%xz%rank(:)%upper = xz_bounds(dim+1:dim*2)
        new_range%yx%rank(:)%lower = yx_bounds(    1:dim  )
        new_range%yx%rank(:)%upper = yx_bounds(dim+1:dim*2)
        new_range%yy%rank(:)%lower = yy_bounds(    1:dim  )
        new_range%yy%rank(:)%upper = yy_bounds(dim+1:dim*2)
        new_range%yz%rank(:)%lower = yz_bounds(    1:dim  )
        new_range%yz%rank(:)%upper = yz_bounds(dim+1:dim*2)
        new_range%zx%rank(:)%lower = zx_bounds(    1:dim  )
        new_range%zx%rank(:)%upper = zx_bounds(dim+1:dim*2)
        new_range%zy%rank(:)%lower = zy_bounds(    1:dim  )
        new_range%zy%rank(:)%upper = zy_bounds(dim+1:dim*2)
        new_range%zz%rank(:)%lower = zz_bounds(    1:dim  )
        new_range%zz%rank(:)%upper = zz_bounds(dim+1:dim*2)
        !&>
    end function to_tensor3d_range_bounds_component
end module arrayRange_type_3d

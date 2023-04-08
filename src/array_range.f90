module array_range
    use, intrinsic :: iso_fortran_env
    use :: arrayRange_type_1d
    use :: arrayRange_type_2d
    use :: arrayRange_type_3d
    implicit none
    private
    public :: array1d_range_type
    public :: array2d_range_type
    public :: array3d_range_type
    public :: scalar1d_range_type
    public :: scalar2d_range_type
    public :: scalar3d_range_type
    public :: vector1d_range_type
    public :: vector2d_range_type
    public :: vector3d_range_type
    public :: tensor2d_range_type
    public :: tensor3d_range_type
    public :: to_array1d_range
    public :: to_array2d_range
    public :: to_array3d_range
    public :: to_scalar1d_range
    public :: to_scalar2d_range
    public :: to_scalar3d_range
    public :: to_vector1d_range
    public :: to_vector2d_range
    public :: to_vector3d_range
    public :: to_tensor2d_range
    public :: to_tensor3d_range
end module array_range

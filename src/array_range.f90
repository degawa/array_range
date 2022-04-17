module array_range
    use, intrinsic :: iso_fortran_env
    use :: type_array1d_range
    use :: type_array2d_range
    use :: type_array3d_range
    implicit none
    private
    public :: array1d_range_type
    public :: array2d_range_type
    public :: array3d_range_type
    public :: to_array1d_range
    public :: to_array2d_range
    public :: to_array3d_range

end module array_range

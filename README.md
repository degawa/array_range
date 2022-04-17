# array_range

This repository provides user-defined types `array_range{1|2|3}d_type` to improve the manipulation of bounds of Fortran arrays.

Fortran's array allows us to define in any range.
When executing a do-loop, the lower and upper bounds of arrays may be acquired as follows:

```Fortran
real(real64) :: val(0:10, 0:10)
integer :: lbnd(2), ubnd(2)

lbnd = lbound(val)
ubnd = ubound(val)

do j = lbnd(2), ubnd(2)
do i = lbnd(1), ubnd(1)
    ...
end do
end do
```

Introducing `array_range{1|2|3}d_type` makes it easier to acquire and reference the bounds:

```Fortran
use :: array_range

real(real64) :: val(0:10, 0:10)
type(array_range2d_type) :: range

range = to_array2d_range([lbound(val), ubound(val)])

do j = range%rank(2)%lower, range%rank(2)%upper
do i = range%rank(1)%lower, range%rank(1)%upper
    ...
end do
end do
```

When defining user-defined types containing multidimensional arrays, further improvement can be achieved by defining new types and procedures by referencing `array_range{1|2|3}d_type` such as:

```Fortran
type(vector2d) :: vec
! contains 2 multidimensional arrays

type(vector2d_range_type) :: range
! vector2d_range_type is defined as
! type :: vector2d_range_type
!     type(array_range2d_type) :: x,y
! end type

range = vec%get_range()
! get_range is a type-bound procedure to return vector2d_range_type object

do j = range%x%rank(2)%lower, range%x%rank(2)%upper
do i = range%x%rank(1)%lower, range%x%rank(1)%upper
    ...
end do
end do

do j = range%y%rank(2)%lower, range%y%rank(2)%upper
do i = range%y%rank(1)%lower, range%y%rank(1)%upper
    ...
end do
end do
```

## usage
### Get the code
To get the code, execute the following commnad:

```
git clone https://github.com/degawa/array_range.git
cd array_range
```

### Build with fpm
To build the library using fpm, execute the following command:

```
fpm build
```

### Reference from other projects
To refer `array_range` from your projects, simplify copy the `src` directory to your project directory and add `use` statement to the modules, functions, and subroutines that refers user-defined types defined in `array_range`.

```Fortran
use :: array_range
```

To refer `array_range` from fpm projects, add the following to your fpm manifest.

```TOML
[dependencies]
array_range = {git = "https://github.com/degawa/array_range.git"}
```

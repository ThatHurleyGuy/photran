This DOES cause a compile error but it would be nice for us to catch it beforehand.


BEFORE
----------

PROGRAM Area
!---------------------------------------------------------------------
!
!  This program computes the area of a circle given the input radius
!
!  Uses:  FUNCTION Area_Circle (r)
!
!---------------------------------------------------------------------
IMPLICIT NONE

! Declare local variables
REAL :: radius
REAL :: area_thing

!  Prompt user for radius of circle
write(*, '(A)', ADVANCE = "NO") "Enter the radius of the circle:  "
read(*,*) radius

area_thing = Area_Circle(radius)

! Write out area of circle using function call
write(*,100) "Area of circle with radius", radius, " is", &
            area_thing
100 format (A, 2x, F6.2, A, 2x, F11.2)

END PROGRAM Area

!-----Area_Circle----------------------------------------------------
!
!  Function to compute the area of a circle of given radius
!
!---------------------------------------------------------------------
FUNCTION Area_Circle(r)

IMPLICIT NONE
REAL :: Area_Circle
REAL, INTENT(IN) :: r

! Declare local constant Pi
REAL, PARAMETER :: Pi = 3.1415927

Area_Circle = Pi * r * r

END FUNCTION Area_Circle



AFTER
----------


PROGRAM Area
!---------------------------------------------------------------------
!
!  This program computes the area of a circle given the input radius
!
!  Uses:  FUNCTION Area_Circle (r)
!
!---------------------------------------------------------------------
IMPLICIT NONE

! Declare local variables
REAL :: radius
REAL :: area_thing

INTERFACE
   FUNCTION Area_Circle (r)
     REAL :: Area_Circle
     REAL, INTENT(IN) :: r
   END FUNCTION Area_Circle
END INTERFACE

!  Prompt user for radius of circle
write(*, '(A)', ADVANCE = "NO") "Enter the radius of the circle:  "
read(*,*) radius

area_thing = Area_Circle(radius)

! Write out area of circle using function call
write(*,100) "Area of circle with radius", radius, " is", &
            area_thing
100 format (A, 2x, F6.2, A, 2x, F11.2)

END PROGRAM Area

!-----Area_Circle----------------------------------------------------
!
!  Function to compute the area of a circle of given radius
!
!---------------------------------------------------------------------
FUNCTION Area_Circle(r)

IMPLICIT NONE
REAL :: Area_Circle
REAL, INTENT(IN) :: r

! Declare local constant Pi
REAL, PARAMETER :: Pi = 3.1415927

Area_Circle = Pi * r * r

END FUNCTION Area_Circle

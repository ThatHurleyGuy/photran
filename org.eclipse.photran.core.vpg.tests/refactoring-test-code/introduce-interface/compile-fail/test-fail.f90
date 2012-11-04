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

radius = 1

area_thing = Area_Circle(radius) !<<<<< 17,21, pass

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

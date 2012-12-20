!taken from http://www.stanford.edu/class/me200c/tutorial_90/09_modules.html
PROGRAM Area

IMPLICIT NONE

REAL :: radius, Area_Circle

radius = 1

CALL Compute_Area(radius, Area_Circle) !<<<<< 10, 13, pass

END PROGRAM Area


SUBROUTINE Compute_Area(r, Area)

IMPLICIT NONE
REAL, INTENT(IN) :: r
REAL, INTENT(OUT) :: Area

REAL, PARAMETER :: Pi = 3.1415927

Area = Pi * r * r

END SUBROUTINE Compute_Area


SUBROUTINE random()
IMPLICIT NONE
REAL :: r
REAL :: a
r = 1
CALL Compute_Area(radius, Area_Circle) 
END SUBROUTINE random

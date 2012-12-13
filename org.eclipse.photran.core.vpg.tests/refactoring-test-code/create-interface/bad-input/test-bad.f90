!taken from http://www.stanford.edu/class/me200c/tutorial_90/09_modules.html
PROGRAM Area !<<<<< 16,1,24,28,fail-final

IMPLICIT NONE

REAL :: radius, Area_Circle

radius = 1

CALL Compute_Area(radius, 2)

END PROGRAM Area

SUBROUTINE Compute_Area(r, Area)

IMPLICIT NONE
REAL, INTENT(IN) :: r
REAL, INTENT(OUT) :: Area

REAL, PARAMETER :: Pi = 3.1415927

Area = Pi * r * r

END SUBROUTINE Compute_Area
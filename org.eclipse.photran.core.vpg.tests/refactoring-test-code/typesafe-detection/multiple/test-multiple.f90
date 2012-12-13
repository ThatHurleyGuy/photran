PROGRAM multiple
IMPLICIT NONE
REAL :: in 
REAL :: out

CALL sub(in, out) 

CALL sub(in, out) 

END PROGRAM



SUBROUTINE sub(i, o)
IMPLICIT NONE 
REAL, INTENT(IN) :: i
REAL, INTENT(OUT) :: o
o = 5
END SUBROUTINE
!Taken from an email forwarded from Jeff Overbey
PROGRAM Example !<<<<< 13,1,19,17,pass
IMPLICIT NONE
INTEGER(KIND=4) :: n = 10
CALL S(n)
END PROGRAM Example

MODULE A; IMPLICIT NONE; INTEGER, PARAMETER :: INT_KIND = 4; END MODULE A
MODULE B; IMPLICIT NONE; USE A; END MODULE B
MODULE C; IMPLICIT NONE; CONTAINS
   SUBROUTINE T(); print *, ":-)"; END SUBROUTINE T
END MODULE C

SUBROUTINE S(n)
IMPLICIT NONE
USE B; USE C
INTEGER(KIND=INT_KIND), INTENT(IN) :: n
   print *, "N =", n
   CALL T
END SUBROUTINE S
PROGRAM testEmpty
IMPLICIT NONE

REAL :: x, y

x = 1
y = 2

CALL Do_Nothing() !<<<<< 9,9,pass

a = x + y

END PROGRAM testEmpty


SUBROUTINE Do_Nothing() 
IMPLICIT NONE
REAL :: z
z = 6
END SUBROUTINE Do_Nothing

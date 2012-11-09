This DOES cause a compile error but it would be nice for us to catch it beforehand.


Broken
--------


PROGRAM Compute_Factorial
!---------------------------------------------------------------------
!
!  This program computes n! using a recursive function
!
!  Uses:  FUNCTION Factorial(n)
!
!---------------------------------------------------------------------
IMPLICIT NONE

! Declare local variables
INTEGER :: n

!  Prompt user for radius of circle
write(*, '(A)', ADVANCE = "NO") "Enter n for computing n!:  "
read(*,*) n

! Notice here it needs the interface to know what the type is
! Write out value of factorial using function call
write(*,100) n, "factorial is ", Factorial(n)
100 format (I3, 2x, A, 2x, I12)

END PROGRAM Compute_Factorial

!-----Factorial------------------------------------------------------
!
!  Function to calculate factorials resursively
!
!---------------------------------------------------------------------
RECURSIVE FUNCTION Factorial(n)  RESULT(Fact)

IMPLICIT NONE
INTEGER :: Fact
INTEGER, INTENT(IN) :: n

IF (n == 0) THEN
   Fact = 1
ELSE
   Fact = n * Factorial(n-1)
END IF

END FUNCTION Factorial



WORKING
------------

PROGRAM Compute_Factorial
!---------------------------------------------------------------------
!
!  This program computes n! using a recursive function
!
!  Uses:  FUNCTION Factorial(n)
!
!---------------------------------------------------------------------
IMPLICIT NONE

INTERFACE
   FUNCTION Factorial(n)
     INTEGER :: Factorial
     INTEGER, INTENT(IN) :: n
   END FUNCTION Factorial
END INTERFACE

! Declare local variables
INTEGER :: n

!  Prompt user for radius of circle
write(*, '(A)', ADVANCE = "NO") "Enter n for computing n!:  "
read(*,*) n

! Write out value of factorial using function call
write(*,100) n, "factorial is ", Factorial(n)
100 format (I3, 2x, A, 2x, I12)

END PROGRAM Compute_Factorial

!-----Factorial------------------------------------------------------
!
!  Function to calculate factorials resursively
!
!---------------------------------------------------------------------
RECURSIVE FUNCTION Factorial(n)  RESULT(Fact)

IMPLICIT NONE
INTEGER :: Fact
INTEGER, INTENT(IN) :: n

IF (n == 0) THEN
   Fact = 1
ELSE
   Fact = n * Factorial(n-1)
END IF

END FUNCTION Factorial
! ============================================================================
! Name        : Numerical-Methods-in-Fortran.f90
! Author      : Murat Koptur
! Version     :
! Copyright   : 
! Description : Hello World in Fortran
! ============================================================================

program main
    use Assert
    use Constants
    implicit none

    real(kind=RP) :: result1  = C_E**C_PI - C_PI
    real(kind=RP) :: expected1 = 19.99909997
    real(kind=RP) :: tol       = 1e-6

    print *, "Numerical-Methods-in-Fortran"
    print *, "e to the pi minus pi: ", result1
    print *, "Assertion: ", assertEqualsWith(result1, expected1, tol)

end program

program main
    use Assert
    use Constants
    use Random
    use Testing
    implicit none

    type(Tests) :: tester

    real(kind=RP) :: tol       = 1e-6

    real(kind=RP) :: r1 = C_E**C_PI - C_PI
    real(kind=RP) :: e1 = 19.99909997

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol)

    call tester%print()
    call tester%end()

end program

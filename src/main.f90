program main
    use Assert
    use Constants
    use Testing
    implicit none

    type(Tests) :: tester

    real(kind=RP) :: tol       = 1e-6

    real(kind=RP) :: result1   = C_E**C_PI - C_PI
    real(kind=RP) :: expected1 = 19.99909997

    real(kind=RP) :: result2 = 2 + 2
    real(kind=RP) :: expected2 = 5

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", result1, expected1, tol)
    !call tester%assertEquals("This test must be fail", result2, expected2)

    call tester%print()
    call tester%end()

end program

program main
    use Assert
    use Constants
    use Distributions
    use Random
    use Testing
    implicit none

    type(Tests) :: tester

    real(kind = RP) :: tol = 1e-6

    real(kind = RP) :: r1 = C_E**C_PI - C_PI
    real(kind = RP) :: e1 = 19.99909997

    integer :: seed
    seed = time()

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol)

    call tester%assertEquals("Uniform Dist. CDF", punif(10.0, 0.0, 20.0), 0.5)
    call tester%assertEquals("Uniform Dist. Quartiles", qunif(0.75, 0.0, 20.0), 15.0)
    call tester%assertEquals("Uniform Dist. PDF", dunif(3.5, 0.0, 20.0), 0.05)

    call tester%print()
    call tester%end()

end program

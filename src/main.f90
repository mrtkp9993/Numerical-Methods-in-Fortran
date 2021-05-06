program main
    use Assert
    use Commons
    use Constants
    use Distributions
    use Random
    use Testing
    implicit none

    type(Tests) :: tester

    real(kind = RP) :: tol = 1e-3

    real(kind = RP) :: r1 = C_E**C_PI - C_PI
    real(kind = RP) :: e1 = 19.99909997

    integer :: seed
    seed = time()

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol)

    call tester%assertEquals("Uniform Dist. CDF", punif(10.0, 0.0, 20.0), 0.5)
    call tester%assertEquals("Uniform Dist. Quartiles", qunif(0.75, 0.0, 20.0), 15.0)
    call tester%assertEquals("Uniform Dist. PDF", dunif(3.5, 0.0, 20.0), 0.05)

    call tester%assertEqualsWith("Error function erf at -0.1", erfun(-0.1), -0.1124629, tol)
    call tester%assertEqualsWith("Error function erf at 0", erfun(0.0), 0.0, tol)
    call tester%assertEqualsWith("Error function erf at 2", erfun(2.0), 0.9953221, tol)

    call tester%assertEqualsWith("Inv.Error function erf at 0.1", erfun_inv(0.1), 0.088855, tol)
    call tester%assertEqualsWith("Inv.Error function erf at 0.9", erfun_inv(0.9), 1.163087, tol)

    call tester%assertEqualsWith("Normal Dist. CDF", pnorm(1.0, 0.0, 1.0), 0.8413, tol)
    call tester%assertEqualsWith("Normal Dist. CDF", pnorm(5.0, 7.0, 2.0), 0.1587, tol)
    call tester%assertEqualsWith("Normal Dist. Quartiles", qnorm(0.45, 0.0, 1.0), -0.125, tol)
    call tester%assertEqualsWith("Normal Dist. Quartiles", qnorm(0.95, 0.0, 1.0), 1.645, tol)
    call tester%assertEqualsWith("Normal Dist. Quartiles", qnorm(0.95, 3.0, 0.5), 3.822, tol)
    call tester%assertEqualsWith("Normal Dist. PDF", dnorm(3.5, 1.0, 20.0), 0.0198, tol)

    call tester%print()
    call tester%end()

end program

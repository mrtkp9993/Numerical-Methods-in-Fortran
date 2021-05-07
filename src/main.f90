program main
    use Assert
    use Commons
    use Constants
    use Distributions
    use Random
    use Testing
    implicit none

    type(Tests) :: tester

    real(DP) :: tol = 1e-18_dp

    real(DP) :: r1 = C_E**C_PI - C_PI
    real(DP) :: e1 = 19.999099979189475767_dp

    integer :: seed
    seed = time()

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol)

    call tester%assertEquals("Uniform Dist. CDF", punif(10.0, 0.0, 20.0), 0.5)
    call tester%assertEquals("Uniform Dist. Quartiles", qunif(0.75, 0.0, 20.0), 15.0)
    call tester%assertEquals("Uniform Dist. PDF", dunif(3.5, 0.0, 20.0), 0.05)

    ! approx. to error func. has maximum error 3E-7
    call tester%assertEqualsWith("Error function erf at -0.1", erfun(-0.1), -0.1124629, 3E-7)
    call tester%assertEqualsWith("Error function erf at 0", erfun(0.0), 0.0, 3E-7)
    call tester%assertEqualsWith("Error function erf at 2", erfun(2.0), 0.995322265, 3E-7)

    call tester%assertEqualsWith("Inv.Error function erf at 0.1", erfun_inv(0.1), 0.088855990, tol)
    call tester%assertEqualsWith("Inv.Error function erf at 0.9", erfun_inv(0.9), 1.163087, tol)

    call tester%assertEqualsWith("Normal Dist. CDF 1", pnorm(1.0, 0.0, 1.0), 0.841344746068542949, tol)
    call tester%assertEqualsWith("Normal Dist. CDF 2", pnorm(5.0, 7.0, 2.0), 0.1587, tol)
    call tester%assertEqualsWith("Normal Dist. Quartiles 1", qnorm(0.45, 0.0, 1.0), -0.125, tol)
    call tester%assertEqualsWith("Normal Dist. Quartiles 2", qnorm(0.95, 0.0, 1.0), 1.644853626951472714864, tol)
    call tester%assertEqualsWith("Normal Dist. Quartiles 3", qnorm(0.95, 3.0, 0.5), 3.822, tol)
    call tester%assertEqualsWith("Normal Dist. PDF", dnorm(4.1, 2.7, 0.40), 0.002181707, tol)

    call tester%print()
    call tester%end()

end program

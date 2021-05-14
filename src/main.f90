program main
    use Assert
    use Commons
    use Constants
    use Distributions
    use IO
    use LinearAlgebra
    use Random
    use Testing
    implicit none

    type(Tests) :: tester

    real(DP) :: tol = 1e-18_dp

    real(DP) :: r1 = C_E**C_PI - C_PI
    real(DP) :: e1 = 19.999099979189475767_dp

    real(DP), dimension(3, 3) :: matrix = reshape( (/ 60, 30, 20, &
                                                      30, 20, 15, &
                                                      20, 15, 12 /), &
                                                   shape(matrix), order=(/2, 1/))
    real(DP), allocatable, dimension(:, :) :: l,u

    call LUDecomposition(matrix, l, u)
    print *, "L: "
    call printMatrix(l) ! tests will be added later
    print *, ""
    print *, "U: "
    call printMatrix(u) ! tests will be added later

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol)

    ! approx. to error func. has maximum error 3E-7
    call tester%assertEqualsWith("Error function erf at -0.1", erfun(-0.1), -0.1124629, 3E-7)
    call tester%assertEqualsWith("Error function erf at 0", erfun(0.0), 0.0, 3E-7)
    call tester%assertEqualsWith("Error function erf at 2", erfun(2.0), 0.995322265, 3E-7)

    call tester%assertEqualsWith("Inv.Error function erf at 0.1", erfun_inv(0.1), 0.088855990, tol)
    call tester%assertEqualsWith("Inv.Error function erf at 0.9", erfun_inv(0.9), 1.163087, tol)

    call tester%print()
    call tester%end()

end program

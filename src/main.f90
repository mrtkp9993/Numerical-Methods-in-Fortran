program main
    use Assert
    use Commons
    use Constants
    use Distributions
    use IO
    use LinearAlgebra
    use Random
    use RootFinding
    use Testing
    implicit none

    type(Tests) :: tester

    real(DP) :: tol1 = 1e-8_dp
    real(DP) :: tol2 = 1e-5_dp

    real(DP) :: r1 = C_E**C_PI - C_PI
    real(DP) :: e1 = 19.999099979189475767_dp

    real(DP), dimension(3, 3) :: m1 = reshape( (/ 60, 30, 20, &
                                                      30, 20, 15, &
                                                      20, 15, 12 /), &
                                                   shape(m1), order=(/2, 1/))
    real(DP), dimension(2, 2) :: m2 = reshape( (/ 1.0_dp, 5.0_dp, &
                                                  2.0_dp, -1.7_dp/), &
                                                shape(m2), order=(/2,1/))
    real(DP), dimension(5, 5) :: m3 = reshape( (/ 7.80, 6.69, 1.85, 8.07, 2.71, &
                                                  4.82, 6.48, 6.64, 1.89, 0.18, &
                                                  1.08, 7.93, 3.98, 0.56, 2.13, &
                                                  5.77, 4.46, 4.78, 4.28, 2.59, &
                                                  1.03, 0.16, 6.59, 7.72, 7.11 /), &
                                                shape(m3), order=(/2,1/))
    real(DP), dimension(5, 5) :: m4 = reshape ( (/17, 24, 1, 8, 15, &
                                                  23, 5, 7, 14, 16, &
                                                  4, 6, 13, 20, 22, &
                                                  10, 12, 19, 21, 3, &
                                                  11, 18, 25, 2, 9/), &
                                                shape(m4), order=(/2, 1/))
    real(DP), dimension(3, 3) :: a = reshape( (/ 0.0_dp, 1.0_dp, 2.0_dp, &
                                                 2.0_dp, 1.0_dp, 4.0_dp, &
                                                 2.0_dp, 4.0_dp, 6.0_dp /), &
                                               shape(a), order=(/2, 1/))
    real(DP), dimension(3) :: b = (/4.0_dp, 3.0_dp, 7.0_dp/)
    real(DP), allocatable, dimension(:, :) :: l1,u1,l2,rr1,cf1,inv1,gq1,gr1
    real(DP), allocatable, dimension(:) :: x, eigv1

    real(DP) :: p00, p01, res1, p10, p11, res2, det1, det2, det3
    integer :: nmax = 100

    p00 = 1.5_dp
    p01 = 2.0_dp
    p10 = -0.5_dp
    p11 = 2.0_dp

    ! make calculations
    call secantMethod(func1, p00, p01, nmax, tol2, res1)
    call secantMethod(func2, p10, p11, nmax, tol2, res2)

    call gauss(a, b, x)
    call determinant(m2, det1)
    call determinant(m1, det2)
    call determinant(m3, det3)
    call cofactor(m1, cf1) ! add test
    call inverse(m1, inv1) ! add test
    call gramschmidt(m1, gq1, gr1) ! add test
    ! eigenvalues 65, +-21.28, +-13.13
    call qriter(m4, 50, eigv1) ! basic QR iteration - Bad results

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol1)

    call tester%assertEqualsWith("Root finding 1D 1", res1, 1.61803, tol2)
    call tester%assertTrue("Root finding 1D 2", isnan(res2))

    call tester%assertEqualsWith("Gauss elim. x1", x(1), -2.5, tol2)
    call tester%assertEqualsWith("Gauss elim. x2", x(2),  0.0, tol2)
    call tester%assertEqualsWith("Gauss elim. x3", x(3),  2.0, tol2)
    call tester%assertEqualsWith("Matrix det. 2x2", det1, -11.69999, tol2)
    call tester%assertEqualsWith("Matrix det. 3x3", det2, 100.0, tol2)
    call tester%assertEqualsWith("Matrix det. 5x5", det3, 4655.40795, tol2)

    call tester%print()
    call tester%end()

contains

    real(DP) function func1(x) result(y)
        real(DP), intent(in) :: x
        y = x * x - x - 1
    end function func1

    real(DP) function func2(x) result(y)
        real(DP), intent(in) :: x
        y = sqrt(x + 2) / x
    end function func2

end program

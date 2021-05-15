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

    real(DP), dimension(3, 3) :: matrix = reshape( (/ 60, 30, 20, &
                                                      30, 20, 15, &
                                                      20, 15, 12 /), &
                                                   shape(matrix), order=(/2, 1/))
    real(DP), allocatable, dimension(:, :) :: l,u

    real(DP) :: p00, p01, res1, p10, p11, res2
    integer :: nmax = 100

    p00 = 1.5_dp
    p01 = 2.0_dp
    p10 = -0.5_dp
    p11 = 2.0_dp

    ! make calculations
    call secantMethod(func1, p00, p01, nmax, tol2, res1)
    call secantMethod(func2, p10, p11, nmax, tol2, res2)

    call LUDecomposition(matrix, l, u)

    call tester%init()

    call tester%assertEqualsWith("e to the pi minus pi", r1, e1, tol1)

    call tester%assertEqualsWith("Root finding 1D", res1, 1.61803, tol2)

    call tester

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

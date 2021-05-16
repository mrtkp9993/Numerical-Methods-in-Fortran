module LinearAlgebra
    use Constants
    implicit none

contains

    subroutine ludcmp(matrix, l, u)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: l, u
        integer :: m, n, k, i, j, s

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (m .ne. n) then
            print *, "Input matrix must be a square matrix."
            stop
        end if

        allocate(l(n, n))
        allocate(u(n, n))

        do k = 1, n
            l(k, k) = 1.0_dp
            do j = k, n
                u(k, j) = matrix(k, j) - sum((/(l(k, s) * u(s, j), s = 1, k - 1)/))
            end do
            do i = k + 1, n
                l(i, k) = (matrix(i, k) - sum((/(l(i, s) * u(s, k), s = 1, k - 1)/))) / u(k, k)
            end do
        end do
    end subroutine ludcmp

    subroutine cholesky(matrix, l)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: l
        integer :: m, n, k, i, j, s

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (m .ne. n) then
            print *, "Input matrix must be a square matrix."
            stop
        end if

        allocate(l(n, n))

        do k = 1, n
            l(k, k) = sqrt(matrix(k, k) - sum((/(l(k, s) * l(k, s), s = 1, k - 1)/)))
            do i = k + 1, n
                l(i, k) = (matrix(i, k) - sum((/(l(i, s) * l(k, s), s = 1, k - 1)/))) / l(k, k)
            end do
        end do

    end subroutine cholesky

end module LinearAlgebra
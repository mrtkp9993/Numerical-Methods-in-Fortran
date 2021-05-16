module LinearAlgebra
    use Constants
    implicit none

contains

    subroutine cholesky(matrix, l)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: l
        integer :: m, n, k, i, s

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

    subroutine gauss(a, b, x)
        real(DP), dimension(:, :), intent(in) :: a
        real(DP), dimension(:), intent(in) :: b
        real(DP), allocatable, dimension(:), intent(inout) :: x
        ! copy a and b so they will not change in program
        real(DP), allocatable, dimension(:, :) :: a_copy
        real(DP), allocatable, dimension(:) :: b_copy
        real(DP), allocatable, dimension(:) :: s
        real(DP) :: c, p, st
        integer :: m1, n1, m2, i, j, k, l, n

        m1 = size(a, 1)
        n1 = size(a, 2)
        m2 = size(b, 1)

        if (m1 .ne. m2) then
            print *, "Dimensions must match!"
            stop
        end if

        n = m1
        allocate(s(n))
        allocate(a_copy(n,n))
        allocate(b_copy(n))
        allocate(x(n))
        a_copy = a
        b_copy = b

        do k = 1, n-1
            do i = k, n
                s(i) = 0.0_dp
                do j = k, n
                    s(i) = max(s(i), abs(a_copy(i, j)))
                end do
            end do

            p = abs(a_copy(k,k) / s(k))
            l = k
            do j = k+1, n
                if (abs(a_copy(j,k) / s(j)) > p) then
                    p = abs(a_copy(j,k) / s(j))
                    l = j
                end if
            end do

            if (p .eq. 0.0_dp) then
                error stop "The matrix is singular."
            end if

            if (l .ne. k) then
                do j = k, n
                    st = a_copy(k, j)
                    a_copy(k, j) = a_copy(l, j)
                    a_copy(l, j) = st
                end do
                st = b_copy(k)
                b_copy(k) = b_copy(l)
                b_copy(l) = st
            end if

            do i = k+1, n
                c = a_copy(i, k) / a_copy(k, k)
                a_copy(i, k) = 0.0_dp
                b_copy(i) = b_copy(i) - c * b_copy(k)
                do j = k+1, m1
                    a_copy(i, j) = a_copy(i, j) - c * a_copy(k, j)
                end do
            end do
        end do

        x(n) = b_copy(n) / a_copy(n, n)
        do i = n-1, 1, -1
            c = 0.0_dp
            do j = i+1, n
                c = c + a_copy(i, j) * x(j)
            end do
            x(i) = (b_copy(i) - c) / a_copy(i, i)
        end do
    end subroutine gauss

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

    subroutine minor_matrix(matrix, i, j, res)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: res
        integer, intent(in) :: i, j
        integer :: ii, jj, m, n

        m = size(matrix, 1)
        n = size(matrix, 2)
        allocate(res(m-1, n-1))

        do ii = 1, m
            do jj = 1, n
                if (ii .lt. i .and. jj .lt. j) then
                    res(ii, jj) = matrix(ii, jj)
                else if (ii .lt. i .and. jj .gt. j) then
                    res(ii, jj-1) = matrix(ii, jj)
                else if (ii .gt. i .and. jj .lt. j) then
                    res(ii-1, jj) = matrix(ii, jj)
                else if (ii .gt. i .and. jj .gt. j) then
                    res(ii-1, jj-1) = matrix(ii, jj)
                end if
            end do
        end do
    end subroutine

end module LinearAlgebra
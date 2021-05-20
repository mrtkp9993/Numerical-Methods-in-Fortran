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
            error stop "Input matrix must be a square matrix."
        end if

        if (.not. allocated(l)) allocate(l(n, n))

        do k = 1, n
            l(k, k) = sqrt(matrix(k, k) - sum((/(l(k, s) * l(k, s), s = 1, k - 1)/)))
            do i = k + 1, n
                l(i, k) = (matrix(i, k) - sum((/(l(i, s) * l(k, s), s = 1, k - 1)/))) / l(k, k)
            end do
        end do

    end subroutine cholesky

    subroutine cofactor(matrix, cof)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: cof
        real(DP), allocatable, dimension(:, :) :: minor
        real(DP) :: det
        integer :: i, j, m, n

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (m .ne. n) then
            error stop "Input matrix must be a square matrix."
        end if

        if (.not. allocated(cof)) allocate(cof(m, n))

        do i = 1, m
            do j = 1, n
                call minorMatrix(matrix, i, j, minor)
                call determinant(minor, det)
                cof(i, j) = (-1) ** (i + j) * det
            end do
        end do

    end subroutine cofactor

    subroutine determinant2(matrix, det)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), intent(inout) :: det
        integer :: m, n

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (m .ne. 2 .or. n .ne. 2) then
            error stop "Dimensions must be equal to 2!"
        end if

        det = matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)

    end subroutine determinant2

    subroutine determinant3(matrix, det)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), intent(inout) :: det
        real(DP), allocatable, dimension(:, :) :: min1, min2, min3
        real(DP) :: det1, det2, det3
        integer :: m, n

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (m .ne. 3 .or. n .ne. 3) then
            error stop "Dimensions must be equal to 3!"
        end if

        call minorMatrix(matrix, 1, 1, min1)
        call minorMatrix(matrix, 1, 2, min2)
        call minorMatrix(matrix, 1, 3, min3)
        call determinant2(min1, det1)
        call determinant2(min2, det2)
        call determinant2(min3, det3)

        det = matrix(1, 1) * det1 - matrix(1, 2) * det2 + matrix(1, 3) * det3

    end subroutine determinant3

    subroutine determinant(matrix, det)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), intent(inout) :: det
        real(DP), allocatable, dimension(:, :) :: lm, um
        real(DP) :: lmdet, umdet
        integer :: m, n, i

        m = size(matrix, 1)

        if (m .eq. 2) then
            call determinant2(matrix, det)
            return
        end if

        if (m .eq. 3) then
            call determinant3(matrix, det)
            return
        end if

        n = size(matrix, 2)
        if (m .ne. n) then
            error stop "Dimensions must match!"
        end if

        call ludcmp(matrix, lm, um)

        lmdet = 1.0_dp
        umdet = 1.0_dp
        do i = 1, n
            lmdet = lmdet * lm(i, i)
            umdet = umdet * um(i, i)
        end do

        det = lmdet * umdet;

    end subroutine determinant

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
            error stop "Dimensions must match!"
        end if

        n = m1
        if (.not. allocated(s)) allocate(s(n))
        if (.not. allocated(a_copy)) allocate(a_copy(n,n))
        if (.not. allocated(b_copy)) allocate(b_copy(n))
        if (.not. allocated(x)) allocate(x(n))
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

    subroutine gramschmidt(matrix, q, r)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: q, r
        ! temporary arrays for calculations
        real(DP), allocatable, dimension(:, :) :: t1, t2, t3, t4
        integer :: m, n, i, j, k

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (.not. (n .le. m)) then
            error stop "Column count must be equal or smaller than row count."
        end if

        if (.not. allocated(q)) allocate(q(m, n))
        if (.not. allocated(r)) allocate(r(m, n))

        do i = 1, m
            do j = 1, n
                r(i, j) = 0.0_dp
            end do
        end do

        q = matrix
        do k = 1, n
            r(k, k) = norm2(q(:, k))
            q(:, k) = q(:, k) / r(k, k)

            if (.not. allocated(t1)) allocate(t1(m, 1))
            t1 = reshape(q(:, k), (/1, m/))
            t2 = reshape(q(:, k+1:n), (/m, n-k/))

            r(k, k+1:n) = reshape(matmul(t1,t2), (/n-k/))

            if (.not. allocated(t3)) allocate(t3(1, n-k))
            if (.not. allocated(t3)) allocate(t3(m, 1))
            t3 = reshape(r(k, k+1:n), (/1, n-k/))
            t4 = reshape(q(:, k), (/m, 1/))

            q(:, k+1:n) = q(:, k+1:n) - matmul(t4, t3)
        end do

    end subroutine gramschmidt

    subroutine inverse(matrix, inv)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: inv
        real(DP), allocatable, dimension(:, :) :: cof, trs
        real(DP) :: det
        integer :: m, n

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (.not. allocated(inv)) allocate(inv(m, n))
        allocate(cof(m,n))
        allocate(trs(m,n))

        call determinant(matrix, det)
        call cofactor(matrix, cof)
        call transpose(cof, trs)

        inv = 1.0_dp / det * trs

    end subroutine inverse

    subroutine ludcmp(matrix, l, u)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: l, u
        integer :: m, n, k, i, j, s

        m = size(matrix, 1)
        n = size(matrix, 2)

        if (m .ne. n) then
            error stop "Input matrix must be a square matrix."
        end if

        if (.not. allocated(l)) allocate(l(n, n))
        if (.not. allocated(u)) allocate(u(n, n))

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

    subroutine minorMatrix(matrix, i, j, res)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: res
        integer, intent(in) :: i, j
        integer :: ii, jj, m, n

        m = size(matrix, 1)
        n = size(matrix, 2)
        if (.not. allocated(res)) allocate(res(m-1, n-1))

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

    subroutine transpose(matrix, trs)
        real(DP), dimension(:, :), intent(in) :: matrix
        real(DP), allocatable, dimension(:, :), intent(inout) :: trs
        integer :: i, j, m, n

        m = size(matrix, 1)
        n = size(matrix, 2)
        if (.not. allocated(trs)) allocate(trs(m, n))

        do i = 1, m
            do j = 1, n
                trs(i, j) = matrix(j, i)
            end do
        end do

    end subroutine transpose

end module LinearAlgebra
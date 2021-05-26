module Interpolation
    use Constants
    use IO
    use LinearAlgebra
    use Polynomials
    implicit none

    contains

        ! interpolate data with length (n+1) with n-degree polynomial
        subroutine interp(xData, yData, xEval, yEval)
            real(DP), dimension(:), intent(in) :: xData, yData, xEval
            real(DP), allocatable, dimension(:), intent(inout) :: yEval
            real(DP), allocatable, dimension(:, :) :: vandermonde, invvandermonde
            real(DP), allocatable, dimension(:) :: coeff
            integer :: i, j, n, m

            if (size(xData) .ne. size(yData)) then
                error stop "Input arrays must have equal length."
            end if

            n = size(xData)

            if (.not. allocated(vandermonde)) allocate(vandermonde(n, n))
            if (.not. allocated(coeff)) allocate(coeff(n))

            do i = 1, n
                do j = 1, n
                    vandermonde(i, j) = xData(i) ** (j-1)
                end do
            end do

            call inverse(vandermonde, invvandermonde)
            coeff = matmul(invvandermonde, yData)

            m = size(xEval)
            if (.not. allocated(yEval)) allocate(yEval(m))
            do i = 1, m
                yEval(i) = horner(coeff, xEval(i))
            end do

        end subroutine interp

        real(DP) function lagrange(xData, yData, x) result(y)
            real(DP), dimension(:), intent(in) :: xData, yData
            real(DP), intent(in) :: x
            integer :: i, j, n
            real(DP) :: prod

            if (size(xData) .ne. size(yData)) then
                error stop "Input arrays must have equal length."
            end if

            n = size(xData)

            y = 0.0_dp
            do i = 1, n
                prod = 1.0_dp
                do j = 1, n
                    if (i .ne. j) then
                        prod = prod * (x - xData(j)) / (xData(i) - xData(j))
                    end if
                end do
                y = y + (yData(i) * prod)
            end do

        end function lagrange

        subroutine lagrangep(xData, yData, xEval, yEval)
            real(DP), dimension(:), intent(in) :: xData, yData, xEval
            real(DP), allocatable, dimension(:), intent(inout) :: yEval
            integer :: i, n, m

            if (size(xData) .ne. size(yData)) then
                error stop "Input arrays must have equal length."
            end if

            n = size(xData)
            m = size(xEval)
            if (.not. allocated(yEval)) allocate(yEval(m))

            do i = 1, m
                yEval(i) = lagrange(xData, yData, xEval(i))
            end do

        end subroutine lagrangep

        subroutine naturalcubicspline(xData, yData, coeffs)
            real(DP), dimension(:), intent(in) :: xData, yData
            real(DP), allocatable, dimension(:, :), intent(inout) :: coeffs
            real(DP), allocatable, dimension(:) :: h, alpha, l, mu, z
            integer :: i, j, n

            if (size(xData) .ne. size(yData)) then
                error stop "Input arrays must have equal length."
            end if

            n = size(xData)
            if (.not. allocated(coeffs)) allocate(coeffs(n-1, 4))
            if (.not. allocated(h)) allocate(h(n-1))
            if (.not. allocated(alpha)) allocate(alpha(n-2))
            if (.not. allocated(l)) allocate(l(n))
            if (.not. allocated(mu)) allocate(mu(n))
            if (.not. allocated(z)) allocate(z(n))

            do i = 1, n-1
                coeffs(i, 1) = yData(i)
            end do

            do i = 1, n-1
                h(i) = xData(i+1)-xData(i)
            end do

            do i = 1, n
                l(i) = 0.0_dp
                mu(n) = 0.0_dp
                z(n) = 0.0_dp
            end do

            do i = 1, n-2
                alpha(i) = 0.0_dp
            end do

            do i = 2, n-1
                alpha(i) = (3.0_dp / h(i) * (yData(i+1) - yData(i)))  &
                        - (3.0_dp / h(i-1) * (yData(i) - yData(i-1)))
            end do

            l(1) = 1.0_dp
            mu(1) = 0.0_dp
            z(1) = 0.0_dp
            do i = 2, n-1
                l(i) = 2*(xData(i+1)-xData(i-1)) - (h(i-1) * mu(i-1))
                mu(i) = h(i) / l(i)
                z(i) = (alpha(i) - h(i-1) * z(i-1)) / l(i)
            end do
            l(n) = 1.0_dp
            mu(n) = 0.0_dp
            z(n) = 0.0_dp

            do j = n-1, 1, -1
                coeffs(j, 3) = z(j) - mu(j) * coeffs(j+1, 3)
                coeffs(j, 2) = ((yData(j+1)-yData(j))/h(j)) &
                        -(h(j)*(coeffs(j+1, 3)+2*coeffs(j, 3))/3)
                coeffs(j, 4) = (coeffs(j+1, 3)-coeffs(j, 3))/(3*h(j))
            end do

        end subroutine naturalcubicspline

        subroutine naturalcubicsplinep()
            error stop "Not implemented yet"
        end subroutine naturalcubicsplinep

end module Interpolation
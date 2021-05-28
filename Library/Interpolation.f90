module Interpolation
    use Constants
    !use LinearAlgebra
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
                stop "Input arrays must have equal length."
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
                stop "Input arrays must have equal length."
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
                stop "Input arrays must have equal length."
            end if

            n = size(xData)
            m = size(xEval)
            if (.not. allocated(yEval)) allocate(yEval(m))

            do i = 1, m
                yEval(i) = lagrange(xData, yData, xEval(i))
            end do

        end subroutine lagrangep

        subroutine naturalcubicspline()
            stop "Not implemented yet"
        end subroutine naturalcubicspline

        subroutine naturalcubicsplinep()
            stop "Not implemented yet"
        end subroutine naturalcubicsplinep

end module Interpolation
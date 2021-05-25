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
            integer :: i, j, k, n, m

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

end module Interpolation
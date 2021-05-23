module Polynomials
    use Constants
    implicit none

    contains

        ! horner scheme
        real(DP) function horner(p, x) result(y)
            real(DP), dimension(:), intent(in) :: p
            real(DP), intent(in) :: x
            integer :: n, i

            n = size(p)
            if (n .le. 0) then
                error stop "Coefficent array length must be greater than zero."
            end if

            y = p(n)
            do i = n-1, 1, -1
                y = y * x + p(i)
            end do

        end function horner

        ! naive method for
        ! calculate the value of
        ! p(x)=a_0+a_1x+a_2x^2+...+a_nx^n
        ! at x
        real(DP) function polyeval(p, x) result(y)
            real(DP), dimension(:), intent(in) :: p
            real(DP), intent(in) :: x
            integer :: n, i

            n = size(p)
            if (n .le. 0) then
                error stop "Coefficent array length must be greater than zero."
            end if

            y = 0.0_dp
            do i = 1, n
                y = y + (p(i) * (x ** (i-1)))
            end do

        end function polyeval

end module Polynomials
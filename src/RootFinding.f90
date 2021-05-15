module RootFinding
    use Constants
    implicit none

    interface
        real function fun(x) result(y)
            real, intent(in) :: x
        end function fun
    end interface

    contains

        ! Secant method for 1D root finding
        subroutine secantMethod(f, p0, p1, nmax, tol, res)
            procedure(fun) :: f
            real(DP), intent(in)  :: p0, p1, tol
            integer, intent(in)   :: nmax
            real(DP), intent(inout) :: res
            integer :: i
            real(DP) :: tp0, tp1, p2

            tp0 = p0
            tp1 = p1

            i = 1
            do while (i .le. nmax)
                p2 = tp0 - f(tp0) * ((tp1 - tp0) / (f(tp1) - f(tp0)))
                if (abs(p2 - tp1) .lt. tol) then
                    print *, "Root found after ", i, " iterations: ", p2
                    res = p2
                    return
                end if
                i = i + 1
                tp0 = tp1
                tp1 = p2
            end do
            print *, "Method failed after ", i, " iterations."
            res = 0
            res = 0/res
        end subroutine secantMethod

end module RootFinding

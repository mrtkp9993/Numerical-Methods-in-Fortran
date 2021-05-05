module Distributions
    use Constants
    use Random
    implicit none

contains

    ! p -> CDF
    ! q -> Inverse CDF
    ! d -> PDF
    ! r -> Random number from dist.

    real(kind = RP) function punif(x, a, b) result(prob)
        real(kind = RP), intent(in) :: x, a, b
        if (x .le. a) then
            prob = 0.0
        else if (b .le. x) then
            prob = 1.0
        else
            prob = (x - a) / (b - a)
        end if
    end function punif

    real(kind = RP) function qunif(prob, a, b) result(x)
        real(kind = RP), intent(in) :: prob, a, b

        if (prob .lt. 0.0 .or. 1.0 .lt. prob) then
            stop 1
        end if

        x = a + (b - a) * prob
    end function qunif

    real(kind = RP) function dunif(x, a, b) result(prob)
        real(kind = RP), intent(in) :: x, a, b
        if (x < a .or. b < x) then
            prob = 0.0
        else
            prob = 1.0 / (b - a)
        end if
    end function dunif

    real(kind = RP) function runif(a, b, seed) result(x)
        integer, intent(inout) :: seed
        real(kind = RP), intent(in) :: a, b
        x = qunif(lcg(seed), a, b)
    end function runif

end module Distributions
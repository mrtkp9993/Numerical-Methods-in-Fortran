module Distributions
    use Commons
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

    real(kind = RP) function pnorm(x, mu, sigma) result(prob)
        real(kind = RP), intent(in) :: x, mu, sigma
        real(kind = RP) :: y

        y = (x - mu) / (sigma * C_SQRT2)
        prob = 0.5 * (1 + erfun(y))
    end function pnorm

    real(kind = RP) function qnorm(prob, mu, sigma) result(x)
        real(kind = RP), intent(in) :: prob, mu, sigma
        if (prob < 0.5) then
            x = -ccdf(prob)
        else
            x = ccdf(1-prob)
        end if
        x = mu + sigma * x
    end function qnorm

    real(kind = RP) function dnorm(x, mu, sigma) result(prob)
        real(kind = RP), intent(in) :: x, mu, sigma
        prob = exp(-0.5 * ((x-mu)/sigma)**2) / sqrt (2*C_PI*sigma*sigma)
    end function dnorm

    real(kind = RP) function rnorm(mu, sigma, seed) result(x)
        integer, intent(inout) :: seed
        real(kind = RP), intent(in) :: mu, sigma
        real(kind = RP) :: u1, u2
        u1 = runif(0.0, 1.0, seed)
        u2 = runif(0.0, 1.0, seed)
        x = sqrt(-2.0*log(u1)) * cos(2.0*C_PI*u2)
        x = mu + sigma * x
    end function rnorm

end module Distributions
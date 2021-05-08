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

    ! uniform dist.
    real(DP) function punif(x, a, b) result(prob)
        real(DP), intent(in) :: x, a, b
        if (x .le. a) then
            prob = 0.0_dp
        else if (b .le. x) then
            prob = 1.0_dp
        else
            prob = (x - a) / (b - a)
        end if
    end function punif

    real(DP) function qunif(prob, a, b) result(x)
        real(DP), intent(in) :: prob, a, b

        if (prob .lt. 0.0 .or. 1.0 .lt. prob) then
            stop 1
        end if

        x = a + (b - a) * prob
    end function qunif

    real(DP) function dunif(x, a, b) result(prob)
        real(DP), intent(in) :: x, a, b
        if (x < a .or. b < x) then
            prob = 0.0_dp
        else
            prob = 1.0_dp / (b - a)
        end if
    end function dunif

    real(DP) function runif(a, b, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: a, b
        x = qunif(lcg(seed), a, b)
    end function runif

    ! normal dist.
    real(DP) function pnorm(x, mu, sigma) result(prob)
        real(DP), intent(in) :: x, mu, sigma
        real(DP) :: y

        y = (x - mu) / (sigma * C_SQRT2)
        prob = 0.5_dp * (1.0_dp + erfun(y))
    end function pnorm

    real(DP) function qnorm(prob, mu, sigma) result(x)
        real(DP), intent(in) :: prob, mu, sigma
        x = erfun_inv(2.0_dp * prob - 1.0_dp)
        x = mu + sigma * C_SQRT2 * x
    end function qnorm

    real(DP) function dnorm(x, mu, sigma) result(prob)
        real(DP), intent(in) :: x, mu, sigma
        real(DP) :: y
        y =(x-mu)/sigma
        prob = exp(-0.5_dp * (y**2)) / sqrt(2.0_dp*C_PI*sigma*sigma)
    end function dnorm

    ! Box-Muller algorithm for generating normal random numbers
    real(DP) function rnorm(mu, sigma, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: mu, sigma
        real(DP) :: u1, u2
        u1 = runif(0.0_dp, 1.0_dp, seed)
        u2 = runif(0.0_dp, 1.0_dp, seed)
        x = sqrt(-2.0_dp*log(u1)) * cos(2.0_dp*C_PI*u2)
        x = mu + sigma * x
    end function rnorm

    ! gamma dist.
    ! Marsaglia-Tsang method
    real(DP) function marsaglia_tsang(a, seed) result (y)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: a
        real(DP) :: d, c, x, v, u

        d = a - 1.0_dp/3.0_dp
        c = 1.0_dp / sqrt(9.0_dp*d)
        do
            v = 0.0_dp
            do while(v .le. 0.0_dp)
                x = rnorm(0.0, 1.0, seed)
                v = 1.0_dp + c * x
            end do
            v = v**3
            u = runif(0.0,1.0, seed)
            if (u .lt. 1.0_dp - 0.0331*(x**2)*(x**2)) then
                y = d*v
                exit
            end if
            if (log(u) .lt. 0.5*x*x+d*(1.0_dp-v+log(v))) then
                y = d*v
                exit
            end if
        end do
    end function marsaglia_tsang

    ! generate gamma random numbers
    real(DP) function rgamma(alpha, beta, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: alpha, beta
        real(DP) :: ms
        if (alpha .gt. 1.0) then
            ms = marsaglia_tsang(alpha, seed)
            x = ms / beta
        else
            ms = marsaglia_tsang(alpha+1.0_dp, seed)
            x = ms*(runif(0.0,1.0,seed)**(1.0_dp/alpha))/beta
        end if
    end function rgamma

end module Distributions

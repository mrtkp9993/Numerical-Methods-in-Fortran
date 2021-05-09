module Distributions
    use Commons
    use Constants
    use Random
    implicit none

contains

    ! uniform dist.
    real(DP) function runif(a, b, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: a, b
        x = a + (b - a) * lcg(seed)
    end function runif

    ! normal dist.
    ! Box-Muller algorithm for generating normal random numbers
    real(DP) function rnorm(mu, sigma, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: mu, sigma
        real(DP) :: u1, u2
        if (sigma .lt. 0) then
            error stop "Sigma^2 must be strictly positive."
        end if
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
        if (alpha .lt. 0 .or. beta .lt. 0) then
            error stop "Parameters must be strictly positive."
        end if
        if (alpha .gt. 1.0) then
            ms = marsaglia_tsang(alpha, seed)
            x = ms / beta
        else
            ms = marsaglia_tsang(alpha+1.0_dp, seed)
            x = ms*(runif(0.0,1.0,seed)**(1.0_dp/alpha))/beta
        end if
    end function rgamma

    ! beta dist.
    real(DP) function rbeta(alpha, beta, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: alpha, beta
        real(DP) :: c, d
        if (alpha .lt. 0 .or. beta .lt. 0) then
            error stop "Parameters must be strictly positive."
        end if
        c = rgamma(alpha, 1.0, seed)
        d = rgamma(beta, 1.0, seed)
        x = c / (c + d)
    end function rbeta

    ! exponential dist.
    real(DP) function rexp(lambda, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: lambda
        if (lambda .lt. 0) then
            error stop "Lambda must be strictly positive."
        end if

        x = -log(runif(0.0_dp, 1.0_dp, seed)) / lambda
    end function rexp

end module Distributions

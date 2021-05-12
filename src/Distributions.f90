! References (algorithms / pseudocodes) for this module
! 1. Devroye's book: Chapter 9 & 10
! http://luc.devroye.org/chapter_nine.pdf, http://luc.devroye.org/chapter_ten.pdf
! 2. Kneusel's Random Numbers and Computers book.

module Distributions
    use Commons
    use Constants
    use Random
    implicit none

contains

    ! beta dist.
    real(DP) function rbeta(alpha, beta, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: alpha, beta
        real(DP) :: c, d
        if (alpha .le. 0 .or. beta .le. 0) then
            error stop "Parameters must be strictly positive."
        end if
        c = rgamma(alpha, 1.0, seed)
        d = rgamma(beta, 1.0, seed)
        x = c / (c + d)
    end function rbeta

    ! binomial dist.
    integer function rbinom(n, a, seed) result(k)
        integer, intent(inout) :: seed
        integer, intent(in) :: n
        real(DP), intent(in) :: a
        real(DP) :: p, u
        integer :: b, i
        k = 0

        if (a .lt. 0 .or. a .gt. 1) then
            error stop "p must be in [0,1]."
        end if

        if (a .le. 0.5_dp) then
            p = a
        else
            p = 1.0_dp - a
        end if
        u = runif(0.0_dp, 1.0_dp, seed)
        do i = 1, n
            if (u .gt. (1.0_dp - p)) then
                b = 1
            else
                b = 0
            end if
            u = (u - (1.0_dp - p) * b) / (p * b + (1.0_dp - p) * (1 - b))
            k = k + b
        end do

        if (a .gt. 0.5) then
            k = n - k
        end if
    end function rbinom

    ! Cauchy dist.
    ! https://math.stackexchange.com/questions/484395/how-to-generate-a-cauchy-random-variable
    real(DP) function rcauchy(x0, gamma, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: x0, gamma

        if (gamma .le. 0) then
            error stop "Lambda must be strictly positive."
        end if

        x = gamma * tan(C_PI * (runif(0.0_dp, 1.0_dp, seed) - 0.5_dp)) + x0;

    end function rcauchy

    ! exponential dist.
    real(DP) function rexp(lambda, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: lambda
        if (lambda .le. 0) then
            error stop "Lambda must be strictly positive."
        end if

        x = -log(runif(0.0_dp, 1.0_dp, seed)) / lambda
    end function rexp

    ! f dist.
    real(DP) function rf(d1, d2, seed) result(x)
        integer, intent(inout) :: seed
        integer, intent(in) :: d1, d2
        real(DP) :: bx
        if (d1 .le. 0 .or. d2 .le. 0) then
            error stop "Parameters must be strictly positive."
        end if
        bx = rbeta(d1 / 2.0_dp, d2 / 2.0_dp, seed)
        x = (d2 * bx) / (d1 * (1 - bx))
    end function rf

    ! gamma dist.
    ! Marsaglia-Tsang method
    real(DP) function marsaglia_tsang(a, seed) result (y)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: a
        real(DP) :: d, c, x, v, u

        d = a - 1.0_dp / 3.0_dp
        c = 1.0_dp / sqrt(9.0_dp * d)
        do
            v = 0.0_dp
            do while(v .le. 0.0_dp)
                x = rnorm(0.0, 1.0, seed)
                v = 1.0_dp + c * x
            end do
            v = v**3
            u = runif(0.0, 1.0, seed)
            if (u .lt. 1.0_dp - 0.0331 * (x**2) * (x**2)) then
                y = d * v
                exit
            end if
            if (log(u) .lt. 0.5 * x * x + d * (1.0_dp - v + log(v))) then
                y = d * v
                exit
            end if
        end do
    end function marsaglia_tsang

    ! generate gamma random numbers
    real(DP) function rgamma(alpha, beta, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: alpha, beta
        real(DP) :: ms
        if (alpha .le. 0 .or. beta .le. 0) then
            error stop "Parameters must be strictly positive."
        end if
        if (alpha .gt. 1.0) then
            ms = marsaglia_tsang(alpha, seed)
            x = ms / beta
        else
            ms = marsaglia_tsang(alpha + 1.0_dp, seed)
            x = ms * (runif(0.0, 1.0, seed)**(1.0_dp / alpha)) / beta
        end if
    end function rgamma

    ! geometric dist.
    integer function rgeom(k, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: k
        real(DP) :: u
        if (k .le. 0 .or. k .gt. 1) then
            error stop "Parameter must be (0,1]."
        end if
        x = 0
        u = runif(0.0_dp, 1.0_dp, seed)
        do while(u .le. k)
            u = runif(0.0_dp, 1.0_dp, seed)
            x = x + 1
        end do
    end function rgeom

    ! normal dist.
    ! Box-Muller algorithm for generating normal random numbers
    real(DP) function rnorm(mu, sigma, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: mu, sigma
        real(DP) :: u1, u2
        if (sigma .le. 0) then
            error stop "Sigma^2 must be strictly positive."
        end if
        u1 = runif(0.0_dp, 1.0_dp, seed)
        u2 = runif(0.0_dp, 1.0_dp, seed)
        x = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * C_PI * u2)
        x = mu + sigma * x
    end function rnorm

    ! Poisson dist.
    ! http://luc.devroye.org/chapter_ten.pdf
    real(DP) function rpois(lambda, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: lambda
        real(DP) :: prod, sum, u

        if (lambda .le. 0) then
            error stop "lambda must be strictly positive."
        end if

        x = 0
        sum = exp(-lambda)
        prod = exp(-lambda)
        u = runif(0.0_dp, 1.0_dp, seed)
        do while(u .gt. sum)
            x = x + 1
            prod = (lambda / x) * prod
            sum = sum + prod
        end do
    end function rpois

    ! t dist.
    ! sources:
    ! 1. https://stats.stackexchange.com/questions/52906/student-t-as-mixture-of-gaussian/59520#59520
    ! 2. https://stats.stackexchange.com/questions/70266/generating-random-numbers-from-a-t-distribution/70283#70283
    real(DP) function rt(v, seed) result(x)
        integer, intent(inout) :: seed
        integer, intent(in) :: v
        real(DP) :: s2

        if (v .le. 0) then
            error stop "Degress of freedom must be strictly positive."
        end if

        s2 = 1.0_dp / rgamma(v / 2.0_dp, v / 2.0_dp, seed)
        x = rnorm(0.0_dp, sqrt(s2), seed)
    end function rt

    ! uniform dist.
    real(DP) function runif(a, b, seed) result(x)
        integer, intent(inout) :: seed
        real(DP), intent(in) :: a, b
        x = a + (b - a) * lcg(seed)
    end function runif

end module Distributions

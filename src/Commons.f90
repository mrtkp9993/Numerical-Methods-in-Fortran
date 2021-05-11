module Commons
    use Constants
    implicit none

contains

    ! Approximation to Error function
    real(DP) function erfun(x) result(y)
        real(DP), intent(in) :: x
        real(DP) :: xn
        real(DP) :: t
        real(DP) :: p = 0.3275911_dp
        real(DP) :: a1 = 0.254829592_dp
        real(DP) :: a2 = -0.284496736_dp
        real(DP) :: a3 = 1.421413741_dp
        real(DP) :: a4 = -1.453152027_dp
        real(DP) :: a5 = 1.061405429_dp

        if (x .lt. 0) then
            xn = -x
        else
            xn = x
        end if

        t = 1.0_dp / (1.0_dp + p * xn)
        y = sign(1.0_dp, x) &
                * (1.0_dp - (a1 * t + a2 * t**2 + a3 * t**3 + a4 * t**4 + a5 * t**5) * exp(-xn ** 2))
    end function erfun

    ! Approximation to Inverse Error function
    real(DP) function erfun_inv(x) result(y)
        real(DP), intent(in) :: x
        real(DP) :: a = 0.140012_dp

        y = sign(1.0_dp, x)                                                      &
                * sqrt(&
                        sqrt(((2.0_dp / (C_PI * a) + (log(1.0_dp - x**2)) / 2.0_dp)**2) &
                                - (log(1.0_dp - x**2)) / a)                          &
                                - ((2.0_dp / (C_PI * a)) + (log(1.0_dp - x**2)) / 2.0_dp))
    end function erfun_inv

    ! Approximation to standard normal dist.
    ! complementary cumulative distribution function
    ! Abramowitz and Stegun, Formula 26.2.23
    real(DP) function ccdf(p) result(xp)
        real(DP), intent(in) :: p
        real(DP) :: t
        real(DP) :: c0 = 2.515517_dp
        real(DP) :: c1 = 0.802853_dp
        real(DP) :: c2 = 0.010328_dp
        real(DP) :: d1 = 1.432788_dp
        real(DP) :: d2 = 0.189269_dp
        real(DP) :: d3 = 0.001308_dp

        t = sqrt(-2.0_dp * log(p))
        xp = t - ((c0 + c1 * t + c2 * t**2) / (1.0_dp + d1 * t + d2 * t**2 + d3 * t**3))
    end function ccdf

end module Commons
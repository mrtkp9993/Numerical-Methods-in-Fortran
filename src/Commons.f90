module Commons
    use Constants
    implicit none

    contains

        ! Approximation to Error function
        real(kind = RP) function erfun(x) result(y)
            real(kind = RP), intent(in) :: x
            real(kind = RP) :: xn
            real(kind = RP) :: t
            real(kind = RP) :: p = 0.3275911
            real(kind = RP) :: a1 = 0.254829592
            real(kind = RP) :: a2 = -0.284496736
            real(kind = RP) :: a3 = 1.421413741
            real(kind = RP) :: a4 = -1.453152027
            real(kind = RP) :: a5 = 1.061405429

            if (x .lt. 0) then
                xn = -x
            else
                xn = x
            end if

            t = 1.0 / (1 + p * xn)
            y = sign(1.0,x) &
                    * (1 - (a1*t + a2*t**2 + a3*t**3 + a4*t**4 + a5*t**5) * exp(-xn ** 2))
        end function erfun

        ! Approximation to Inverse Error function
        real(kind = RP) function erfun_inv(x) result(y)
            real(kind = RP), intent(in) :: x
            real(kind = RP) :: a = 0.140012

            y = sign(1.0,x)                                          &
                    * sqrt(                                          &
                            sqrt(((2/(C_PI*a) + (log(1-x**2))/2)**2) &
                                    -(log(1-x**2))/a)                &
                            -((2/(C_PI*a)) + (log(1-x**2))/2))
        end function erfun_inv

        ! Approximation to standard normal dist.
        ! complementary cumulative distribution function
        ! Abramowitz and Stegun, Formula 26.2.23
        real(kind = RP) function ccdf(p) result(xp)
            real(kind = RP), intent(in) :: p
            real(kind = RP) :: t
            real(kind = RP) :: c0 = 2.515517
            real(kind = RP) :: c1 = 0.802853
            real(kind = RP) :: c2 = 0.010328
            real(kind = RP) :: d1 = 1.432788
            real(kind = RP) :: d2 = 0.189269
            real(kind = RP) :: d3 = 0.001308

            t = sqrt(-2.0*log(p))
            xp = t - ((c0 + c1*t + c2*t**2) / (1 + d1*t + d2*t**2 + d3*t**3))
        end function ccdf

end module Commons
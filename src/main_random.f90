program main_random
    use Commons
    use Constants
    use Distributions
    use IO
    use Random
    implicit none

    real(dp), dimension(1000000, 13) :: randomData
    integer :: i, seed
    character(40) :: fileName
    fileName = "data.csv"
    seed = 12345

    print *, "Generating numbers..."
    do i = 1, 1000000
        print *, "Iter: ", i
        randomData(i, 1) = roundNearN(runif(3.0_dp, 7.0_dp, seed), 8)
        randomData(i, 2) = roundNearN(rnorm(4.0_dp, 0.3_dp, seed), 8)
        randomData(i, 3) = roundNearN(rgamma(5.0_dp, 5.0_dp, seed), 8)
        randomData(i, 4) = roundNearN(rbeta(1.0_dp, 3.0_dp, seed), 8)
        randomData(i, 5) = roundNearN(rexp(0.5_dp, seed), 8)
        randomData(i, 6) = rbinom(20, 0.7_dp, seed)
        randomData(i, 7) = roundNearN(rf(5, 2, seed), 8)
        randomData(i, 8) = rpois(4.0_dp, seed)
        randomData(i, 9) = rgeom(0.5_dp, seed)
        randomData(i, 10) = roundNearN(rt(5, seed), 8)
        randomData(i, 11) = roundNearN(rcauchy(-2.0_dp, 1.0_dp, seed), 8)
        randomData(i, 12) = roundNearN(rlognormal(4.0_dp, 0.3_dp, seed), 8)
    end do

    call writeCsv(randomData, fileName)

end program main_random

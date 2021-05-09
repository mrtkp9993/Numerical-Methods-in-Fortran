program main_random
    use Constants
    use Distributions
    use IO
    use Random
    implicit none

    real(dp), dimension(1000000,8) :: randomData
    integer :: i, seed
    character(40) :: fileName
    fileName = "data.csv"
    seed = 12345

    print *, "Generating numbers..."
    do i = 1, 1000000
        print *, "Iter: ", i
        randomData(i,1) = runif(3.0_dp,7.0_dp,seed)
        randomData(i,2) = rnorm(4.0_dp,0.3_dp,seed)
        randomData(i,3) = rgamma(5.0_dp,5.0_dp,seed)
        randomData(i,4) = rbeta(1.0_dp,3.0_dp,seed)
        randomData(i,5) = rexp(0.5_dp, seed)
        randomData(i,6) = rbinom(20, 0.7_dp, seed)
        randomData(i,7) = rf(5, 2, seed)
        randomData(i,8) = rpois(4.0_dp, seed)
    end do

    call writeCsv(randomData, fileName)

end program main_random

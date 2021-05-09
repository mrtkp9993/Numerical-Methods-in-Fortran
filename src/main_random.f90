program main_random
    use Constants
    use Distributions
    use IO
    use Random
    implicit none

    real(dp), dimension(1000,5) :: randomData
    integer :: i, seed
    character(40) :: fileName
    fileName = "data.csv"
    seed = 12345

    do i = 1, 1000
        randomData(i,1) = runif(3.0_dp,7.0_dp,seed)
        randomData(i,2) = rnorm(4.0_dp,0.3_dp,seed)
        randomData(i,3) = rgamma(5.0_dp,5.0_dp,seed)
        randomData(i,4) = rbeta(1.0_dp,3.0_dp,seed)
        randomData(i,5) = rexp(0.5_dp, seed)
    end do

    call writeCsv(randomData, fileName)

end program main_random

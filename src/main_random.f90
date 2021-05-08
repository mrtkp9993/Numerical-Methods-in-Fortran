program main_random
    use Constants
    use Distributions
    use IO
    use Random
    implicit none

    real(dp), dimension(1000,1) :: randomData
    integer :: i, seed
    character(40) :: fileName
    fileName = "data.csv"
    seed = 12345

    do i = 1, 1000
        randomData(i,1) = rgamma(1.0_dp,5.0_dp,seed)
    end do

    call writeCsv(randomData, fileName)

end program main_random

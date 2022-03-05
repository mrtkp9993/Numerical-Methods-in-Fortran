program main
   use Constants
   use Distr
   use Logging
   use IO
   use Random
   use Utils
   use Testing
   implicit none

   type(Logger) :: loggerM
   type(Tests) :: testerM

   real(DP) :: tol1 = 1e-8_dp

   real(DP) :: r1 = C_E**C_PI - C_PI
   real(DP) :: e1 = 19.999099979189475767_dp

   integer :: i1 = 7567423
   integer :: j = 3
   integer :: k = 7
   integer :: tmp

   real(DP), dimension(5, 5) :: mat1 = reshape( (/ 7.80, 6.69, 1.85, 8.07, 2.71, &
                                                   4.82, 6.48, 6.64, 1.89, 0.18, &
                                                   1.08, 7.93, 3.98, 0.56, 2.13, &
                                                   5.77, 4.46, 4.78, 4.28, 2.59, &
                                                   1.03, 0.16, 6.59, 7.72, 7.11 /), &
                                                shape(mat1), order=(/2,1/))

   real(dp), dimension(100000, 12) :: randomData
   integer :: i, seed
   character(40) :: fileName
   fileName = "data.csv"
   seed = 12345

   ! Logger first test
   call loggerM%plog("main", "Current precision is "//str(tol1))

   ! IO first test
   call printMatrix(mat1)
   call writeCsv(mat1, "test.csv")

   ! Tester init
   call testerM%init()
   call testerM%assertEqualsWith("e to the pi minus pi", r1, e1, tol1)
   call testerM%print()
   call testerM%end()

   ! Random 
   call loggerM%plog("main", "Generating numbers...")
   do i = 1, 2
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
   
   call loggerM%plog("main", "Generating numbers were done.")

end program main

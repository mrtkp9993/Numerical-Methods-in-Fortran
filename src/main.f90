program main
    use Constants
    use Logging
    use Utils
    use Testing
    implicit none

    type(Logger) :: loggerM
    type(Tests) :: testerM

    real(DP) :: tol1 = 1e-8_dp
    
    real(DP) :: r1 = C_E ** C_PI - C_PI
    real(DP) :: e1 = 19.999099979189475767_dp

    call loggerM%plog("main", "Current precision is " // str(tol1))

    call testerM%init()
    call testerM%assertEqualsWith("e to the pi minus pi", r1, e1, tol1)
    call testerM%print()
    call testerM%end()

end program main
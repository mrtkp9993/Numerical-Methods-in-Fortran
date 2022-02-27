program main
    use Assert
    use Constants
    use Logging
    use Utils
    implicit none

    type(Logger) :: loggerM

    real(DP) :: tol1 = 1e-8_dp
    
    real(DP) :: r1 = C_E ** C_PI - C_PI
    real(DP) :: e1 = 19.999099979189475767_dp

    logical :: res

    res = assertEqualsWith(r1, e1, tol1)

    call loggerM%plog("main", "Current precision is " // str(tol1))

end program main
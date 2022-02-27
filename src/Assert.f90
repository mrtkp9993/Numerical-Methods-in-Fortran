module Assert
    use Constants
    use Logging
    use Utils
    implicit none

    type(Logger) :: loggerA

contains

    logical function assertEqualsWith(actual, expected, tol) result(equal)
        real(DP), intent(in) :: actual, expected, tol

        if (abs(actual-expected) .le. tol) then
            equal = .true.
            call loggerA%plog("Assert", "Variables are equal with " // str(tol) // " tolerance.")
        else
            equal = .false.
            call loggerA%plog("Assert", "Variables are not equal with " // str(tol) // " tolerance.")
        end if
    end function assertEqualsWith

end module Assert

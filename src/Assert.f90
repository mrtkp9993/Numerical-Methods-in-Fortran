module Assert
    use Constants
    use Logging
    use Utils
    implicit none

    type(Logger) :: loggerA
    character(*), parameter :: moduleName = "Assert"

contains

    logical function assertEqualsWith(actual, expected, tol) result(equal)
        real(DP), intent(in) :: actual, expected, tol

        if (abs(actual-expected) .le. tol) then
            equal = .true.
            call loggerA%plog(moduleName, "Variables are equal with " // str(tol))
        else
            equal = .false.
            call loggerA%plog(moduleName, "Variables are not equal with " // str(tol))
        end if
    end function assertEqualsWith

end module Assert

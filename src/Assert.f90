module Assert
    use Constants
    implicit none

contains

    logical function assertEqualsWith(actual, expected, tol) result(equal)
        real(DP), intent(in) :: actual, expected, tol

        if (abs(actual-expected) .le. tol) then
            equal = .true.
            write(*,*) "Variables are equal to ", tol
        else
            equal = .false.
            write(*,*) "Variables are not equal to ", tol
        end if
    end function assertEqualsWith

end module Assert

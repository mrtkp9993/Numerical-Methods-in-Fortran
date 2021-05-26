module Assert
    use Constants
    implicit none

contains

    ! TODO Change functions to throw error
    logical function assertEqualsWith(actual, expected, tol) result(equal)
        real(DP), intent(in) :: actual, expected, tol

        if (abs(actual - expected) .le. tol) then
            equal = .true.
        else
            equal = .false.
        end if
    end function assertEqualsWith

end module Assert
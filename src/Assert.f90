module Assert
    use Constants
    implicit none

    contains
        logical function assertEquals(actual, expected) result(equal)
            real(kind=RP), intent(in) :: actual, expected

            if (actual .eq. expected) then
                equal = .true.
            else
                equal = .false.
            end if
        end function assertEquals

        logical function assertEqualsWith(actual, expected, tol) result(equal)
            real(kind=RP), intent(in) :: actual, expected, tol

            if ((actual - expected) .le. tol) then
                equal = .true.
            else
                equal = .false.
            end if
        end function assertEqualsWith

end module Assert
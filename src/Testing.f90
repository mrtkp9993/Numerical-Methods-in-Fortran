module Testing
    use Constants
    implicit none

    type Tests
        integer :: numTests, numFailed
        character(40), allocatable, dimension(:) :: failedTests
        contains
            procedure :: init
            procedure :: assertEquals, assertEqualsWith
            procedure :: print
    end type Tests
contains
    subroutine init(this)
        class(Tests), intent(inout) :: this
        allocate(this%failedTests(0))
        this%numTests  = 0
        this%numFailed = 0
    end subroutine init

    subroutine assertEquals(this, name, actual, expected)
        class(tests), intent(inout) :: this
        character(*), intent(in)    :: name
        real(kind=RP), intent(in)   :: actual, expected

        this%numTests = this%numTests + 1
        if (actual .ne. expected) then
            this%numFailed = this%numFailed + 1
            this%failedTests = [this%failedTests, name]
        end if
    end subroutine assertEquals

    subroutine assertEqualsWith(this, name, actual, expected, tol)
        class(tests), intent(inout) :: this
        character(*), intent(in)    :: name
        real(kind=RP), intent(in) :: actual, expected, tol

        this%numTests = this%numTests + 1
        if ((actual - expected) .ge. tol) then
            this%numFailed = this%numFailed + 1
            this%failedTests = [this%failedTests, name]
        end if
    end subroutine assertEqualsWith

    subroutine print(this)
        class(tests), intent(in) :: this

        if (this%numFailed .eq. 0) then
            print *, "All tests performed successfully."
        else
            print *, this%numFailed, " of ", this%numTests, " tests failed."
            print *, "Failed tests"
            print *, "------------"
            print *, this%failedTests
        end if
    end subroutine print
end module Testing
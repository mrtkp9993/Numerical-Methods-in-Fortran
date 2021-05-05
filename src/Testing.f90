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
        procedure :: end
    end type Tests
contains
    subroutine init(this)
        class(Tests), intent(inout) :: this
        allocate(this%failedTests(0))
        this%numTests = 0
        this%numFailed = 0
    end subroutine init

    subroutine assertEquals(this, name, actual, expected)
        class(tests), intent(inout) :: this
        character(*), intent(in) :: name
        real(kind = RP), intent(in) :: actual, expected

        this%numTests = this%numTests + 1
        if (actual .ne. expected) then
            this%numFailed = this%numFailed + 1
            this%failedTests = [this%failedTests, name]
        end if
    end subroutine assertEquals

    subroutine assertEqualsWith(this, name, actual, expected, tol)
        class(tests), intent(inout) :: this
        character(*), intent(in) :: name
        real(kind = RP), intent(in) :: actual, expected, tol

        this%numTests = this%numTests + 1
        if ((actual - expected) .ge. tol) then
            this%numFailed = this%numFailed + 1
            this%failedTests = [this%failedTests, name]
        end if
    end subroutine assertEqualsWith

    subroutine print(this)
        class(tests), intent(in) :: this
        integer :: i

        if (this%numFailed .eq. 0) then
            print *, ""
            print *, "All tests performed successfully."
            print *, ""
        else
            print *, ""
            print *, this%numFailed, " of ", this%numTests, " tests failed."
            print *, ""
            print *, "Failed tests"
            print *, "------------"
            do i = 1, this%numFailed
                print *, i, " ", this%failedTests(i)
            end do
            print *, ""
        end if
    end subroutine print

    subroutine end(this)
        class(tests), intent(inout) :: this

        if (this%numFailed .ne. 0) then
            deallocate(this%failedTests)
            error stop "There are failed tests."
        end if
    end subroutine end

end module Testing
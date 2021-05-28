module Testing
    use Constants
    implicit none

    !type Tests
!        integer :: numTests, numFailed
!        character(40), allocatable, dimension(:) :: failedTests
!        real(DP), allocatable, dimension(:) :: failedTestsActual
!        real(DP), allocatable, dimension(:) :: failedTestsExpctd
!    contains
!        procedure :: init
!        procedure :: assertEquals, assertEqualsWith, assertTrue
!        procedure :: print
!        procedure :: end
!    end type Tests
contains
!    subroutine init(this)
!        class(Tests), intent(inout) :: this
!        allocate(this%failedTests(0))
!        allocate(this%failedTestsActual(0))
!        allocate(this%failedTestsExpctd(0))
!        this%numTests = 0
!        this%numFailed = 0
!    end subroutine init
!
!    subroutine assertEquals(this, name, actual, expected)
!        class(tests), intent(inout) :: this
!        character(*), intent(in) :: name
!        real(DP), intent(in) :: actual, expected
!
!        this%numTests = this%numTests + 1
!        if (abs(actual - expected) .gt. C_EPSILON) then
!            this%numFailed = this%numFailed + 1
!            this%failedTests = [this%failedTests, name]
!            this%failedTestsActual = [this%failedTestsActual, actual]
!            this%failedTestsExpctd = [this%failedTestsExpctd, expected]
!        end if
!    end subroutine assertEquals
!
!    subroutine assertEqualsWith(this, name, actual, expected, tol)
!        class(tests), intent(inout) :: this
!        character(*), intent(in) :: name
!        real(DP), intent(in) :: actual, expected, tol
!
!        this%numTests = this%numTests + 1
!        if (abs(actual - expected) .gt. tol) then
!            this%numFailed = this%numFailed + 1
!            this%failedTests = [this%failedTests, name]
!            this%failedTestsActual = [this%failedTestsActual, actual]
!            this%failedTestsExpctd = [this%failedTestsExpctd, expected]
!        end if
!    end subroutine assertEqualsWith
!
!    subroutine assertTrue(this, name, actual)
!        class(tests), intent(inout) :: this
!        character(*), intent(in) :: name
!        logical, intent(in) :: actual
!
!        this%numTests = this%numTests + 1
!        if (.not. actual) then
!            this%numFailed = this%numFailed + 1
!            this%failedTests = [this%failedTests, name]
!            this%failedTestsActual = [this%failedTestsActual, 1.0_dp]
!            this%failedTestsExpctd = [this%failedTestsExpctd, 0.0_dp]
!        end if
!    end subroutine assertTrue
!
!    subroutine print(this)
!        class(tests), intent(in) :: this
!        integer :: i
!
!        if (this%numFailed .eq. 0) then
!            print *, ""
!            print *, "All tests performed successfully."
!            print *, ""
!        else
!            print *, ""
!            print *, this%numFailed, " of ", this%numTests, " tests failed."
!            print *, ""
!            print *, "Failed tests"
!            print *, "------------"
!            do i = 1, this%numFailed
!                print *, i, " ", this%failedTests(i), &
!                        "Actual: ", this%failedTestsActual(i), &
!                        "Expected: ", this%failedTestsExpctd(i)
!            end do
!            print *, ""
!        end if
!    end subroutine print
!
!    subroutine end(this)
!        class(tests), intent(inout) :: this
!
!        if (this%numFailed .ne. 0) then
!            deallocate(this%failedTests)
!            stop "There are failed tests."
!        end if
!
!        deallocate(this%failedTestsActual)
!        deallocate(this%failedTestsExpctd)
!    end subroutine end

end module Testing
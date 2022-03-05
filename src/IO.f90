module IO
   use Logging
   implicit none

   type(Logger) :: loggerIO
   character(*), parameter :: moduleName = "IO"

contains

   subroutine printMatrix(matrix)
      real, dimension(:, :), intent(in) :: matrix
      integer :: i
      call loggerIO%plog(moduleName, "printMatrix called.")
      do i = 1, size(matrix, 1)
         write (*, '(*(F16.8, ","))') matrix(i, :)
      end do
      call loggerIO%plog(moduleName, "printMatrix ended.")
   end subroutine printMatrix

   subroutine writeCsv(data, fileName)
      real, dimension(:, :), intent(in) :: data
      character(*), intent(in) :: fileName
      integer :: i, j
      call loggerIO%plog(moduleName, "writeCsv called.")

      j = size(data, 2)

      open (10, file=filename)
      do i = 1, size(data, 1)
         write (10, '(*(F16.8, ","))') data(i, :)
      end do
      close (10)
      call loggerIO%plog(moduleName, "writeCsv ended.")
   end subroutine writeCsv

end module IO

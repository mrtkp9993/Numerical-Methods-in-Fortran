module Logging

   implicit none

   type Logger
   contains
      procedure :: plog
   end type Logger

contains

   subroutine plog(this, moduleName, message)
      class(Logger), intent(inout) :: this
      character(*), intent(in) :: moduleName, message
      character(len=30) :: date

      call fdate(date)

      write (*, *), date, " - ", moduleName, " - ", message
   end subroutine plog

end module Logging

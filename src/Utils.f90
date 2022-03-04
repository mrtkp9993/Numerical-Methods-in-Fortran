module Utils
   use Constants
   implicit none
contains
   character(len=40) function str(k) result(str1)
      real(DP), intent(in) :: k
      write (str1, '(f20.10)') k
      str1 = adjustl(str1)
   end function str
end module Utils

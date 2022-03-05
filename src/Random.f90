module Random
    use Constants
    use Utils
    implicit none

contains

    ! Linear congruential generator
    real(DP) function lcg(seed) result(randUnif)
        integer, intent(inout) :: seed
        integer :: mult, incr, modl
        mult = 1103515245
        incr = 12345
        modl = 2**32
        seed = modulo(mult * seed + incr, modl)
        randUnif = seed / real(modl, kind = DP)
    end function lcg

    ! Lagged Fibonacci generator
    real(DP) function lfg(seed, j, k, mod) result(randUnif)
        integer, intent(inout) :: seed
        integer, intent(in) :: j, k, mod
        integer :: sz
        integer :: ix
        integer, dimension(:), allocatable :: td

        call num2digits(seed, td)
        if (size(td) .ne. k) then
            error stop "Digit count of seed is not equal to k."
        end if
        sz = modulo(td(j) + td(k), mod)
        td(1:size(td)-1) = td(2:size(td))
        td(size(td)) = sz
        ! print *, td
        seed = 0
        do ix = 1, size(td)
            seed = seed + (td(size(td)-ix+1) * 10**(ix-1))
         end do
        randUnif = sz / real(mod, kind = DP)
    end function lfg

end module Random
module Random
    use Constants
    implicit none

contains

    ! Linear congruential generator
    real(kind = RP) function lcg(seed) result(randUnif)
        integer, intent(inout) :: seed
        integer :: mult, incr, modl
        mult = 1103515245
        incr = 12345
        modl = 2**32
        seed = modulo(mult * seed + incr, modl)
        randUnif = seed / real(modl, kind = RP)
    end function lcg

end module Random
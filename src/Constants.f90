module Constants
    implicit none

    integer, parameter :: RP = selected_real_kind(16)

    real(kind=RP), parameter :: C_E        = 2.71828182845904523536
    real(kind=RP), parameter :: C_LOG2E    = 1.44269504088896340760
    real(kind=RP), parameter :: C_LOG10E   = 0.43429448190325182765
    real(kind=RP), parameter :: C_LN2      = 0.69314718055994530942
    real(kind=RP), parameter :: C_LN10     = 2.30258509299404568402
    real(kind=RP), parameter :: C_PI       = 3.14159265358979323846
    real(kind=RP), parameter :: C_PI_2     = 1.57079632679489661923
    real(kind=RP), parameter :: C_1_PI     = 0.31830988618379067154
    real(kind=RP), parameter :: C_2_PI     = 0.63661977236758134308
    real(kind=RP), parameter :: C_2_SQRTPI = 1.12837916709551257390
    real(kind=RP), parameter :: C_SQRT2    = 1.41421356237309504880
    real(kind=RP), parameter :: C_SQRT1_2  = 0.70710678118654752440
    real(kind=RP), parameter :: C_SQRTPI   = 1.77245385090551602730
    real(kind=RP), parameter :: C_SQRT2PI  = 2.50662827463100050242

end module Constants

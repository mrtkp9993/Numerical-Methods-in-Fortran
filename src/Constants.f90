module Constants
    implicit none

    integer, parameter :: DP = selected_real_kind(15, 307)

    real(DP), parameter :: C_E = 2.71828182845904523536_dp
    real(DP), parameter :: C_LOG2E = 1.44269504088896340760_dp
    real(DP), parameter :: C_LOG10E = 0.43429448190325182765_dp
    real(DP), parameter :: C_LN2 = 0.69314718055994530942_dp
    real(DP), parameter :: C_LN10 = 2.30258509299404568402_dp
    real(DP), parameter :: C_PI = 3.14159265358979323846_dp
    real(DP), parameter :: C_PI_2 = 1.57079632679489661923_dp
    real(DP), parameter :: C_1_PI = 0.31830988618379067154_dp
    real(DP), parameter :: C_2_PI = 0.63661977236758134308_dp
    real(DP), parameter :: C_2_SQRTPI = 1.12837916709551257390_dp
    real(DP), parameter :: C_SQRT2 = 1.41421356237309504880_dp
    real(DP), parameter :: C_SQRT1_2 = 0.70710678118654752440_dp
    real(DP), parameter :: C_SQRTPI = 1.77245385090551602730_dp
    real(DP), parameter :: C_SQRT2PI = 2.50662827463100050242_dp

    real(DP), parameter :: C_EPSILON = epsilon(C_E)

end module Constants

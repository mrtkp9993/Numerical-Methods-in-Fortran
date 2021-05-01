! ============================================================================
! Name        : Numerical-Methods-in-Fortran.f90
! Author      : Murat Koptur
! Version     :
! Copyright   : 
! Description : Hello World in Fortran
! ============================================================================

program main_plot
    use Plots
    implicit none

    integer, parameter    :: M = 50
    integer               :: x, y, i
    real, dimension(M, 2) :: matrix

    matrix(1, 1) = 0.0
    matrix(1, 2) = 0.0

    do i = 2, M
        matrix(i, 1) = 0.1 * i
        matrix(i, 2) = matrix(i, 1)**2
    end do

    call plot2d(matrix)

end program

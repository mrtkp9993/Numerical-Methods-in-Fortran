! ============================================================================
! Name        : Numerical-Methods-in-Fortran.f90
! Author      : Murat Koptur
! Version     :
! Copyright   : 
! Description : Hello World in Fortran
! ============================================================================

program main_plot
    use Constants
    implicit none

    integer, parameter    :: M = 50
    integer               :: x, y, i, j
    real, dimension(M, 2) :: matrix

    print '(a)', 'set datafile separator ","'
    print '(a)', 'set title "Plot Example"'
    print '(a)', 'set nokey'
    print '(a)', 'set grid'
    print '(a)', 'set xlabel "x"'
    print '(a)', 'set ylabel "y"'
    print '(a)', 'plot "-" using 1:2 with lines'

    matrix(1, 1) = 0.0
    matrix(1, 2) = 0.0

    do i = 2, M
        matrix(i, 1) = 0.1 * i
        matrix(i, 2) = matrix(i, 1)**2
    end do

    print '((F8.5, ",", F8.5))', ((matrix(i,j), j = 1, 2), i = 1, M)

end program

module Plots
    implicit none

contains

    subroutine plot2d(matrix)
        implicit none

        real, dimension(:, :), intent(in) :: matrix
        integer :: i, j

        print '(a)', 'set datafile separator ","'
        print '(a)', 'set nokey'
        print '(a)', 'set grid'
        print '(a)', 'set xlabel "x"'
        print '(a)', 'set ylabel "y"'
        print '(a)', 'plot "-" using 1:2 with lines'

        print '((F8.5, ",", F8.5))', ((matrix(i, j), j = 1, 2), i = 1, size(matrix, 1))

    end subroutine plot2d

end module Plots

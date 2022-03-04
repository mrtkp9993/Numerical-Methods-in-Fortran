module IO
    implicit none

contains

    subroutine printMatrix(matrix)
        real, dimension(:, :), intent(in) :: matrix
        integer :: i
        do i = 1, size(matrix, 1)
            write (*, '(*(F16.8, ","))') matrix(i, :)
        end do
    end subroutine printMatrix

    subroutine writeCsv(data, fileName)
        real, dimension(:, :), intent(in) :: data
        character(*), intent(in) :: fileName
        integer :: i, j
        j = size(data, 2)

        open(10, file = filename)
        do i = 1, size(data, 1)
            write (10, '(*(F16.8, ","))') data(i, :)
        end do
        close(10)
    end subroutine writeCsv

end module IO
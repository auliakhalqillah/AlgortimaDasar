program cal_derteminant
    implicit none

    integer :: i,j,io 
    integer :: m,n
    real, dimension(:,:), allocatable :: matrix
    real :: determinan,caldet

    ! Read number of data
    write(*,*) 'Status IOSTAT'
    open(1, file='data_matrix_high_A.txt')
    m = 0
    do
        read(1,*,iostat=io)
        write(*,*)'Baris ke-',m+1, io
        if (io /= 0) exit
        m = m + 1
    end do
    close(1)

    ! Set number of rows and columns of square matrix
    m = int(m**0.5)
    n = m

    ! Allocate memory size for array variable
    allocate(matrix(m,n))

    ! Read matrix file
    open(2,file='data_matrix_high_A.txt')
    do i = 1,m 
        do j = 1,n
            read(2,*) matrix(i,j)
        end do
    end do
    close(2)

    ! Show matrix
    write(*,*) 
    write(*,*) 'Matrix'
    do i = 1,m
        write(*,*) (matrix(i,j), j=1,n)
    end do

    write(*,*) 

    ! Calculate determinant by calling its function
    determinan = caldet(matrix,m,n)

    write(*,*) 'Determinan:'
    write(*,*) determinan

    deallocate(matrix)
end program

subroutine getsubmatrix(matrix,m,n,mainrow,maincolumn,submatrix)
    ! ------------------------------------------------------------------
    ! This is a subroutine to get a cofactor/submatrix from main matrix
    ! This concept algorithm is obtained from
    ! https://www.geeksforgeeks.org/adjoint-inverse-matrix/ in C++
    ! ------------------------------------------------------------------
    real, dimension(m,n) :: matrix ! input matrix
    real, dimension(m-1,n-1) :: submatrix ! output matrix
    integer :: maincolumn, mainrow, row, col

    i = 1
    j = 1
    do row = 1,m 
        do col = 1,n
            if ((row .ne. mainrow) .and. (col .ne. maincolumn)) then                     
                submatrix(i,j) = matrix(row,col)
                if (j == n-1) then 
                    j = 1
                    i = i + 1
                else
                    j = j + 1
                end if
            end if
        end do
    end do
    
end subroutine getsubmatrix

recursive function caldet(matrix,m,n) result(sumdet)
    ! ------------------------------------------------------------------
    ! This is a recursive function to calculate determinant.
    ! ------------------------------------------------------------------
    real, dimension(m,n) :: matrix
    real, dimension(m-1,n-1) :: submatrix
    integer :: j

    if (m == 2 .and. n == 2 ) then
        det = matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)
    else
        det = 0
        do j = 1,n
            sign = (-1)**(j-1)
            call getsubmatrix(matrix,m,n,1,j,submatrix)
            det = det + (sign * matrix(1,j) * caldet(submatrix,m-1,n-1))
        end do
    end if

    sumdet = det
    return
end function caldet
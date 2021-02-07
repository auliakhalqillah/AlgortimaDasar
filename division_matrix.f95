program divisionmatrix
    implicit none

    integer :: i,j,io,k
    integer :: m,n,mm,nn
    real, dimension(:,:), allocatable :: matrixA
    real, dimension(:,:), allocatable :: matrixB
    real, dimension(:,:), allocatable :: division
    real, dimension(:,:), allocatable :: invmat
    real, dimension(:,:), allocatable :: submatrix
    real, dimension(:,:), allocatable :: adjugate
    real :: determinant,caldet

    ! Read number of data for matrixA
    write(*,*) 'Status IOSTAT (Matrix A)'
    open(1, file='data_matrix_high_A.txt')
    m = 0
    do
        read(1,*,iostat=io)
        write(*,*)'Baris ke-',m+1, io
        if (io /= 0) exit
        m = m + 1
    end do
    close(1)

    ! Set number of rows and columns of square matrixA
    m = int(m**0.5)
    n = m 

    write(*,*)

    ! Read number of data for matrixB
    write(*,*) 'Status IOSTAT (Matrix B)'
    open(3, file='data_matrix_high_B.txt')
    mm = 0
    do
        read(3,*,iostat=io)
        write(*,*)'Baris ke-',mm+1, io
        if (io /= 0) exit
        mm = mm + 1
    end do
    close(3)

    ! Set number of rows and columns of square matrixB
    mm = int(mm**0.5)
    nn = mm 

    ! Allocate memory size for array variables
    allocate(matrixA(m,n), matrixB(m,n), division(m,n), invmat(m,n), adjugate(m,n), submatrix(m-1,n-1))

    ! Read matrixA file
    open(2,file='data_matrix_high_A.txt')
    do i = 1,m 
        do j = 1,n
            read(2,*) matrixA(i,j)
        end do
    end do
    close(2)

    ! Read matrixB file
    open(4,file='data_matrix_high_B.txt')
    do i = 1,mm 
        do j = 1,nn
            read(4,*) matrixB(i,j)
        end do
    end do
    close(4)

    ! Show matrixA
    write(*,*) 
    write(*,*) 'matrixA'
    do i = 1,m
        write(*,*) (matrixA(i,j), j=1,n)
    end do

    write(*,*) 

    ! Show matrixB
    write(*,*) 
    write(*,*) 'matrixB'
    do i = 1,mm
        write(*,*) (matrixB(i,j), j=1,nn)
    end do

    write(*,*)

    ! Calculate determinant by calling its function
    determinant = caldet(matrixA,m,n)

    write(*,*) 'determinant:'
    write(*,*) determinant

    write(*,*)

    ! Get adjugate matrixA
    write(*,*) 'Adjugate'

    call getadjugate(matrixA,m,n,adjugate)

    ! Show adjugate matrixA
    do i = 1,m
        write(*,*) (adjugate(i,j), j=1,n)
    end do 

    ! Calculate inverse matrixA
    do i = 1,m 
        do j = 1,n 
            invmat(i,j) = (1/determinant) * adjugate(i,j)
        end do
    end do

    write(*,*)

    write(*,*) 'Inverse matrixA'
    do i = 1,m 
        write(*,*) (invmat(i,j), j= 1,m)
    end do

    ! Calculate division on matrix B/A = B*inv(A)
    do i = 1,m 
        do j = 1,n
            division(i,j) = 0
            do k = 1,m 
                division(i,j) = division(i,j) + (matrixB(i,k) * invmat(k,j))
            end do
        end do
    end do

    write(*,*)

    write(*,*) 'Division Matrix'
    do i = 1,m 
        write(*,*) (division(i,j), j = 1,n)
    end do

    ! Deallocate memory size for array variables
    deallocate(matrixA, matrixB, division, invmat, adjugate, submatrix)

end program

subroutine getsubmatrix(matrixA,m,n,mainrow,maincolumn,submatrix)
    ! ------------------------------------------------------------------
    ! This is a subroutine to get a cofactor/submatrix from main matrixA
    ! This concept algorithm is obtained from
    ! https://www.geeksforgeeks.org/adjoint-inverse-matrixA/ in C++
    ! ------------------------------------------------------------------
    real, dimension(m,n) :: matrixA ! input matrixA
    real, dimension(m-1,n-1) :: submatrix ! output matrixA
    integer :: maincolumn, mainrow, row, col

    i = 1
    j = 1
    do row = 1,m 
        do col = 1,n
            if ((row .ne. mainrow) .and. (col .ne. maincolumn)) then                     
                submatrix(i,j) = matrixA(row,col)
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

    
recursive function caldet(matrixA,m,n) result(sumdet)
    ! ------------------------------------------------------------------
    ! This is a recursive function to calculate determinant.
    ! ------------------------------------------------------------------
    real, dimension(m,n) :: matrixA
    real, dimension(m-1,n-1) :: submatrix
    integer :: j

    if (m == 2 .and. n == 2 ) then
        det = matrixA(1,1)*matrixA(2,2) - matrixA(1,2)*matrixA(2,1)
    else
        det = 0 
        do j = 1,n
            sign = (-1)**(j-1)
            call getsubmatrix(matrixA,m,n,1,j,submatrix)
            det = det + (sign * matrixA(1,j) * caldet(submatrix,m-1,n-1))
        end do
    end if

    sumdet = det
    return
end function caldet

subroutine getadjugate(matrixA,m,n,adj)
    ! ------------------------------------------------------------------
    ! This is a subroutine to get a adjugate matrixA. Adjugate matrixA
    ! is a square matrixA that transposed of its cofactor matrixA
    !! ------------------------------------------------------------------
    real, dimension(m,n) :: matrixA, adj
    real, dimension(m-1,n-1) :: submatrix
    real :: caldet 

    if (m == 2 .and. n == 2) then
        ! adjugate for 2x2 matrix
        adj(1,1) = 1 * matrixA(2,2)
        adj(1,2) = -1 * matrixA(1,2)
        adj(2,1) = -1 * matrixA(2,1)
        adj(2,2) = 1 * matrixA(1,1)
    else
        ! adjugate for 3x3 matrix and higher
        do i = 1,m 
            do j = 1,n
                sign = (-1)**(i+j)
                call getsubmatrix(matrixA,m,n,i,j,submatrix)
                ! adj(i,j) is cofactor and adj(j,i) is adjugate (transposed)
                adj(j,i) = sign * caldet(submatrix,m-1,n-1)
            end do
        end do
    end if
end subroutine getadjugate
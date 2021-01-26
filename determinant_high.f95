program cal_derteminant
    implicit none

    integer :: i,j,io 
    integer :: m,n
    real, dimension(:,:), allocatable :: matrix
    real :: determinan,caldet

    write(*,*) 'Status IOSTAT'
    open(1, file='data_matrix_high.txt')
    m = 0
    do
        read(1,*,iostat=io)
        write(*,*)'Baris ke-',m+1, io
        if (io /= 0) exit
        m = m + 1
    end do
    close(1)

    m = int(m**0.5)
    n = m ! banyak baris sama dengan banyak kolom

    allocate(matrix(m,n))

    open(2,file='data_matrix.txt')
    do i = 1,m 
        do j = 1,n
            read(2,*) matrix(i,j)
        end do
    end do
    close(2)

    write(*,*) 
    write(*,*) 'Matrix'
    do i = 1,m
        write(*,*) (matrix(i,j), j=1,n)
    end do

    write(*,*) 

    ! Menghitung determinan dengan memanggil fungsi caldet
    determinan = caldet(matrix,m,n)

    write(*,*) 'Determinan:'
    write(*,*) determinan

    deallocate(matrix)
end program

subroutine getsubmatrix(matrix,m,n,maincolumn,submatrix)
! ----------------------------------------------------------
! Mendapatkan kofaktor matrix
! Oleh: Aulia Khalqillah,S.Si.,M.Si
! ----------------------------------------------------------
    real, dimension(m,n) :: matrix ! input matrix
    real, dimension(m-1,n-1) :: submatrix ! output matrix
    integer :: maincolumn

    do i = 1,m-1
        status = 0
        do j = 1,n-1
            if (j .eq. maincolumn) then
                submatrix(i,j) = matrix(i+1,j+1)
                status = 1
            else
                if (status == 0) then
                    submatrix(i,j) = matrix(i+1,j)
                else
                    submatrix(i,j) = matrix(i+1,j+1)
                end if
            end if
        end do
    end do
    
end subroutine getsubmatrix

recursive real function caldet(matrix,m,n) result(sumdet)
! ----------------------------------------------------------
! Menghitung determinan matrix dimensi tinggi
! Oleh: Aulia Khalqillah,S.Si.,M.Si
! ----------------------------------------------------------
    real, dimension(m,n) :: matrix
    real, dimension(m-1,n-1) :: submatrix
    integer :: j

    if (m == 2 .and. n == 2 ) then
        det = matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)
    else
        det = 0
        do j = 1,n
            sign = (-1)**(j-1)
            call getsubmatrix(matrix,m,n,j,submatrix)
            det = det + (sign * matrix(1,j) * caldet(submatrix,m-1,n-1))
        end do
    end if

    sumdet = det
    return
end function caldet
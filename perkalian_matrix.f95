program perkalian_matrix
    implicit none

    integer :: i,j,k
    real :: a(3,3), b(3,3), c(3,3)

    ! Defenisikan nilai dalam matrix
    a(1,1) = 1
    a(1,2) = 2
    a(1,3) = 3
    a(2,1) = 4
    a(2,2) = 5
    a(2,3) = 6
    a(3,1) = 7
    a(3,2) = 8
    a(3,3) = 9

    b(1,1) = 2
    b(1,2) = 4
    b(1,3) = 6
    b(2,1) = 1
    b(2,2) = 3
    b(2,3) = 5
    b(3,1) = 1
    b(3,2) = 2
    b(3,3) = 3

    ! Cara lain menampilkan matrix
    write(*,*) 'Matrix a'
    do i = 1,3
        write(*,*) (a(i,j), j=1,3)
    end do

    write(*,*)

    write(*,*) 'Matrix b'
    do i = 1,3
        write(*,*) (b(i,j), j=1,3)
    end do

    write(*,*)

    ! PERKALIAN MATRIX
    ! Mengalikan matrix dengan suatu nilai/konstanta
    write(*,*) 'Mengalikan matrix dengan suatu nilai/konstanta'
    do i = 1,3
        write(*,*) (a(i,j) * 2, j = 1,3)
    end do

    write(*,*)

    ! Mengalikan matrix dengan matrix
    do i = 1,3
        do j = 1,3
            c(i,j) = 0
            do k = 1,3
                c(i,j) = c(i,j) + (a(i,k) * b(k,j))
            end do
        end do
    end do

    ! Menampilkan hasil perkalian matrix dengan matrix
    write(*,*) 'Menampilkan hasil perkalian matrix dengan matrix'
    do i = 1,3
        write(*,*) (c(i,j), j = 1,3)
    end do

end program perkalian_matrix
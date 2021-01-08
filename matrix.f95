program matrixsederhana
    implicit none

    integer :: i,j
    real :: a(3,2)

    ! Defenisikan nilai dalam matrix
    a(1,1) = 1
    a(1,2) = 2
    a(2,1) = 4
    a(2,2) = 5
    a(3,1) = 7
    a(3,2) = 8

    ! Tampilkan matrix
    do i = 1,3
        do j = 1,2
            write(*,*) a(i,j)
        end do
    end do

    write(*,*)

    ! Cara lain menampilkan matrix
    do i = 1,3
        write(*,*) (a(i,j), j=1,2)
    end do

    write(*,*)

    ! Menjumlahkan matrix dengan suatu nilai
    do i = 1,3
        write(*,*) (a(i,j) + 5, j=1,2)
    end do

    write(*,*)

    ! Mengurangkan matrix dengan suatu nilai
    do i = 1,3
        write(*,*) (a(i,j) - 2, j=1,2)
    end do

    write(*,*)

    ! Menjumlahkan matrix dengan matrix
    do i = 1,3
        write(*,*) (a(i,j) + a(i,j), j=1,2)
    end do

    write(*,*)
    
    ! Mengurangkan matrix dengan matrix
    do i = 1,3
        write(*,*) (a(i,j) - a(i,j), j=1,2)
    end do

end program matrixsederhana
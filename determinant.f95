program cal_derteminant
    implicit none

    integer :: i,j,io
    integer :: m,n
    real, dimension(:,:), allocatable :: matrix
    real :: determinan,caldet

    ! -------------------------------------------------
    ! Mengidentifikasi banyak data matrix
    ! -------------------------------------------------
    ! IOSTAT adalah atribut pada fortran untuk 
    ! mengidentifikasi banyakanya suatu data dalam
    ! sebuah file.
    ! 
    ! IOSTAT adalah tipe data integer.
    ! 
    ! Jika IOSTAT bernilai nol (0), maka mengindikasikan
    ! bahwa data diidentifikasi dengan baik dalam file 
    ! terkait.
    ! 
    ! Jika IOSTAT bernilai -1, maka mengindikasikan
    ! akhir dari data dalam file terkait.
    ! 
    ! Info lebih lanjut tentang IOSTAT
    ! https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap04/iostatus.html
    ! 
    write(*,*) 'Status IOSTAT'
    open(1, file='data_matrix.txt')
    m = 0
    do
        read(1,*,iostat=io)
        write(*,*)'Baris ke-',m+1, io
        if (io /= 0) exit
            m = m + 1
    end do
    close(1)

    ! -------------------------------------------------
    ! Menghitung akar dari banyak data matrix.
    ! -------------------------------------------------
    ! Misalnya bnayak data matrix = 4, maka
    ! banyaknya baris dan kolom matrix adalah akar 4
    ! atau sama dengan 2. Dengan kata lain, data matrix
    ! yang digunakan memiliki 2 baris dan 2 kolom.
    ! 
    m = int(m**0.5)
    n = m
    write(*,*)
    write(*,*) 'Banyak baris dan kolom:',n

    ! ALLOCATE adalah fungsi dalam fortran yang digunakan
    ! untuk mengalokasikan ukuran/banyak/dimensi suatu data 
    ! dengan nilai yang tertentu ke dalam memori perangkat
    ! tanpa harus mendeklarasikan nilai tersebut di awal
    allocate(matrix(m,n))

    ! Membaca file/data matrix
    open(2,file='data_matrix.txt')
    do i = 1,m 
        do j = 1,n
            read(2,*) matrix(i,j)
        end do
    end do
    
    ! Menampilkan matrix
    write(*,*)
    write(*,*) 'Matrix 2x2'
    do i = 1,m
        write(*,*) (matrix(i,j), j = 1,n)
    end do
    close(2)

    ! Menghitung determinan dengan memanggil fungsi caldet
    determinan = caldet(matrix,m,n)
    write(*,*)
    write(*,*) 'Determinan:'
    write(*,*) determinan

    ! DEALLOCATE adalah menonaktifkan alokasi data yang
    ! dibuat sebelumnya.
    deallocate(matrix)

end program

real function caldet(matrix,m,n)
! ----------------------------------------------------------
! Fungsi perhitungan determinan matrix 2x2
! ----------------------------------------------------------
    real, dimension(m,n) :: matrix
    caldet = matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)
    return
end function caldet
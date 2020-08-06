! email: personal@auliakhalqillah.com
program bilanganganjil
    implicit none
    integer :: i,n 
    real :: xodd

    write(*,*) 'Bilangan Ganjil'
    n = 10 ! banyak data
    xodd = 1 ! inisial bilangan ganjil
    do i = 1,n 
        write(*,*) xodd
        xodd = xodd + 2
    end do

end program

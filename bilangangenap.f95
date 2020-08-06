! email: personal@auliakhalqillah.com
program bilangangenap
    implicit none
    integer :: i,n 
    real :: xeven

    write(*,*) 'Bilangan Genap'
    n = 10 ! banyak data
    xeven = 2 ! inisial bilangan genap
    do i = 1,n 
        write(*,*) xeven
        xeven = xeven + 2
    end do

end program

! email: personal@auliakhalqillah.com
program deretfibbonacci
    implicit none
    integer :: i,n 
    real :: x,y,collect

    x = 0 ! deret pertama fibbonacci
    y = 1
    n = 10 ! banyak deret data yang dihasilkan
    write(*,*) 'Deret Fibbonacci'
    do i = 1,n
        write(*,*) x
        collect = x + y
        x = y
        y = collect
    end do 

end program

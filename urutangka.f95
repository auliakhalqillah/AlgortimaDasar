program urutangka
    implicit none
    real :: data(3), store
    integer :: i,j,n

    data = [3,4,1]
    n = size(data)
    do i = 1,n
        do j = 1,n-1
            if (data(j) > data(j+1)) then
                store = data(j)
                data(j) = data(j+1)
                data(j+1) = store
            end if
        end do
    end do
    write(*,*) data
end program 
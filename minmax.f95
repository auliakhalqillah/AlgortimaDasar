! email: personal@auliakhalqillah.com
program minmax
    implicit none
    real :: data(3), minimum, maximum
    integer :: i,j,n
    
    data = [2,1,4]
    n = 3

    ! Nilai Minimum
    minimum = data(1)
    i = 1
    do i = 1,n-1
        if (minimum < data(i+1)) then
            minimum = minimum
        else
            minimum = data(i+1)
        end if
    end do
    print*, 'Nilai Minimum:',minimum

    ! Nilai Maximum
    maximum = data(1)
    j = 1
    do j = 1,n-1
        if (maximum > data(j+1)) then
            maximum = maximum
        else
            maximum = data(j+1)
        end if
    end do
    print*, 'Nilai Maksimum:',maximum

end program minmax
program rec_sum
    implicit none
    integer :: n, io, i
    real, dimension(:), allocatable :: data
    real :: total_sum, itersum
    real :: start_time, finish_time
    character(len=100) :: fmt

    call cpu_time(start_time)
    write(*,*) 'Iterative Summation Function'
    write(*,*)

    open(1, file='data_recursive.txt')
    n = 0
    do 
        read(1,*,iostat=io)
        if (io /= 0) exit
        n = n + 1 
    end do
    close(1)

    allocate(data(n))
    open(2, file='data_recursive.txt')
    do i = 1,n
        read(2,*) data(i)
    end do
    close(2)

    write(*,*) 'DATA:', data
    write(*,*) 'N DATA:', n

    total_sum = itersum(data,n)
    write(*,*) 'Total:', total_sum
    deallocate(data)

    call cpu_time(finish_time)
    fmt = '(a, f10.7, a)'
    write(*,fmt) 'Time Processing:', finish_time-start_time, ' seconds'

end program

function itersum(data,n) result(total)
    real,dimension(n) :: data
    real :: total
    
    total = 0
    do i = 1,n
        total = total + data(i)
    end do 
    return
end function itersum
program main
implicit none
real(8),parameter :: pi=3.14159265358979323846D0
real(8) :: mypi, h, mysum=0.0D0, x
integer :: n, i

n=1000000000
h=1.0D0/n
do i=1, n
    x=h*(dble(i)-0.5D0)
    mysum=mysum+f(x)
enddo
mypi=h*mysum
write(*,100)mypi
100 format('mypi=',f14.12)

contains    
    function f(x)
        real(8) :: f, x
        f=4.d0/(1.d0 + x*x)
    end function f

end program


program main  
use omp_lib
implicit none
integer,parameter :: n_threads=4
real(8),parameter :: pi=3.14159265358979323846D0
real(8) :: mypi=0.0D0, h, mysum(n_threads)=0.0D0, x
integer :: n=1000000000, i,myid

h=1.0D0/n
!$OMP PARALLEL NUM_THREADS(n_threads) PRIVATE(myid,i,x)
      myid=omp_get_thread_num();
      myid=myid+1;
      do i=myid, n, n_threads
          x=h*(dble(i)-0.5D0)
          mysum(myid)=mysum(myid)+f(x)
      enddo
!$OMP END PARALLEL
do i=1,n_threads
    mypi=mypi+mysum(i)
enddo
mypi=h*mypi

write(*,100)mypi
100 format('mypi=',f15.12)
contains
    function f(x)
        real(8) :: f, x
        f=4.d0/(1.d0 + x*x)
    end function f
end program

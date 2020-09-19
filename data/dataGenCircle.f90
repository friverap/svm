program genera_data

implicit none

integer			::			ns, nsn, nd, i, j, io, cac
real			::			tmp,tmp2
real, dimension(:), allocatable		::		x,y

ns = 1000
nsn = 1*ns
nd = 2

!call random

allocate(x(ns),y(ns))
call random_number(x)
call random_number(y)

open(unit=3, file='trainingC.dat', form='formatted', action='write')
open(unit=4, file='trainingC.gnuplot',action='write')

write(unit=3,fmt='(I4)') ns
write(unit=3,fmt='(I4)') nd

do i=1,ns
	x(i) = x(i)*2.0 - 1.0
	y(i) = y(i)*2.0 - 1.0
end do

do i=1,ns
	if (sqrt((x(i)+0.5)**2+(y(i)+0.5)**2).LT.0.4.or.sqrt((x(i)-0.5)**2+(y(i)-0.5)**2).LT.0.4)then!&
	   ! .or.sqrt((x(i)+0.5)**2+(y(i)-0.5)**2).LT.0.4.or.sqrt((x(i)-0.5)**2+(y(i)+0.5)**2).LT.0.4) then
		tmp = 1.0
	else
		tmp = -1.0
	end if
	write(unit=3,fmt='(3f16.4)') x(i),y(i),tmp
	write(unit=4,fmt='(3f16.4)') x(i),y(i),tmp
end do

close(unit=3)
close(unit=4)
open(unit=2, file='predictingC.dat', form='formatted', action='write')

deallocate(x,y)
allocate(x(nsn),y(nsn))

write(2,fmt=*) nsn
call random_number(x)
call random_number(y)

do i=1,ns
	x(i) = x(i)*2.0 - 1.0
	y(i) = y(i)*2.0 - 1.0
end do

do i=1,nsn
	write (unit=2,fmt='(2f16.4)') x(i),y(i)
end do

close(2)

deallocate(x,y)

contains

subroutine random

  implicit none

  character(len=10)                   ::  date, time, zone
  integer, dimension(:), allocatable  ::  seed
  integer                             ::    n
  integer                             ::    i,io
  integer, dimension(8)               ::  values
  
  n = 10
  
  allocate(seed(n),stat = io)
    if (io.NE.0) then
        print *,'Memory error while runing genetic algorithms'
        stop
    end if

  do i = 1,n
    call date_and_time(date,time,zone,values)
    seed(i) = real(values(8))
  end do
  
  call random_seed(size=n)

  call random_seed(put=seed)

  deallocate(seed)
  
end subroutine random


end program genera_data

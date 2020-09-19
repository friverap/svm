program genera_data2

implicit none

integer			::			ns, nsn, nd, i, j, io, cac
real			::			tmp,tmp2,pi,alpha1,alpha2,dx,xmax,xmin
real, dimension(:), allocatable		::		x,noise

ns = 1000
nsn = ns
pi = 4.0d0*atan(1.0d0)
nd = 1
alpha1 = pi/4.0
alpha2 = 4.0
xmax = 5.0
xmin = -5.0
allocate(x(ns),noise(ns))

call random_number(noise)
open(unit=3, file='training.dat', form='formatted', action='write')
open(unit=4, file='training.gnuplot',action='write')

write(unit=3,fmt='(I4)') ns
write(unit=3,fmt='(I4)') nd
write(unit=3,fmt='(I4)') nd

dx  = (xmax - xmin)/dble(Ns)

do i=1,Ns
   x(i)  = xmin + dble(i) * dx
end do

do i=1,ns
	tmp2 = x(i)
	tmp = sin(abs(pi*tmp2))/abs(pi*tmp2) + log(noise(i)*3.0)/20.0
	write(unit=3,fmt='(2f16.4)') tmp2, tmp
	write(unit=4,fmt='(2f16.4)') tmp2, tmp
end do

close(unit=3)
close(unit=4)
open(unit=2, file='predicting.dat', form='formatted', action='write')

!deallocate(x)
!allocate(x(nsn))

write(2,fmt=*) nsn

do i=1,nsn
	tmp = x(i)
	write (unit=2,fmt='(f16.4)') tmp
end do

close(2)

deallocate(x)

end program genera_data2


subroutine read_data
	
use globals
use arrays
	implicit none
	
	integer			::			i,j


	if (action.eq.'training') then
		open(unit = 1, file = "/Users/friverap/repositorio/Pancho/svm_v3/data/training.dat", action = 'read', status='old', iostat=io)
		if (io.NE.0) stop 'could not open data file.'
		
		read(unit = 1,fmt = *) ns
		read(unit = 1,fmt = *) ndx
		if (svm_type.eq.'regression') then
			read(unit = 1,fmt = *) ndy
		else
			ndy = 1
		end if
		
		call allocate()
		
		do i=1,ns
				read(unit = 1,fmt = *) xin(i,:),yin(i,:)
		end do
	close(unit = 1)
	else if (action.eq.'prediction') then
		open(unit = 2, file = "/Users/friverap/repositorio/Pancho/svm_v3/xxx/train.dat", action = 'read', status='old', iostat=io)
		if (io.NE.0) stop 'could not open data file.'
	
		read(unit = 2,fmt = *) ns
		if (svm_type.eq.'regression') then
			read(unit = 2,fmt = *) ndy
		else
			ndy = 1
		end if

		read(unit = 2,fmt = *) ndx
		read(unit = 2,fmt = *) kernel_type
		read(unit = 2,fmt = *) s1
		read(unit = 2,fmt = *) s2
		read(unit = 2,fmt = *) gamma

		call allocate()
		
		do j = 1,ndy
			do i = 1,ns
				read(unit = 2,fmt='(E16.4)') alpha(i,j)
			end do
		end do
		
		do i = 1,ndy
			read(unit = 2,fmt='(E16.4)') b(i)
		end do
		
		do i = 1,ns
			read(unit = 2,fmt='(E16.4)') xin(i,:)
		end do
	close(unit = 2)		
	end if
	


end subroutine read_data


subroutine allocate
use globals
use arrays
!stat is a status varible that is set to a positive value if an error is detected and is set to zero otherwise. If there is no status variable, the occurrence of an error causes the program to terminate.
	if (action.eq.'training') then
		allocate(H(ns,ns),stat=merror)
	end if

	allocate(xin(ns,ndx),stat=merror)

	allocate(yin(ns,ndy),stat=merror)

	allocate(b(ndy),stat=merror)
	
	allocate(alpha(ns,ndy),stat=merror)

end subroutine allocate

subroutine svm_end
use globals
use arrays
	if (allocated(H)) deallocate(H)
	if (allocated(xin)) deallocate(xin)
	if (allocated(yin)) deallocate(yin)
	if (allocated(b)) deallocate(b)
	if (allocated(alpha)) deallocate(alpha)
	
	stop
		
end subroutine svm_end


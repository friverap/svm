subroutine training
	use globals
	use arrays
	
	real (kind=8), dimension(:), allocatable	::		n, v, ones
	integer										::		i,j,k

	print *, ns
	print *, ndx
	print *,ndy
	
	call grad
	if (ga_use.EQV..FALSE.) then
		open(unit = 10, file = 'train.dat', status = 'replace')
		write(10,fmt=*) ns
		if (svm_type.eq.'regression') write(10,fmt=*) ndy
		write(10,fmt=*) ndx
		write(10,fmt=*) kernel_type
		write(10,fmt=*) s1
		write(10,fmt=*) s2
		write(10,fmt=*) gamma
		
		do j = 1,ndy
			do i = 1,ns
				write(10,fmt='(E16.4)') alpha(i,j)
			end do
		end do
		
		do i = 1,ndy
			write(10,fmt='(E16.4)') b(i)
		end do
		
		do i = 1,ns
			write(10,fmt='(E16.4)') xin(i,:)
		end do
		
		close(10)
	
	
		deallocate(n,v,ones,stat=merror)
		
		print *,''
		print *,'Training completed.'
		print *,''
		
		call svm_end
			if (merror.NE.0) then
				print *,'Memory error finishing training'; call svm_end; end if
	else
		deallocate(n,v,ones,stat=merror)
			if (merror.NE.0) then
				print *,'Memory error finishing training'; call svm_end; end if
	end if
	
end subroutine training


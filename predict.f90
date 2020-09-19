subroutine predicting
	use globals 
	use arrays
	
	real(kind=8)		::		tmp1, tmp2, tmp3
	integer				::		i,j,k,l
	logical, dimension(:), allocatable			::		tmp4

	open(unit = 15, file = "/Users/friverap/repositorio/Pancho/svm_v3/data/predicting.dat", &
	action = 'read', status='old', form='formatted', iostat=io)
	if (io.NE.0) then
		print *,'could not open data file.'; call svm_end;
	end if
	read (unit = 15,fmt=*) nsn
	print *, nsn
	print *, ns
	print *, ndx
	print *,ndy
	print *,s1
	print *,s2
	allocate(xnew(nsn,ndx),ynew(nsn,ndy),xprime(ns),omega(ns),stat=merror)
	if (merror.NE.0) then
		print *,"Memory error. New data could not be allocated"; call svm_end; end if

	do i=1,nsn
		do j=1,ndx
			read(unit = 15,fmt='(F16.4)', advance = 'no') xnew(i,j)
		end do
		read(unit = 15,fmt='(x)')
	end do

	close(unit = 15)
	
		do i = 1,nsn
			do j = 1,ndx
						print *, xnew(i,j)
										
			end do
		end do
		
	! ******************************* REGRESSION
	if (svm_type.eq.'regression') then
		if (kernel_type.eq.'polinomial') then
		
			do k = 1,ndy
				do i = 1,nsn
					do j = 1,ns
						omega(j) = (dot_product(xnew(i,:),xin(j,:))+s1)**s2
					end do
					ynew(i,k) = dot_product(omega,alpha(:,k))+b(k)
				end do
			end do
		else if (kernel_type.eq.'RBF') then
			do k = 1,ndy
				do i = 1,nsn
					do j = 1,ns
						tmp1 = dot_product(xnew(i,:),xnew(i,:))
						tmp2 = dot_product(xnew(i,:),xin(j,:))
						tmp3 = dot_product(xin(j,:),xin(j,:))
						omega(j) = dexp(-(tmp1-2.d0*tmp2+tmp3)/(s1*s1))
					end do
					ynew(i,k) = dot_product(omega(1:ns),alpha(:,k))+b(k)
				end do
			end do
		else if (kernel_type.eq.'fourier') then
			do k = 1,ndy
				do i = 1,nsn
					do j = 1,ns
						tmp1 = 1.d0
						do l = 1,ndx
							tmp2 = (1.d0-s1*s1)/(2.d0*(1.d0-2.d0*s1*dcos(xin(i,l)-xin(j,l))+s1*s1))
							tmp1 = tmp1*tmp2
						end do
						omega(j) = tmp1
					end do
					ynew(i,k) = dot_product(omega,alpha(:,k))+b(k)
				end do
			end do
		end if
	else if (svm_type.eq.'classification') then! *************************************** CLASSIFICATION

		if (kernel_type.eq.'polinomial') then

			do i = 1,nsn
				do j = 1,ns
					omega(j) = (dot_product(xnew(i,:),xin(j,:))+s1)**s2
				end do
				ynew(i,1) = sign(1.d0,dot_product(omega,alpha(:,1))+b(1))
			end do

		else if (kernel_type.eq.'RBF') then

			do i = 1,nsn
				do j = 1,ns
					tmp1 = dot_product(xnew(i,:),xnew(i,:))
					tmp2 = dot_product(xnew(i,:),xin(j,:))
					tmp3 = dot_product(xin(j,:),xin(j,:))
					omega(j) = dexp(-(tmp1-2.d0*tmp2+tmp3)/(s1*s1))
				end do
				ynew(i,1) = sign(1.d0,dot_product(omega,alpha(:,1))+b(1))
			end do

		else if (kernel_type.eq.'fourier') then

			do i = 1,nsn
				do j = 1,ns
					tmp1 = 1.d0
					do k = 1,ndx
						tmp2 = (1.d0-s1*s1)/(2.d0*(1.d0-2.d0*s1*dcos(xin(i,k)-xin(j,k))+s1*s1))
						tmp1 = tmp1*tmp2
					end do
					omega(j) = tmp1
				end do
				ynew(i,1) = sign(1.d0,dot_product(omega,alpha(:,1))+b(1))
			end do

		end if
	end if


	if (ga_use.EQV..FALSE.) then
		open(unit = 3, file = 'predict.dat', form = 'formatted', action = 'write', status = 'replace', iostat=io)


		do i = 1,nsn
			do j = 1,ndx
				write (unit = 3, fmt = '(E16.4,x)', advance = 'no') xnew(i,j)

			end do
			do j = 1,ndy
				write (unit = 3, fmt = '(E16.4,x)', advance = 'no') ynew(i,j)

			end do
			write (unit = 3, fmt = '(x)') 
		end do
		
		close(unit = 3)
	if (svm_type.eq.'regression') then
		open(unit = 4, file = 'error.dat', form = 'formatted', action = 'write', status = 'replace', iostat=io)
		
		do i = 1,nsn
			do j = 1,ndx
				write (unit = 4, fmt = '(E16.4,x)', advance = 'no') xnew(i,j)

			end do
			do j = 1,ndy
				write (unit = 4, fmt = '(E16.4,x)', advance = 'no') abs(sin(3.1416*xnew(i,j))/(3.1416*xnew(i,j))-ynew(i,j))

			end do
			write (unit = 4, fmt = '(x)') 
		end do
		
		close(unit = 4)
	end if
		deallocate(xnew,ynew,omega)
		
		print *,''
		print *,'Prediction completed.'
		print *,''
		
		call svm_end
	else
	
	
	end if
	
end subroutine predicting

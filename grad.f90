subroutine grad
	use globals
	use arrays
	
	real (kind=8)								::		rtmp1, rtmp2, rtmp3, s
	real (kind=8), dimension(:), allocatable	::		n, v, ones
	integer										::		i,j,k
	allocate(n(ns),v(ns),ones(ns),stat=merror)
	
	if (merror.NE.0) then
		print *,"Memory error. Training could be carried out"; call svm_end; end if	
	
	if (svm_type.eq.'regression') then
	! **************************************** REGRESSION
		if (kernel_type.eq.'polinomial') then
		
				do j = 1,ns
					do i = 1,j
						H(i,j) =  (dot_product(xin(i,:),xin(j,:)) + s1)**s2
						H(j,i) = H(i,j) 
					end do
				end do
		else if (kernel_type.eq.'RBF') then
		
				do j = 1,ns
					do i = 1,j
						rtmp1 = dot_product(xin(i,:),xin(i,:))
						rtmp2 = dot_product(xin(i,:),xin(j,:))
						rtmp3 = dot_product(xin(j,:),xin(j,:))
						H(i,j) = dexp(-(rtmp1-2.d0*rtmp2+rtmp3)/(s1*s1))
						H(j,i) = H(i,j)
					end do
				end do
		else if (kernel_type.eq.'fourier') then
		
				do j = 1,ns
					do i = i,j
						rtmp1 = 1.d0
						do k = 1,ndx
							rtmp2 = (1.d0-s1*s1)/(2.d0*(1.d0-2.d0*s1*dcos(xin(i,k)-xin(j,k))+s1*s1))
							rtmp1 = rtmp1*rtmp2
						end do
						H(i,j) = rtmp1
						H(j,i) = rtmp1
					end do
				end do
			end if
		
	else if (svm_type.eq.'classification') then! ************************************ CLASSIFICATION
		if (kernel_type.eq.'polinomial') then
		
				do j = 1,ns
					do i = 1,j
						H(i,j) =  yin(j,1)*yin(i,1)*(dot_product(xin(i,:),xin(j,:)) + s1)**s2
						H(j,i) = H(i,j) 
					end do
				end do
		else if (kernel_type.eq.'RBF') then
		
				do j = 1,ns
					do i = 1,j
						rtmp1 = dot_product(xin(i,:),xin(i,:))
						rtmp2 = dot_product(xin(i,:),xin(j,:))
						rtmp3 = dot_product(xin(j,:),xin(j,:))
						H(i,j) = yin(j,1)*yin(i,1)*dexp(-(rtmp1-2.d0*rtmp2+rtmp3)/(s1*s1))
						H(j,i) = H(i,j)
					end do
				end do
		else if (kernel_type.eq.'fourier') then
		
				do j = 1,ns
					do i = i,j
						rtmp1 = 1.d0
						do k = 1,ndx
							rtmp2 = (1.d0-s1*s1)/(2.d0*(1.d0-2.d0*s1*dcos(xin(i,k)-xin(j,k))+s1*s1))
							rtmp1 = rtmp1*rtmp2
						end do
						H(i,j) = yin(j,1)*yin(i,1)*rtmp1
						H(j,i) = H(i,j)
					end do
				end do
		end if
	end if

	do i = 1,ns
		H(i,i) = 1/gamma + H(i,i)
	end  do

    ones = 1.0

	 if (svm_type.eq.'regression') then	
		do j = 1,ndy
			n = conjgrad(H,ones)
			v = conjgrad(H,yin(1:ns,j))
			s = dot_product(ones,n)
			b(j) = dot_product(n,yin(1:ns,j))/s

			do i = 1,ns
				alpha(i,j) = v(i) - n(i)*b(j)
			end do
		end do
	else if (svm_type.eq.'classification') then
		n = conjgrad(H,yin(:,1))
		v = conjgrad(H,ones)
		s = dot_product(yin(:,1),n)
		b(1) = dot_product(n,ones)/s

		do i = 1,ns
			alpha(i,1) = v(i) - n(i)*b(1)
		end do
	end if
	
contains
function conjgrad(A,b)

	real(kind=8), dimension(:), intent(in)		::		b
    real(kind=8), dimension(size(b))			::		conjgrad
	real(kind=8), dimension(:), allocatable		::		x,p,r1,r2
	real(kind=8), dimension(:,:), intent(in)	::		A
	real(kind=8)								::		beta, lambda, tmp1, tmp2
	integer										::		i,k

	allocate(x(ns),p(ns),r1(ns),r2(ns),stat=merror)
	if (merror.NE.0) then
		print *,"Memory error. Conjugate gradient method could not be runned"; call svm_end; end if
	
	i = 0
	x = 0.d0
	r1 = b
	r2 = 0.d0
	p = 0.d0

	do while ((dot_product(r1,r1).GT.tol_cj).OR.(i.GT.10000))

		i = i + 1
		
		if (i==1) then
			p = b
		else
			beta = dot_product(r1,r1)/dot_product(r2,r2)
			do k = 1,ns
				p(k) = r1(k) + beta*p(k)
			end do
		end  if
	
		tmp1 = dot_product(r1,r1)

		tmp2 = dot_product(matmul(p,A),p)

		lambda = tmp1 / tmp2 !dot_product(r1,r1)/dot_product(matmul(p,A),p)
			
		do k = 1,ns
			x(k) = x(k) + lambda*p(k)
		end do

		r2 = r1
		r1 = r1 - lambda*matmul(A,p)
		
	end  do
		
	conjgrad = x

	deallocate(x,p,r1,r2)

end function conjgrad

end subroutine grad

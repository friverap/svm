subroutine check_parameters

  use arrays
  use globals

  implicit none

  if ((action.ne.'training').and. &
       (action.ne.'prediction')) then
     print *, '================================================================================'
     print *, 'The action you selected --->',action,'<--- is not available'
     print *, '================================================================================'
     stop
  end if

 if ((svm_type.ne.'classification').and. &
       (svm_type.ne.'regression')) then
     print *, '================================================================================'
     print *, 'The type of svm you selected --->',svm_type,'<--- is not available'
     print *, '================================================================================'
     stop
  end if

 if ((kernel_type.ne.'polinomial').and. &
       (kernel_type.ne.'RBF').and. &
       (kernel_type.ne.'fourier')) then
     print *, '================================================================================'
     print *, 'The kernel type you selected --->',kernel_type,'<--- is not available'
     print *, '================================================================================'
     stop
  end if

 if (gamma.le.0.0) then
   print *, '================================================================================'
   print *,'gamma cannot be equal or less than zero'
   print *, '================================================================================'
   stop
 end if

 	if ((kernel_type.eq.'polinomial').and.((s1.lt.0.0).OR.(s2.lt.0.d0).or.(s2-nint(s2).gt.1.d-5))) then
   print *, '================================================================================'
		print *,'first kernel parameter must be greater than zero, '
		print *,'and second kernel parameter must be an integer greater or equal'
		print *,'to zero'
   print *, '================================================================================'

	end if
	
	if ((kernel_type.eq.'RBF').and.(s1.le.0.d0)) then
   print *, '================================================================================'
		print *,'kernel parameter must be greater than zero'
   print *, '================================================================================'

	end if
	
	if ((kernel_type.eq.'fourier').and.((s1.le.0.d0).or.(s1.gt.1.0))) then
   print *, '================================================================================'
		print *,'kernel parameter must be greater than zero'
		print *,'and less than one'
   print *, '================================================================================'

	end if

end subroutine check_parameters

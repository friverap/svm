program main

use globals
use arrays
implicit none
	
	character(len=20) filestatus

	  Namelist /svm_Input/ s1, s2, &
	       gamma, tol_cj, &
	       kernel_type,svm_type, action   

	  open (3, file='input.par', status = 'old' )
	  read (3, nml = svm_Input)
	  close(3)
	  
	print *,''
	print *,'Least Squares Support Vector Machine' 
	print *,''
	print *,'Parameters:'
	print *,''
	print *,'   svm_type=', svm_type
	print *,'   action=', action
	print *,'   gamma=', gamma
	print *,'   kernel_type=', kernel_type	
	print *,'                   polinomial kernel K(x,x'') = (<x,x''> + s1)^s2' 
	print *,'                   RBF kernel, K(x,x'') = exp(-1/s1*||x-x''||^2)'
	print *,'                   fourier kernel, K(x,x'') = II (1-s1^2)/(2(1-2scos(xi-xj))+s^2)'
	print *,'                   '
	print *,'   s1=',s1,'          first kernel parameter'
	print *,'   s2=',s2,'          second kernel parameter'
	print *,'   tol_cj=',tol_cj,'    conjugate gradient tolerance'
	print *,''
	
	call check_parameters
	
	call read_data

	if (action.eq.'training') then
		call training 
	else if (action.eq.'prediction') then
		call predicting
	end if



end program main


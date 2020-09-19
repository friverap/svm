module globals

	implicit none

	!!!!!!!!!!!!!!!  SVM variables !!!!!!!!!

	character (len=20)							::		svm_type, action, kernel_type
	integer										::		ndx, ndy, ns, nsn, io, merror
	real(kind=8)								::		s1, s2, gamma, mean, variance, tol_cj
	logical										::	    ga_use
	
end module globals


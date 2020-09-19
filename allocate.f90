subroutine allocate

  use arrays
  use globals 

  implicit none

if (action.eq.'traininig') then
	allocate(H(ns,ns),stat=merror)
end if

allocate(xin(ns,ndx),stat=merror)

allocate(yin(ns,ndy),stat=merror)

allocate(b(ndy),stat=merror)

allocate(alpha(ns,ndy),stat=merror)

allocate(xnew(nsn,ndx),ynew(nsn,ndy),omega(ns),stat=merror)

end subroutine allocate

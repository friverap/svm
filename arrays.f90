 module arrays

  implicit none

  real(kind=8), allocatable, dimension (:) :: b, omega,xprime
  real(kind=8), allocatable, dimension (:) :: position

  real(kind=8), allocatable, dimension (:,:) :: xin, yin 
  real(kind=8), allocatable, dimension (:,:) :: xnew, ynew
  real(kind=8), allocatable, dimension (:,:) :: H,alpha

  end module arrays

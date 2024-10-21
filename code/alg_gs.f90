SUBROUTINE alg_gs(fname,fvec,x,n,check,gswght,MAXITS,TOLF)

use nrtype, only: dp

IMPLICIT NONE

INTEGER :: i,its,n,MAXITS
REAL(dp) :: TOLF,f,fold,gswght
REAL(dp), DIMENSION(n) :: x,fvec,xold,p,xn
REAL(dp), PARAMETER :: EPS=epsilon(x),TOLX=1.0e-8
LOGICAL :: check
integer,parameter::maxlns=100
real(dp),parameter::stpfac=10.0_dp
external fname

its=0
call fname(fvec,x,n)

! Check convergence
f=maxval(abs(fvec(:)))
print*,'Error in iteration # 1: ', f


! Test for initial guess being a root.
if (f < TOLF) then			
	check=.false.
	RETURN
endif
its=0
do while (its<MAXITS) 									! Start of iteration loop
	xold(:)=x(:)													! Store x, F, and f.
	fold=f
    
    p=-gswght*fvec
    call lnsrch_simple(xold,fold,p,x,n,f,fvec,stpfac,check,fname,maxlns)
    
    print*,'Error in iteration # ',its+1, ':', f

	if (f < TOLF) then							! Test for convergence on function values.
		check=.false.
		exit
	elseif ( maxval(abs(x(:)-xold(:))) < TOLX) then					! Test for convergence on dx: Here: absolute deviation 
		check=.false.
		exit			
	endif
	its = its+1
   
end do
check=.true.
print*,'MAXITS exceeded in ALG_GS'

!pause

contains

! ---------------------------------------------
SUBROUTINE lnsrch_simple(xold,fold,p,x,n,f,fvec,stpfac,check,fname,maxlns)

use nrtype, only: dp
IMPLICIT NONE
integer,intent(in):: n,maxlns
real(dp),intent(in)::stpfac
real(dp),intent(inout)::xold(n),fold,p(n),x(n),f,fvec(n)
logical,intent(inout)::check
real(dp)::alam
real(dp),parameter::alamin=0.01_dp
integer::its
external fname

check=.false.

alam=1.0_dp
do its=1,maxlns
	if (its>1) print*,'line search iteration # ',its-1

    x(:)=xold(:)+alam*p(:)
    call fname(fvec,x,n)
    f=maxval(abs(fvec(:)))
    ! print*, 'current alam,f,fold,maxlns:', alam,f,fold,maxlns
    
	if (alam < alamin) then										! Convergence on x
		check=.true.
		RETURN
	elseif (f < fold) then						! Sufficient function decrease.
		RETURN
    else														! Backtrack.
	    alam=alam/stpfac
    endif
end do 

END SUBROUTINE lnsrch_simple
! ---------------------------------------------

end subroutine alg_gs

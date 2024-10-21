SUBROUTINE alg_qn(fname,fvec,x,n,QTmat,Rmat,intlj,reevalj,check,rstit0,MaxLns,MAXITS,stpmax,TOLF,optpr)


use nrtype, only: dp
USE nrutil, ONLY: get_diag,lower_triangle, outerprod,put_diag,unit_matrix
use alexfuncs, ONLY: qrdcmp,qrupdt,rsolv
! use numerical_libraries
IMPLICIT NONE

INTEGER :: i,its,k,n,rstit,rstit0,MaxLns,MAXITS
REAL(dp) :: TOLF,TOLX,f,fold,stpmax,vabs,stpmx
REAL(dp), DIMENSION(n) :: x,fvec,c,d,fvcold,g,p,s,t,w,xold,diagJ
REAL(dp), PARAMETER :: EPS=epsilon(x)
REAL(dp), DIMENSION(n,n) :: Rmat,QTmat,rold,qtold
LOGICAL :: check,restrt,sing,intlj,reevalj,optpr
external fname

its=0
TOLX=TOLF			! tolerance on X should not be lower than tolalg
restrt=.true.
call fminbr(fname,f,fvec,x,n)
if (optpr) print*,'Error in iteration # 1:', maxval(abs(fvec(:)))
	
! Test for initial guess being a root.
if (maxval(abs(fvec(:))) < TOLF) then			
	check=.false.
	RETURN
endif

! vabs=sqrt(dot_product(x(:),x(:)))
! stpmax=STPMX*max(vabs,1.0*n)						! Calculate stpmax for line searches.

rold=Rmat
qtold=QTmat
rstit=rstit0
do its=1,MAXITS										! Start of iteration loop
	if (its==rstit) then
		rstit=rstit+rstit0
		restrt=.true.
	endif
	
	if (restrt) then
		
		if (intlj) then								! initialj=1 if initial Jacobi evaluation is required
			! initialize Jacobi
			call fdjacda(fname,x,fvec,Rmat)
			if (optpr) print*, 'done with Jacobi initialization' 
		elseif (reevalj) then
			intlj=.true.							! make sure to in next iter if still required
		else
			Rmat=rold								! make sure to always reinitialize with old Jacobi matrix
			QTmat=qtold
		endif

		call qrdcmp(Rmat,c,d,sing)										! QR decomposition of Jacobian.
		if (sing) then
			print*,'singular Jacobian in broyden'
			return
		endif
		call unit_matrix(QTmat)										! Form QT explicitly.
		do k=1,n-1
			if (c(k) /= 0.0) then
				QTmat(k:n,:)=QTmat(k:n,:)-outerprod(Rmat(k:n,k),& 
				matmul(Rmat(k:n,k),QTmat(k:n,:)))/c(k)
			endif
		end do
		where (lower_triangle(n,n)) Rmat(:,:)=0.0
		call put_diag(d(:),Rmat(:,:))									! Form R explicitly.

	else															! Carry out Broyden update.
		
		s(:)=x(:)-xold(:)		
		do i=1,n 
			t(i)=dot_product(Rmat(i,i:n),s(i:n))
		end do
		w(:)=fvec(:)-fvcold(:)-matmul(t(:),QTmat(:,:)) 
		
		! it is better to update in any case even though the following conditions do not hold
		! where (abs(w(:)) < EPS*(abs(fvec(:))+abs(fvcold(:)))) &
		! 	w(:)=0.0												! Don’t update with noisy components of w
		
		if (any(w(:) /= 0.0)) then
			t(:)=matmul(QTmat(:,:),w(:)) 
			s(:)=s(:)/dot_product(s,s)							
			call qrupdt(Rmat,QTmat,t,s)									! Update R and QT .
			d(:)=get_diag(Rmat(:,:))									! Diagonal of R stored in d.
			if (any(d(:) == 0.0)) &
				print*, 'Rmat singular in broyden'
		endif
	endif
	p(:)=-matmul(QTmat(:,:),fvec(:))									! r.h.s. for linear equations 
	do i=1,n														! Compute gradient for the line search
		g(i)=-dot_product(Rmat(1:i,i),p(1:i))
	end do
		
	xold(:)=x(:)													! Store x, F, and f.
	fvcold(:)=fvec(:)
	fold=f
	call rsolv(Rmat,d,p)												! Solve linear equations for step

    
	! lnsrch returns new x and f. It also calculates fvec at the new x when it calls fmin.
	call lnsrch(xold,fold,g,p,x,n,f,fvec,stpmax,check,fname,MaxLns,optpr)
    
    
    
	
	if (optpr) print*,'Error in iteration # ',its+1, ':', maxval(abs(fvec(:)))
	
	if (maxval(abs(fvec(:))) < TOLF) then							! Test for convergence on function values.
		check=.false.
		RETURN
	endif
	if (not(check)) then											! True if line search failed to find a new x
		restrt=.false.
		if ( maxval(abs(x(:)-xold(:))) < TOLX) then					! Test for convergence on dx: Here: absolute deviation 
			! better not do anything here!
			! restrt=.true.
		endif
	else
		if (not(restrt)) then
			restrt=.true.
		! ELSE: keep your fingers crossed and continue
		endif
	endif
end do
check=.true.
print*,'MAXITS exceeded in ALG_QN'

contains

! ---------------------------------------------
SUBROUTINE lnsrch(xold,fold,g,p,x,n,f,fvec,stpmax,check,fname,MaxLns,optpr)

! Given an N-dimensional point xold, the value of the function and gradient there, fold
! and g, and a direction p, finds a new point x along the direction p from xold where the
! function fname has decreased ¡°sufficiently.¡± xold, g, p, and x are all arrays of length N.
! The new function value is returned in f. stpmax is an input quantity that limits the length
! of the steps so that you do not try to evaluate the function in regions where it is undefined
! or subject to overflow. p is usually the Newton direction. The output quantity check is
! false on a normal exit. It is true when x is too close to xold. In a minimization algorithm,
! this usually signals convergence and can be ignored. However, in a zero-finding algorithm
! the calling program should check whether the convergence is spurious.
! Parameters: ALF ensures sufficient decrease in function value; TOLX is the convergence
! criterion on .x.

use nrtype, only: dp
IMPLICIT NONE
integer:: n,ndum,MaxLns,its,i
REAL(dp), DIMENSION(n) :: xold,g,p,fvec,x
REAL(dp) :: fold,stpmax,f,vabs
LOGICAL :: check,optpr
REAL(dp), PARAMETER :: ALF=1.0e-4,TOLX=epsilon(x)
REAL(dp) :: a,alam,alam2,alamin,b,disc,f2,pabs,rhs1,rhs2,slope,tmplam
external fname

check=.false.

!pabs=sqrt(dot_product(p(:),p(:)))
!if (pabs > stpmax) then
!	p(:)=p(:)*stpmax/pabs						! Scale if attempted step is too big.
!endif
! alternative scaling if step-size is too big
do i=1,n
	pabs = abs(p(i))
	if ( pabs > stpmax ) then
		p(:)=p(:)*stpmax/pabs
	endif
end do


slope=dot_product(g,p)
! if (slope >= 0.0) print*, 'roundoff problem in lnsrch'

alamin=TOLX/maxval(abs(p(:))/max(abs(xold(:)),1.0))			! Compute alammin
alam=1.0														! Always try full Newton step first.
do its=1,MaxLns
	x(:)=xold(:)+alam*p(:)
	call fminbr(fname,f,fvec,x,n)
	
	if (alam < alamin) then										! Convergence on x
		x(:)=xold(:)
		check=.true.
		RETURN
	elseif (f <= fold+ALF*alam*slope) then						! Sufficient function decrease.
		RETURN
	else														! Backtrack.
		if (alam == 1.0) then									! First time.
			tmplam=-slope/(2.0*(f-fold-slope))
		else
			if (optpr) print*, 'line search iteration #:', its-1
															! Subsequent backtracks.
			rhs1=f-fold-alam*slope
			rhs2=f2-fold-alam2*slope
			a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
			b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
			if (a == 0.0) then
				tmplam=-slope/(2.0*b)
			else
				disc=b*b-3.0*a*slope
				if (disc < 0.0) then
					tmplam=0.5*alam
				elseif (b <= 0.0) then
					tmplam=(-b+sqrt(disc))/(3.0*a)
				else
					tmplam=-slope/(b+sqrt(disc))
				end if
			end if
			if (tmplam > 0.5*alam) tmplam=0.5*alam 
		end if
	end if
	alam2=alam
	f2=f
	alam=max(tmplam,0.1*alam) 
end do 

END SUBROUTINE lnsrch
! ---------------------------------------------


! ---------------------------------------------
subroutine fminbr(fname,f,fvec,x,n)

use nrtype, only: dp
implicit none

integer::n
real(dp)::x(n),fvec(n),f
external fname

call fname(fvec,x,n)
f=0.5*dot_product(fvec,fvec)

end subroutine fminbr
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE fdjacda(fname,x,fvec,df)

! Computes forward-difference approximation to Jacobian. On input, x is the point at which
! the Jacobian is to be evaluated, and fvec is the vector of function values at the point,
! both arrays of length N. df is the N × N output Jacobian. FUNCTION funcv(x) is a
! fixed-name, user-supplied routine that returns the vector of functions at x.
! Parameter: EPS is the approximate square root of the machine precision.

IMPLICIT NONE
REAL(dp), DIMENSION(:), INTENT(IN) :: fvec
REAL(dp), DIMENSION(:), INTENT(INOUT) :: x
REAL(dp), DIMENSION(:,:), INTENT(OUT) :: df
REAL(dp), DIMENSION(size(x)) :: funcv
REAL(dp), PARAMETER :: EPS=1.0e-4 ! EPS=1.0e-3
INTEGER :: j,n
REAL(dp), DIMENSION(size(x)) :: xsav,xph,h
EXTERNAL fname

n=size(x)
xsav=x
h=EPS*abs(xsav)
where (h == 0.0) h=EPS
xph=xsav+h								! Trick to reduce finite precision error.
h=xph-xsav
do j=1,n
	x(j)=xph(j)
	call fname(funcv,x,n)
	df(:,j)=(funcv-fvec(:))/h(j)			! Forward difference formula.
	x(j)=xsav(j)
end do

END SUBROUTINE fdjacda
! ---------------------------------------------


end subroutine alg_qn
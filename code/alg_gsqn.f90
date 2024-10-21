SUBROUTINE alg_gsqn(fname,fvec,x,n,QTmat,Rmat,check,rstit0,MaxLns,MAXITS,updt,stpmax,TOLF,tt)

use nrtype, only: dp
USE nrutil, ONLY: get_diag
!use linrg_int

IMPLICIT NONE

INTEGER :: i,j,its,k,l,n,rstit,nsmall,MaxLns,MaxIts,tt,rstit0
REAL(dp) :: TOLF,TOLX,f,fold,stpmax,vabs
REAL(dp), DIMENSION(n) :: x,fvec,fvcold,xold,p,g
real(dp),dimension(tt,n/tt)::p0,g0,f0
real(dp),dimension(n/tt,1)::s,w
real(dp),dimension(1,n/tt)::sr
REAL(dp), PARAMETER :: EPS=epsilon(x)
REAL(dp), DIMENSION(n/tt,n/tt) :: Rmat,QTmat,B,Bold,invB,transB
real(dp),dimension(n/tt)::diaginvB
real(dp)::srs
integer::nvec(n/tt)
LOGICAL :: check,restrt,sing,updt
external fname

its=0
TOLX=TOLF			! tolerance on X should not be lower than tolalg
restrt=.true.
call fminbr(fname,f,fvec,x,n)
print*,'Error in iteration # 1:', maxval(abs(fvec(:)))
	
! Test for initial guess being a root.
if (maxval(abs(fvec(:))) < TOLF) then			
	check=.false.
	RETURN
endif

!vabs=sqrt(dot_product(x(:),x(:)))
!stpmax=STPMX*max(vabs,1.0*n)						! Calculate stpmax for line searches.


B = Rmat
nsmall=n/tt

call dlinrg(nsmall, B, nsmall, invB, nsmall)	! correct B in case diag elements are larger than one
diaginvB=get_diag(invB)
if (maxval(diaginvB)>1.0) then
	do while (maxval(diaginvB)>1.0)
		invB=invB/maxval(diaginvB)
		diaginvB=get_diag(invB)
	end do
	call dlinrg(nsmall, invB, nsmall, B, nsmall)
endif

Bold = B		! form Bold for updates

k=int(tt/2)			! build index vector
do i=1,nsmall
	nvec(i)=k
	k=k+tt
end do

rstit=rstit0
do its=1,MAXITS


										! Start of iteration loop
	if (its==rstit) then
		rstit=rstit+rstit0
		restrt=.true.
	endif
	
	if (updt) then
		if (restrt) then
			B=Bold
		else			
			s(:,1)=x(nvec)-xold(nvec)
			w(:,1)=fvec(nvec)-fvcold(nvec)-matmul(B(:,:),s(:,1)) 
			
			! it is better to update in any case even though the following conditions do not hold
			!where (abs(w(:,1)) < EPS*(abs(fvec(nvec))+abs(fvcold(nvec)))) &
			!	w(:,1)=0.0												! Don�t update with noisy components of w
			
			if (any(w(:,1) /= 0.0)) then
				sr=reshape(s(:,1),(/1,nsmall/))
				B = B + matmul(w,sr)/dot_product(s(:,1),s(:,1))
			endif
		endif
	endif
	
	! build invB, transB, p and g
	call dlinrg(nsmall, B, nsmall, invB, nsmall)
	transB = transpose(B)
	f0=reshape(fvec,(/tt,nsmall/))
	p0=0.0
	g0=0.0
	do i=1,nsmall
		do j=1,nsmall
			p0(:,i)=p0(:,i)-invB(i,j)*f0(:,j);
            g0(:,i)=g0(:,i)-transB(i,j)*f0(:,j);
        end do
	end do
	p=reshape(p0,(/n/))
	g=reshape(g0,(/n/))
	
	xold(:)=x(:)													! Store x, F, and f.
	fvcold(:)=fvec(:)
	fold=f

	! lnsrch returns new x and f. It also calculates fvec at the new x when it calls fmin.
	call lnsrch(xold,fold,g,p,x,n,f,fvec,stpmax,check,fname,MaxLns)
	print*,'Error in iteration # ',its+1, ':', maxval(abs(fvec(:)))

	if (maxval(abs(fvec(:))) < TOLF) then							! Test for convergence on function values.
		check=.false.
		RETURN
	endif
	if (not(check)) then											! True if line search failed to find a new x
		restrt=.false.
		if ( maxval(abs(x(:)-xold(:))) < TOLX) then					! Test for convergence on dx: Here: absolute deviation 
			! better not do anything here!
		endif
	else
		if (not(restrt)) then
			restrt=.true.
		! ELSE: keep your fingers crossed and continue
		endif
	endif

end do	! end for its



check=.true.
print*,'MAXITS exceeded in ALG_GSQN'

contains

! ---------------------------------------------
SUBROUTINE lnsrch(xold,fold,g,p,x,n,f,fvec,stpmax,check,fname,MaxLns)

use nrtype, only: dp
IMPLICIT NONE
integer:: n,ndum,MaxLns,its
REAL(dp), DIMENSION(n) :: xold,g,p,fvec,x
REAL(dp) :: fold,stpmax,f,vabs
LOGICAL :: check
REAL(dp), PARAMETER :: ALF=1.0e-4,TOLX=epsilon(x)
REAL(dp) :: a,alam,alam2,alamin,b,disc,f2,pabs,rhs1,rhs2,slope,tmplam
external fname

check=.false.
pabs=sqrt(dot_product(p(:),p(:)))

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
		else													! Subsequent backtracks.
			print*,'line search iteration # ',its-1
			
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

end subroutine alg_gsqn

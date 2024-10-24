module nrutils

! these are a number of functions copied from the numerical recipes book

use nrtype

implicit none

contains

! ---------------------------------------------
FUNCTION indmax(x,maxx)

! Returns index of maximum in vector x

real(dp), dimension(:), INTENT(IN) :: x
real(dp),intent(in)::maxx
integer::indmax
INTEGER:: i,n

n=size(x,1)
do i=1,n
	if (x(i)==maxx) then
		indmax=i
		return
	end if
end do
indmax=i

END FUNCTION indmax
! ---------------------------------------------

! ---------------------------------------------b
FUNCTION zbrent(func,x1,x2,tol)
USE nrtype; USE nrutil, ONLY : nrerror
IMPLICIT NONE
REAL(DP), INTENT(IN) :: x1,x2,tol
REAL(DP) :: zbrent
INTERFACE
	FUNCTION func(x)
	USE nrtype
	IMPLICIT NONE
	REAL(DP), INTENT(IN) :: x
	REAL(DP) :: func
	END FUNCTION func
END INTERFACE
INTEGER(I4B), PARAMETER :: ITMAX=100
REAL(DP), PARAMETER :: EPS=epsilon(x1)
INTEGER(I4B) :: iter
REAL(DP) :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
a=x1
b=x2
fa=func(a)
fb=func(b)
if ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) then
	! call nrerror('root must be bracketed for zbrent')
    print*, 'root must be bracketed for zbrent'
    print*, 'keep your fingers crossed and continue...'
    zbrent=b
	RETURN
endif
c=b
fc=fb
do iter=1,ITMAX
	if ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) then
		c=a
		fc=fa
		d=b-a
		e=d
	end if
	if (abs(fc) < abs(fb)) then
		a=b
		b=c
		c=a
		fa=fb
		fb=fc
		fc=fa
    end if
    tol1=2.0_sp*EPS*abs(b)+0.5_sp*tol
	xm=0.5_sp*(c-b)
	if (abs(xm) <= tol1 .or. fb == 0.0) then
		zbrent=b
		RETURN
	end if
	if (abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
		s=fb/fa
		if (a == c) then
			p=2.0_sp*xm*s
			q=1.0_sp-s
		else
			q=fa/fc
			r=fb/fc
			p=s*(2.0_sp*xm*q*(q-r)-(b-a)*(r-1.0_sp))
			q=(q-1.0_sp)*(r-1.0_sp)*(s-1.0_sp)
		end if
		if (p > 0.0) q=-q
		p=abs(p)
		if (2.0_sp*p  <  min(3.0_sp*xm*q-abs(tol1*q),abs(e*q))) then
			e=d
			d=p/q
		else
			d=xm
			e=d
		end if
	else
		d=xm
		e=d
	end if
	a=b
	fa=fb
	b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
	fb=func(b)
end do
call nrerror('zbrent: exceeded maximum iterations')
zbrent=b
END FUNCTION zbrent
! ---------------------------------------------    

end module nrutils
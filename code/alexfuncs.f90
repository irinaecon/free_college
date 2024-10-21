module alexfuncs

! these are a number of functions copied from the numerical recipes book

use nrtype, ONLY: dp
implicit none

contains


! ---------------------------------------------
SUBROUTINE fdjac(fname,x,fvec,df)

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
REAL(dp), PARAMETER :: EPS=1.0e-3
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

END SUBROUTINE fdjac
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE qrdcmp(a,c,d,sing)

! Constructs the QR decomposition of the n ×n matrix a. The upper triangular matrix R is
! returned in the upper triangle of a, except for the diagonal elements of R, which are returned
! in the n-dimensional vector d. The orthogonal matrix Q is represented as a product of n-1
! Householder matrices Q1 . . .Qn-1, where Qj = 1 - uj . uj/cj. The ith component of uj
! is zero for i = 1, . . . , j - 1 while the nonzero components are returned in a(i,j) for
! i = j, . . . ,n. sing returns as true if singularity is encountered during the decomposition,
! but the decomposition is still completed in this case.

USE nrutil, ONLY: outerprod
IMPLICIT NONE
REAL(dp), DIMENSION(:,:), INTENT(INOUT) :: a
REAL(dp), DIMENSION(:), INTENT(OUT) :: c,d
real(dp)::vabs
LOGICAL, INTENT(OUT) :: sing
INTEGER:: k,n
REAL(dp) :: scale,sigma

sing=.false.
n=size(a,1)
do k=1,n-1
	scale=maxval(abs(a(k:n,k)))
	if (scale == 0.0) then				! Singular case.
		sing=.true.
		c(k)=0.0
		d(k)=0.0
	else								! Form Qk and Qk · A.
		a(k:n,k)=a(k:n,k)/scale
		vabs=sqrt(dot_product(a(k:n,k),a(k:n,k)))
		sigma=sign(vabs,a(k,k))
		a(k,k)=a(k,k)+sigma
		c(k)=sigma*a(k,k)
		d(k)=-scale*sigma
		a(k:n,k+1:n)=a(k:n,k+1:n)-outerprod(a(k:n,k),matmul(a(k:n,k),a(k:n,k+1:n)))/c(k)
	end if
end do
d(n)=a(n,n)
if (d(n) == 0.0) sing=.true.

END SUBROUTINE qrdcmp
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE qrupdt(r,qt,u,v)

! Given the QR decomposition of some nxn matrix, calculates the QR decomposition of
! the matrix Q (R + kron(u,v)). Here r and qt are nxn matrices, u and v are n-dimensional
! vectors. Note that QT is input and returned in qt.

USE nrutil, ONLY : ifirstloc

IMPLICIT NONE
REAL(dp), DIMENSION(:,:), INTENT(INOUT) :: r,qt
REAL(dp), DIMENSION(:), INTENT(INOUT) :: u
REAL(dp), DIMENSION(:), INTENT(IN) :: v
INTEGER:: i,k,n

n=size(r,1)
k=n+1-ifirstloc(u(n:1:-1) /= 0.0)		! Find largest k such that u(k) .= 0.
if (k < 1) k=1

do i=k-1,1,-1						! Transform R + kron(u,v) to upper Hessenberg.
	call rotate(r,qt,i,u(i),-u(i+1))
	u(i)=pythag(u(i),u(i+1))
end do
r(1,:)=r(1,:)+u(1)*v
do i=1,k-1							! Transform upper Hessenberg matrix to upper triangular. 
	call rotate(r,qt,i,r(i,i),-r(i+1,i))
end do

END SUBROUTINE qrupdt
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE rotate(r,qt,i,a,b)

! Given nxn matrices r and qt, carry out a Jacobi rotation on rows i and i+1 of each matrix.
! a and b are the parameters of the rotation

IMPLICIT NONE
REAL(dp), DIMENSION(:,:), TARGET, INTENT(INOUT) :: r,qt
INTEGER, INTENT(IN) :: i
REAL(dp), INTENT(IN) :: a,b
REAL(dp), DIMENSION(size(r,1)) :: temp
INTEGER:: n
REAL(dp) :: c,fact,s

n=size(r,1)
if (a == 0.0) then							! Avoid unnecessary overflow or underflow.
	c=0.0
	s=sign(1.0,b)
elseif (abs(a) > abs(b)) then
	fact=b/a
	c=sign(1.0/sqrt(1.0+fact**2),a)
	s=fact*c
else
	fact=a/b
	s=sign(1.0/sqrt(1.0+fact**2),b)
	c=fact*s
endif
temp(i:n)=r(i,i:n)							! Premultiply r by Jacobi rotation.
r(i,i:n)=c*temp(i:n)-s*r(i+1,i:n)
r(i+1,i:n)=s*temp(i:n)+c*r(i+1,i:n)
temp=qt(i,:)								! Premultiply qt by Jacobi rotation.
qt(i,:)=c*temp-s*qt(i+1,:)
qt(i+1,:)=s*temp+c*qt(i+1,:)

END SUBROUTINE rotate
! ---------------------------------------------


! ---------------------------------------------
FUNCTION pythag(a,b)

IMPLICIT NONE
REAL(dp), INTENT(IN) :: a,b
REAL(dp) :: pythag
REAL(dp) :: absa,absb

absa=abs(a)
absb=abs(b)
if (absa > absb) then
	pythag=absa*sqrt(1.0+(absb/absa)**2)
else
	if (absb == 0.0) then
		pythag=0.0
	else
		pythag=absb*sqrt(1.0+(absa/absb)**2)
	endif
endif

END FUNCTION pythag
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE rsolv(a,d,b)

! Solves the set of n linear equations R · x = b, where R is an upper triangular matrix stored
! in a and d. The n×n matrix a and the vector d of length n are input as the output of the
! routine qrdcmp and are not modified. b is input as the right-hand-side vector of length n,
! and is overwritten with the solution vector on output.

IMPLICIT NONE
REAL(dp), DIMENSION(:,:), INTENT(IN) :: a
REAL(dp), DIMENSION(:), INTENT(IN) :: d
REAL(dp), DIMENSION(:), INTENT(INOUT) :: b
INTEGER:: i,n

n=size(a,1)
b(n)=b(n)/d(n)
do i=n-1,1,-1
	b(i)=(b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/d(i)
end do

END SUBROUTINE rsolv
! ---------------------------------------------



end module alexfuncs
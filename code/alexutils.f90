module alexutils

! these are a number of functions copied from the numerical recipes book

use nrtype

implicit none

contains


! ---------------------------------------------
function get_diag(mat)
! input: matrix of arbitrary size
! output: diagonal elements of matrix

real(dp),dimension(:,:),INTENT(IN) :: mat
real(dp), dimension(min(size(mat,1),size(mat,2))) :: get_diag
integer j

do j=1,min(size(mat,1),size(mat,2))
	get_diag(j)=mat(j,j)
end do

end function
! ---------------------------------------------


! ---------------------------------------------
FUNCTION lower_triangle(j,k ,extra)
! (returns a lower triangular mask)

INTEGER, INTENT(IN) :: j,k
INTEGER, OPTIONAL, INTENT(IN) :: extra
LOGICAL, DIMENSION(j,k) :: lower_triangle

INTEGER :: n,jj,kk
n=0
if (present(extra)) n=extra
do jj=1,j
	do kk=1,k
		lower_triangle(jj,kk)= (kk-jj < n)
	end do
end do

END FUNCTION lower_triangle
! ---------------------------------------------


! ---------------------------------------------
FUNCTION outerprod(a,b)
! takes outer product of two vectors a and b
real(dp), DIMENSION(:), INTENT(IN) :: a,b
real(dp), DIMENSION(size(a),size(b)) :: outerprod

outerprod = spread(a,dim=2,ncopies=size(b)) * &
	spread(b,dim=1,ncopies=size(a))

END FUNCTION outerprod
! ---------------------------------------------


! ---------------------------------------------
FUNCTION ifirstloc(mask)

! Returns the index (subscript value) of the first location, in a one-dimensional
! logical mask, that has the value .TRUE., or returns size(mask)+1 if all
! components of mask are .FALSE.
! Note that while the reference implementation uses a do-loop, the function is
! parallelized in nrutil by instead using the merge and maxloc intrinsics.
! Reference implementation:

logical, dimension(:), INTENT(IN) :: mask
INTEGER:: ifirstloc
INTEGER:: i

do i=1,size(mask)
	if (mask(i)) then
		ifirstloc=i
		return
	end if
end do
ifirstloc=i

END FUNCTION ifirstloc
! ---------------------------------------------


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


! ---------------------------------------------
SUBROUTINE put_diag(diag,mat)

! Sets the diagonal of matrix mat equal to the argument diag, either a scalar
! or else a vector whose size must be the smaller of the two dimensions of
! matrix mat. The following shows an implementation where diag is a vector;
! the scalar case can be overloaded (see Appendix C1).
! Reference implementation:

real(dp), DIMENSION(:), INTENT(IN) :: diag
real(dp), DIMENSION(:,:), INTENT(INOUT) :: mat

INTEGER:: j,n,m

n=size(diag)
m=min(size(mat,1),size(mat,2))

if (n==m) then
	do j=1,n
		mat(j,j)=diag(j)
	end do
else	
	print*, 'error in put_diag: n not equal to m'
	return
endif

END SUBROUTINE put_diag
! ---------------------------------------------



! ---------------------------------------------
SUBROUTINE unit_matrix(mat)
real(dp), DIMENSION(:,:), INTENT(OUT) :: mat

! Sets the diagonal components of mat to unity, all other components to zero.
! When mat is square, this will be the unit matrix; otherwise, a unit matrix
! with appended rows or columns of zeros.

INTEGER:: i,n
n=min(size(mat,1),size(mat,2))

mat(:,:)=0.0
do i=1,n
	mat(i,i)=1.0
end do

END SUBROUTINE unit_matrix
! ---------------------------------------------


! ---------------------------------------------
function kronprod(A,B)

! Takes the Kronecker product of the two input matrices mat1 and mat2

real(dp), DIMENSION(:,:), INTENT(IN) :: A,B
real(dp), DIMENSION(size(A,1)*size(B,1),size(A,2)*size(B,2)) :: kronprod
INTEGER:: n,m,p,q,mp,nq,i,j,k,l

m=size(A,1)
n=size(A,2)
p=size(B,1)
q=size(B,2)


kronprod(:,:)=0.0
do i=1,m						! go down rows of A
	k=(i-1)*p+1					! fill in rows of B
	do j=1,n					! go down columns of A
		l=(j-1)*q+1				! fill in columns of B
		kronprod(k:k+p-1,l:l+q-1)=A(i,j)*B
	end do
end do

end function kronprod
! ---------------------------------------------


! ---------------------------------------------
function linmake(nx)

integer,intent(in)::nx
integer::linmake(nx)
integer::xc

do xc=1,nx
    linmake(xc)=xc
end do

end function linmake
! ---------------------------------------------


!SUBROUTINE spear(data1,data2,d,zd,probd,rs,probrs)
!USE nrtype; 
!USE nrutil, ONLY : assert_eq
!USE nr, ONLY :betai, erfcc !,sort2
!IMPLICIT NONE
!REAL(DP), DIMENSION(:), INTENT(IN) :: data1,data2
!REAL(DP), INTENT(OUT) :: d,zd,probd,rs,probrs
!INTEGER(I4B) :: n
!REAL(DP) :: aved,df,en,en3n,fac,sf,sg,t,vard
!REAL(DP), DIMENSION(size(data1)) :: wksp1,wksp2
!n=assert_eq(size(data1),size(data2),'spear')
!wksp1(:)=data1(:)
!wksp2(:)=data2(:)
!call sort2(wksp1,wksp2)
!call crank(wksp1,sf)
!call sort2(wksp2,wksp1)
!call crank(wksp2,sg)
!wksp1(:)=wksp1(:)-wksp2(:)
!d=dot_product(wksp1,wksp1)
!en=n
!en3n=en**3-en
!aved=en3n/6.0_dp-(sf+sg)/12.0_dp
!fac=(1.0_dp-sf/en3n)*(1.0_dp-sg/en3n)
!vard=((en-1.0_dp)*en**2*(en+1.0_dp)**2/36.0_dp)*fac
!zd=(d-aved)/sqrt(vard)
!probd=erfcc(abs(zd)/SQRT2)
!rs=(1.0_dp-(6.0_dp/en3n)*(d+(sf+sg)/12.0_dp))/sqrt(fac)
!fac=(1.0_dp+rs)*(1.0_dp-rs)
!if (fac > 0.0) then
!	t=rs*sqrt((en-2.0_dp)/fac)
!	df=en-2.0_dp
!	probrs=betai(0.5_dp*df,0.5_dp,df/(df+t**2))
!else
!	probrs=0.0
!end if
!
!CONTAINS
!!BL
!	SUBROUTINE crank(w,s)
!	USE nrtype; USE nrutil, ONLY : arth,array_copy
!	IMPLICIT NONE
!	REAL(DP), INTENT(OUT) :: s
!	REAL(DP), DIMENSION(:), INTENT(INOUT) :: w
!	INTEGER(I4B) :: i,n,ndum,nties
!	INTEGER(I4B), DIMENSION(size(w)) :: tstart,tend,tie,idx
!	n=size(w)
!	idx(:)=arth(1,1,n)
!	tie(:)=merge(1,0,w == eoshift(w,-1))
!	tie(1)=0
!	w(:)=idx(:)
!	if (all(tie == 0)) then
!		s=0.0
!		RETURN
!	end if
!	call array_copy(pack(idx(:),tie(:)<eoshift(tie(:),1)),tstart,nties,ndum)
!	tend(1:nties)=pack(idx(:),tie(:)>eoshift(tie(:),1))
!	do i=1,nties
!		w(tstart(i):tend(i))=(tstart(i)+tend(i))/2.0_dp
!	end do
!	tend(1:nties)=tend(1:nties)-tstart(1:nties)+1
!	s=sum(tend(1:nties)**3-tend(1:nties))
!    END SUBROUTINE crank
!    
!     SUBROUTINE sort2(arr,slave)
!    USE nrtype; USE nrutil, ONLY : assert_eq
!    USE nr, ONLY : indexx
!    IMPLICIT NONE
!    REAL(DP), DIMENSION(:), INTENT(INOUT) :: arr,slave
!    INTEGER(I4B) :: ndum
!    INTEGER(I4B), DIMENSION(size(arr)) :: index
!    ndum=assert_eq(size(arr),size(slave),'sort2')
!    call indexx(arr,index)
!    arr=arr(index)
!    slave=slave(index)
!    END SUBROUTINE sort2
!
!    
!   
!    
!    
!END SUBROUTINE spear



        
    
  

end module alexutils
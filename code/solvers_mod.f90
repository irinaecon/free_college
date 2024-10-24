module solvers_mod
    
    use nrtype
    use params_mod
    
    implicit none
    
    contains
    
! --------------------------------------------------------------------
FUNCTION golden(ax,bx,cx,func,tol,xmin)
USE nrtype
IMPLICIT NONE
REAL(dP), INTENT(IN) :: ax,bx,cx,tol
REAL(dP), INTENT(OUT) :: xmin
REAL(dP) :: golden

INTERFACE
	FUNCTION func(x)
    use nrtype
	IMPLICIT NONE
	REAL(dP), INTENT(IN) :: x
	REAL(dP) :: func
	END FUNCTION func
END INTERFACE

REAL(dP), PARAMETER :: R=0.61803399_sp,C=1.0_sp-R
REAL(dP) :: f1,f2,x0,x1,x2,x3
x0=ax
x3=cx
if (abs(cx-bx) > abs(bx-ax)) then
	x1=bx
	x2=bx+C*(cx-bx)
else
	x2=bx
	x1=bx-C*(bx-ax)
end if
   
f1=func(x1)
f2=func(x2)
do
	if (abs(x3-x0) <= tol*(abs(x1)+abs(x2))) exit
	if (f2 < f1) then
		call shft3(x0,x1,x2,R*x2+C*x3)
		call shft2(f1,f2,func(x2))
	else
		call shft3(x3,x2,x1,R*x1+C*x0)
		call shft2(f2,f1,func(x1))
	end if
end do
if (f1 < f2) then
	golden=f1
	xmin=x1
else
	golden=f2
	xmin=x2
end if
!BL


END FUNCTION golden
! -------------------------------------------------------------------- 


SUBROUTINE shft3(a,b,c,d)
REAL(dP), INTENT(OUT) :: a
REAL(dP), INTENT(INOUT) :: b,c
REAL(dP), INTENT(IN) :: d
a=b
b=c
c=d
END SUBROUTINE shft3

SUBROUTINE shft2(a,b,c)
REAL(dP), INTENT(OUT) :: a
REAL(dP), INTENT(INOUT) :: b
REAL(dP), INTENT(IN) :: c
a=b
b=c
END SUBROUTINE shft2


! ---------------------------------------------------------------------
FUNCTION brent(ax,bx,cx,func,tol,xmin,fflag)
	USE nrtype; USE nrutil, ONLY : nrerror
	IMPLICIT NONE
	REAL(DP), INTENT(IN) :: ax,bx,cx,tol
	REAL(DP), INTENT(OUT) :: xmin
	REAL(DP) :: brent
    logical,intent(out):: fflag
	INTERFACE
		FUNCTION func(x)
		USE nrtype
		IMPLICIT NONE
		REAL(DP), INTENT(IN) :: x
		REAL(DP) :: func
		END FUNCTION func
	END INTERFACE
	INTEGER(I4B), PARAMETER :: ITMAX=100        ! only use 100 iters (what is enough is enough...)
	REAL(DP), PARAMETER :: CGOLD=0.3819660_dp,ZEPS=1.0e-3_dp*epsilon(ax)
	INTEGER(I4B) :: iter
	REAL(DP) :: a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
	
    fflag = .false.
    
    a=min(ax,cx)
	b=max(ax,cx)
	v=bx
	w=v
	x=v
	e=0.0
	fx=func(x)
	!print*, 'initial estimate, function value: ', x, fx
    fv=fx
	fw=fx
	do iter=1,ITMAX
		xm=0.5_sp*(a+b)
		tol1=tol*abs(x)+ZEPS
		tol2=2.0_sp*tol1
		if (abs(x-xm) <= (tol2-0.5_sp*(b-a))) then
			xmin=x
			brent=fx
			RETURN
		end if
		if (abs(e) > tol1) then
			r=(x-w)*(fx-fv)
			q=(x-v)*(fx-fw)
			p=(x-v)*q-(x-w)*r
			q=2.0_sp*(q-r)
			if (q > 0.0) p=-p
			q=abs(q)
			etemp=e
			e=d
			if (abs(p) >= abs(0.5_sp*q*etemp) .or. &
				p <= q*(a-x) .or. p >= q*(b-x)) then
				e=merge(a-x,b-x, x >= xm )
				d=CGOLD*e
			else
				d=p/q
				u=x+d
				if (u-a < tol2 .or. b-u < tol2) d=sign(tol1,xm-x)
			end if
		else
			e=merge(a-x,b-x, x >= xm )
			d=CGOLD*e
		end if
		u=merge(x+d,x+sign(tol1,d), abs(d) >= tol1 )
		fu=func(u)
        !print*, 'estimate, function value: ', u, fu
		! store current value as xmin, otherwise no output if # of iterations is exceeded:
        xmin = u
        if (fu <= fx) then
			if (u >= x) then
				a=x
			else
				b=x
			end if
			call shft(v,w,x,u)
			call shft(fv,fw,fx,fu)
		else
			if (u < x) then
				a=u
			else
				b=u
			end if
			if (fu <= fw .or. w == x) then
				v=w
				fv=fw
				w=u
				fw=fu
			else if (fu <= fv .or. v == x .or. v == w) then
				v=u
				fv=fu
			end if
		end if
    end do
    if (opt_warn) print*, "brent didn't converge?",fv
    brent = fv
    fflag=.true.
	!call nrerror('brent: exceed maximum iterations')
	if (opt_warn) print*, 'warning in brent: exceed maximum iterations'
    
	CONTAINS
!BL
	SUBROUTINE shft(a,b,c,d)
	REAL(DP), INTENT(OUT) :: a
	REAL(DP), INTENT(INOUT) :: b,c
	REAL(DP), INTENT(IN) :: d
	a=b
	b=c
	c=d
	END SUBROUTINE shft
    END FUNCTION brent
! ---------------------------------------------------------------------


!! --------------------------------------------------------------------
!FUNCTION brent(ax,bx,cx,func,tol,xmin)
!USE nrtype; USE nrutil, ONLY : nrerror
!IMPLICIT NONE
!REAL(DP), INTENT(IN) :: ax,bx,cx,tol
!REAL(DP), INTENT(OUT) :: xmin
!REAL(DP) :: brent
!INTERFACE
!	FUNCTION func(x)
!	USE nrtype
!	IMPLICIT NONE
!	REAL(DP), INTENT(IN) :: x
!	REAL(DP) :: func
!	END FUNCTION func
!END INTERFACE
!INTEGER(I4B), PARAMETER :: ITMAX=100
!REAL(DP), PARAMETER :: CGOLD=0.3819660_sp,ZEPS=1.0e-3_sp*epsilon(ax)
!INTEGER(I4B) :: iter
!REAL(DP) :: a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
!a=min(ax,cx)
!b=max(ax,cx)
!v=bx
!w=v
!x=v
!e=0.0
!fx=func(x)
!fv=fx
!fw=fx
!do iter=1,ITMAX
!	xm=0.5_sp*(a+b)
!	tol1=tol*abs(x)+ZEPS
!	tol2=2.0_sp*tol1
!	if (abs(x-xm) <= (tol2-0.5_sp*(b-a))) then
!		xmin=x
!		brent=fx
!		RETURN
!	end if
!	if (abs(e) > tol1) then
!		r=(x-w)*(fx-fv)
!		q=(x-v)*(fx-fw)
!		p=(x-v)*q-(x-w)*r
!		q=2.0_sp*(q-r)
!		if (q > 0.0) p=-p
!		q=abs(q)
!		etemp=e
!		e=d
!		if (abs(p) >= abs(0.5_sp*q*etemp) .or. &
!			p <= q*(a-x) .or. p >= q*(b-x)) then
!			e=merge(a-x,b-x, x >= xm )
!			d=CGOLD*e
!		else
!			d=p/q
!			u=x+d
!			if (u-a < tol2 .or. b-u < tol2) d=sign(tol1,xm-x)
!		end if
!	else
!		e=merge(a-x,b-x, x >= xm )
!		d=CGOLD*e
!	end if
!	u=merge(x+d,x+sign(tol1,d), abs(d) >= tol1 )
!	fu=func(u)
!	if (fu <= fx) then
!		if (u >= x) then
!			a=x
!		else
!			b=x
!		end if
!		call shft(v,w,x,u)
!		call shft(fv,fw,fx,fu)
!	else
!		if (u < x) then
!			a=u
!		else
!			b=u
!		end if
!		if (fu <= fw .or. w == x) then
!			v=w
!			fv=fw
!			w=u
!			fw=fu
!		else if (fu <= fv .or. v == x .or. v == w) then
!			v=u
!			fv=fu
!		end if
!	end if
!end do
!!call nrerror('brent: exceed maximum iterations')
!    
!CONTAINS
!!BL
!SUBROUTINE shft(a,b,c,d)
!REAL(DP), INTENT(OUT) :: a
!REAL(DP), INTENT(INOUT) :: b,c
!REAL(DP), INTENT(IN) :: d
!a=b
!b=c
!c=d
!END SUBROUTINE shft
!END FUNCTION brent
!! --------------------------------------------------------------------
    
    
    
    SUBROUTINE BVLS (A, B, BND, X, RNORM, NSETP, W, INDEX, IERR)

!   Given an M by N matrix, A, and an M-vector, B,  compute an
!   N-vector, X, that solves the least-squares problem A *X = B
!   subject to X(J) satisfying  BND(1,J)  <=  X(J)  <=  BND(2,J)  
!   
!   The values BND(1,J) = -huge(ONE) and BND(2,J) = huge(ONE) are 
!   suggested choices to designate that there is no constraint in that 
!   direction.  The parameter ONE is 1.0 in the working precision.
!
!   This algorithm is a generalization of  NNLS, that solves
!   the least-squares problem,  A * X = B,  subject to all X(J)  >=  0.
!   The subroutine NNLS appeared in 'SOLVING LEAST SQUARES PROBLEMS,' 
!   by Lawson and Hanson, Prentice-Hall, 1974.  Work on BVLS was started 
!   by C. L. Lawson and R. J. Hanson at Jet Propulsion Laboratory, 
!   1973 June 12.  Many modifications were subsequently made.
!   This Fortran 90 code was completed in April, 1995 by R. J. Hanson.
!   Assumed shape arrays, automatic arrays, array operations, internal 
!   subroutines and F90 elemental functions are used.  Comments are 
!   prefixed with !  More than 72 characters appear on some lines.
!   The BVLS package is an additional item for the reprinting of the book 
!   by SIAM Publications and the distribution of the code package 
!   using netlib and Internet or network facilities.

!INTERFACE
!   SUBROUTINE BVLS (A, B, BND, X, RNORM, NSETP, W, INDEX, IERR)
!    REAL(KIND(1E0)) A(:,:), B(:), BND(:,:), X(:), RNORM, W(:)
!    INTEGER NSETP, INDEX(:), IERR
!   END SUBROUTINE
!END INTERFACE

!   A(:,:)     [INTENT(InOut)]
!   	On entry A() contains the M by N matrix, A.
!   	On return A() contains the product matrix, Q*A, where 
!   	Q is an M by M orthogonal matrix generated by this 
!   	subroutine.  The dimensions are M=size(A,1) and N=size(A,2).
!
!   B(:)     [INTENT(InOut)]
!   	On entry B() contains the M-vector, B.
!   	On return, B() contains Q*B.  The same Q multiplies A. 
!
!   BND(:,:)  [INTENT(In)]
!   	BND(1,J) is the lower bound for X(J).
!   	BND(2,J) is the upper bound for X(J).
!   	Require:  BND(1,J)  <=  BND(2,J).
!
!   X(:)    [INTENT(Out)]
!   	On entry X() need not be initialized.  On return,
!   	X() will contain the solution N-vector.
!
!   RNORM    [INTENT(Out)]
!   	The Euclidean norm of the residual vector, b - A*X.
!
!   NSETP    [INTENT(Out)]
!   	Indicates the number of components of the solution
!   	vector, X(), that are not at their constraint values.
!
!   W(:)     [INTENT(Out)]
!   	An N-array.  On return, W() will contain the dual solution 
!   	vector.   Using Set definitions below: 
!   	W(J) = 0 for all j in Set P, 
!   	W(J)  <=  0 for all j in Set Z, such that X(J) is at its 
!   	lower bound, and
!   	W(J)  >=  0 for all j in Set Z, such that X(J) is at its 
!   	upper bound.
!   	If BND(1,J) = BND(2,J), so the variable X(J) is fixed,
!   	then W(J) will have an arbitrary value.
!   
!   INDEX(:)    [INTENT(Out)]
!   	An INTEGER working array of size N.  On exit the contents
!   	of this array define the sets P, Z, and F as follows:
!   
!   INDEX(1)   through INDEX(NSETP) =  Set P. 
!   INDEX(IZ1) through INDEX(IZ2)      = Set Z. 
!   INDEX(IZ2+1) through INDEX(N)     = Set F. 
!   IZ1 = NSETP + 1 = NPP1
!   	Any of these sets may be empty.  Set F is those components
!   	that are constrained to a unique value by the given 
!   	constraints.   Sets P and Z are those that are allowed a non-
!   	zero range of values.  Of these, set Z are those whose final
!   	value is a constraint value, while set P are those whose 
!   	final value is not a constraint.  The value of IZ2 is not returned.
!...	It is computable as the number of bounds constraining a component
!...	of X uniquely.
!
!   IERR    [INTENT(Out)]
!   Indicates status on return.
!  	= 0   Solution completed.
!   	= 1   M  <=  0 or N  <=  0
!   	= 2   B(:), X(:), BND(:,:), W(:), or INDEX(:) size or shape violation.
!   	= 3   Input bounds are inconsistent.
!   	= 4   Exceed maximum number of iterations.
!
!   Selected internal variables:
!   EPS [real(kind(one))]
!   	Determines the relative linear dependence of a column vector
!   	for a variable moved from its initial value.  This is used in 
!   	one place with the default value EPS=EPSILON(ONE).  Other
!   	values, larger or smaller may be needed for some problems.
!   	Library software will likely make this an optional argument.
!
!   ITMAX  [integer]
!   	Set to 3*N.  Maximum number of iterations permitted.
!   	This is usually larger than required.  Library software will 
!   	likely make this an optional argument.
!   ITER   [integer]
!   	Iteration counter.

      implicit none
      logical FIND, HITBND, FREE1, FREE2, FREE 
      integer IERR, M, N 
      integer I, IBOUND, II, IP, ITER, ITMAX, IZ, IZ1, IZ2
      integer J, JJ, JZ, L, LBOUND, NPP1, NSETP

integer INDEX(:)
!
!   Z()     [Scratch]  An M-array (automatic) of working space.
!   S()     [Scratch]  An N-array (automatic) of working space.
!
      real(kind(1E0)), parameter :: ZERO = 0E0, ONE=1E0, TWO = 2E0
      real(kind(one)) :: A(:,:), B(:), S(size(A,2)), X(:), W(:),&
                                 Z(size(A,1)), BND(:,:)
      real(kind(one)) ALPHA, ASAVE, CC, EPS, RANGE, RNORM
      real(kind(one)) NORM, SM, SS, T, UNORM, UP, ZTEST

CALL  INITIALIZE 
!   
!   The above call will set IERR. 
LOOPA: DO
!   
!   Quit on error flag, or if all coefficients are already in the 
!   solution, .or. if M columns of A have been triangularized.
   IF  (IERR  /=   0  .or.  IZ1  > IZ2 .or. NSETP >= M) exit LOOPA   
!   
   CALL  SELECT_ANOTHER_COEFF_TO!_SOLVE_FOR 
!   
!   See if no index was found to be moved from set Z to set P.   
!   Then go to termination.   
   IF  ( .not. FIND ) exit LOOPA 
!   
   CALL  MOVE_J_FROM_SET_Z_TO_SET_P
!   
   CALL  TEST_SET_P_AGAINST_CONSTRAINTS
!   
!   The above call may set IERR. 
!   All coefficients in set P are strictly feasible.  Loop back.
END DO LOOPA  
!   
CALL  TERMINATION
RETURN

CONTAINS  ! These are internal subroutines.
SUBROUTINE INITIALIZE 
   M=size(A,1); N=size(A,2)
   IF  (M  <=  0 .or. N  <=  0) then
      IERR = 1
      RETURN
   END IF
!
! Check array sizes for consistency and with M and N.
   IF(SIZE(X) < N) THEN
      IERR=2
      RETURN
   END IF
   IF(SIZE(B) < M) THEN
      IERR=2
      RETURN
   END IF
   IF(SIZE(BND,1) /= 2) THEN
      IERR=2
      RETURN 
   END IF
   IF(SIZE(BND,2) < N) THEN
      IERR=2
      RETURN
   END IF
   IF(SIZE(W) < N) THEN
      IERR=2
      RETURN
   END IF
   IF(SIZE(INDEX) < N) THEN
      IERR=2
      RETURN
   END IF

IERR = 0  
!   The next two parameters can be changed to be optional for library software.
   EPS = EPSILON(ONE)
   ITMAX=3*N 
   ITER=0
!   Initialize the array index().  
   DO I=1,N
      INDEX(I)=I
   END DO
!   
   IZ2=N 
   IZ1=1 
   NSETP=0   
   NPP1=1
!   
!   Begin:  Loop on IZ to initialize  X().
   IZ=IZ1
   DO
      IF  (IZ  >  IZ2 ) EXIT
      J=INDEX(IZ)
      IF  ( BND(1,J)   <=   -huge(ONE)) then 
         IF  (BND(2,J)   >=    huge(ONE)) then   
            X(J) = ZERO 
         else   
            X(J) = min(ZERO,BND(2,J)) 
         END IF
     ELSE  IF  ( BND(2,J)   >=   huge(ONE)) then 
        X(J) = max(ZERO,BND(1,J))
     else  
        RANGE = BND(2,J) - BND(1,J)
        IF  ( RANGE   <=   ZERO ) then 
!
!   Here X(J) is constrained to a single value. 
           INDEX(IZ)=INDEX(IZ2)
           INDEX(IZ2)=J
           IZ=IZ-1 
           IZ2=IZ2-1   
           X(J)=BND(1,J)   
           W(J)=ZERO   
         ELSE  IF  ( RANGE  >  ZERO) then  
!
!   The following statement sets X(J) to 0 if the constraint interval
!   includes 0, and otherwise sets X(J) to the endpoint of the 
!   constraint interval that is closest to 0.
!   
            X(J) = max(BND(1,J), min(BND(2,J),ZERO))
         else   
            IERR = 3   
            RETURN   
         END IF! ( RANGE:.) 
   END IF
!
      IF  ( abs(X(J))   >   ZERO ) then 
!
!   Change B() to reflect a nonzero starting value for X(J). 
         B(1:M)=B(1:M)-A(1:M,J)*X(J) 
      END IF
      IZ=IZ+1 
   END DO! ( IZ   <=   IZ2 )
END SUBROUTINE! ( INITIALIZE )  

SUBROUTINE  SELECT_ANOTHER_COEFF_TO!_SOLVE_FOR 
!
!   1. Search through set z for a new coefficient to solve for.
!   First select a candidate that is either an unconstrained 
!   coefficient or else a constrained coefficient that has room
!   to move in the direction consistent with the sign of its dual
!   vector component.  Components of the dual (negative gradient)
!   vector will be computed as needed.
!   2. For each candidate start the transformation to bring this 
!   candidate into the triangle, and then do two tests:  Test size 
!   of new diagonal value to avoid extreme ill-conditioning, and 
!   the value of this new coefficient to be sure it moved in the 
!   expected direction.   
!   3. If some coefficient passes all these conditions, set FIND = true,
!   The index of the selected coefficient is J = INDEX(IZ).
!   4. If no coefficient is selected, set FIND = false.
!
   FIND = .FALSE.
   DO IZ=IZ1,IZ2 
      J=INDEX(IZ)   
!
!   Set FREE1 = true if X(J) is not at the left end-point of its 
!   constraint region.
!   Set FREE2 = true if X(J) is not at the right end-point of its 
!   constraint region.
!   Set FREE = true if X(J) is not at either end-point of its 
!   constraint region.
!
     FREE1 = X(J)   >   BND(1,J) 
     FREE2 = X(J)   <   BND(2,J) 
     FREE = FREE1 .and. FREE2  

     IF  ( FREE ) then   
        CALL TEST_COEF_J_FOR_DIAG!_ELT_AND_DIRECTION_OF_CHANGE
     else
!   Compute dual coefficient W(J).   
           W(J)=dot_product(A(NPP1:M,J),B(NPP1:M))  
!
!   Can X(J) move in the direction indicated by the sign of W(J)?
!
          IF  ( W(J)   <  ZERO ) then  
             IF  ( FREE1 ) &
                CALL TEST_COEF_J_FOR_DIAG!_ELT_AND_DIRECTION_OF_CHANGE
          ELSE  IF  ( W(J)   >  ZERO ) then
             IF  ( FREE2 ) &
                CALL TEST_COEF_J_FOR_DIAG!_ELT_AND_DIRECTION_OF_CHANGE
          END IF
       END IF
       IF  ( FIND ) RETURN
   END DO!  IZ  
END SUBROUTINE! ( SELECT ANOTHER COEF TO SOLVE FOR ) 

SUBROUTINE TEST_COEF_J_FOR_DIAG!_ELT_AND_DIRECTION_OF_CHANGE
!
!   The sign of W(J) is OK for J to be moved to set P.
!   Begin the transformation and check new diagonal element to avoid
!   near linear dependence.   
!   
   ASAVE=A(NPP1,J)   
   call HTC (NPP1, A(1:M,J), UP)
   UNORM = NRM2(A(1:NSETP,J))
   IF  ( abs(A(NPP1,J)) > EPS * UNORM) then
!
!   Column J is sufficiently independent.  Copy b into Z, update Z.
      Z(1:M)=B(1:M)
! Compute product of transormation and updated right-hand side.
      NORM=A(NPP1,J); A(NPP1,J)=UP
      IF(ABS(NORM) > ZERO) THEN
         SM=DOT_PRODUCT(A(NPP1:M,J)/NORM, Z(NPP1:M))/UP
         Z(NPP1:M)=Z(NPP1:M)+SM*A(NPP1:M,J)
         A(NPP1,J)=NORM
      END IF

      IF  (abs(X(J)) >  ZERO) Z(1:NPP1)=Z(1:NPP1)+A(1:NPP1,J)*X(J)
!   Adjust Z() as though X(J) had been reset to zero. 
      IF  ( FREE ) then   
         FIND = .TRUE.  
      else  
!
!   Solve for ZTEST ( proposed new value for X(J) ).
!   Then set FIND to indicate if ZTEST has moved away from X(J) in
!   the expected direction indicated by the sign of W(J).
         ZTEST=Z(NPP1)/A(NPP1,J)
         FIND = ( W(J)  <  ZERO  .and.  ZTEST   <  X(J) )  .or. & 
         ( W(J)  >  ZERO  .and.  ZTEST  >  X(J) )
      END IF
   END IF
!
!   If J was not accepted to be moved from set Z to set P,
!   restore A(NNP1,J).  Failing these tests may mean the computed 
!   sign of W(J) is suspect, so here we set W(J) = 0.  This will  
!   not affect subsequent computation, but cleans up the W() array.
   IF  ( .not. FIND ) then 
      A(NPP1,J)=ASAVE
      W(J)=ZERO 
   END IF! ( .not. FIND )
END SUBROUTINE !TEST_COEF_J_FOR_DIAG!_ELT_AND_DIRECTION_OF_CHANGE

SUBROUTINE MOVE_J_FROM_SET_Z_TO_SET_P
!
!   The index  J=index(IZ)  has been selected to be moved from
!   set Z to set P.  Z() contains the old B() adjusted as though X(J) = 0.  
!   A(*,J) contains the new Householder transformation vector.    
   B(1:M)=Z(1:M)
!
   INDEX(IZ)=INDEX(IZ1)  
   INDEX(IZ1)=J  
   IZ1=IZ1+1 
   NSETP=NPP1
   NPP1=NPP1+1   
!   The following loop can be null or not required.
   NORM=A(NSETP,J); A(NSETP,J)=UP
   IF(ABS(NORM) > ZERO) THEN
      DO JZ=IZ1,IZ2 
         JJ=INDEX(JZ)
         SM=DOT_PRODUCT(A(NSETP:M,J)/NORM, A(NSETP:M,JJ))/UP
         A(NSETP:M,JJ)=A(NSETP:M,JJ)+SM*A(NSETP:M,J)
      END DO
   A(NSETP,J)=NORM
   END IF
!   The following loop can be null.
   DO L=NPP1,M   
      A(L,J)=ZERO
   END DO!  L
!
   W(J)=ZERO 
!
!   Solve the triangular system.  Store this solution temporarily in Z().
   DO I = NSETP, 1, -1
      IF  (I  /=  NSETP) Z(1:I)=Z(1:I)-A(1:I,II)*Z(I+1)
      II=INDEX(I)   
      Z(I)=Z(I)/A(I,II) 
   END DO 
END SUBROUTINE! ( MOVE J FROM SET Z TO SET P )  

SUBROUTINE TEST_SET_P_AGAINST_CONSTRAINTS 
!
   LOOPB: DO
!   The solution obtained by solving the current set P is in the array Z().
!
      ITER=ITER+1   
      IF  (ITER   >  ITMAX) then 
         IERR = 4   
         exit LOOPB   
      END IF
!
      CALL SEE_IF_ALL_CONSTRAINED_COEFFS!_ARE_FEASIBLE
!
!   The above call sets HITBND.  If HITBND = true then it also sets 
!   ALPHA, JJ, and IBOUND.  
     IF  ( .not. HITBND ) exit LOOPB   
!
!   Here ALPHA will be between 0 and 1 for interpolation  
!   between the old X() and the new Z().
      DO IP=1,NSETP 
         L=INDEX(IP)
         X(L)=X(L)+ALPHA*(Z(IP)-X(L))   
      END DO
!
      I=INDEX(JJ)   
!   Note:  The exit test is done at the end of the loop, so the loop 
!   will always be executed at least once.
      DO
!   
!   Modify A(*,*), B(*) and the index arrays to move coefficient I
!   from set P to set Z.   
!
         CALL  MOVE_COEF_I_FROM_SET_P_TO_SET_Z
!
         IF  (NSETP  <=  0) exit LOOPB  
!
!   See if the remaining coefficients in set P are feasible.  They should
!   be because of the way ALPHA was determined.  If any are infeasible 
!   it is due to round-off error.  Any that are infeasible or on a boundary 
!   will be set to the boundary value and moved from set P to set Z.
!
           IBOUND = 0
           DO JJ=1,NSETP 
              I=INDEX(JJ)
              IF  ( X(I)  <=  BND(1,I)) then 
                  IBOUND=1
                  EXIT
              ELSE IF ( X(I)  >=  BND(2,I)) then
                  IBOUND=2
                  EXIT
              END IF
            END DO
            IF  (IBOUND   <=   0)   EXIT
      END DO
!
!   Copy B( ) into Z( ).  Then solve again and loop back. 
      Z(1:M)=B(1:M)
!   
      DO I = NSETP, 1, -1
         IF  (I  /=  NSETP) Z(1:I)=Z(1:I)-A(1:I,II)*Z(I+1)
         II=INDEX(I)
         Z(I)=Z(I)/A(I,II)  
      END DO
   END DO LOOPB

!   The following loop can be null.
   DO IP=1,NSETP 
      I=INDEX(IP)
      X(I)=Z(IP) 
   END DO

END SUBROUTINE! ( TEST SET P AGAINST CONSTRAINTS)

SUBROUTINE  SEE_IF_ALL_CONSTRAINED_COEFFS!_ARE_FEASIBLE
!
!   See if each coefficient in set P is strictly interior to its constraint region.
!   If so, set HITBND = false.
!   If not, set HITBND = true, and also set ALPHA, JJ, and IBOUND.
!   Then ALPHA will satisfy  0.  < ALPHA  <=  1.
!
   ALPHA=TWO 
   DO IP=1,NSETP
      L=INDEX(IP)   
      IF  (Z(IP)  <=  BND(1,L)) then
!   Z(IP) HITS LOWER BOUND 
         LBOUND=1   
      ELSE  IF  (Z(IP)  >=  BND(2,L)) then
!   Z(IP) HITS UPPER BOUND 
         LBOUND=2   
      else  
         LBOUND = 0 
      END IF
!
      IF  ( LBOUND   /=   0 ) then  
         T=(BND(LBOUND,L)-X(L))/(Z(IP)-X(L)) 
         IF  (ALPHA   >  T) then
           ALPHA=T  
           JJ=IP
           IBOUND=LBOUND
         END IF! ( LBOUND )  
      END IF! ( ALPHA   >  T )   
   END DO 
HITBND = abs(ALPHA   -  TWO) > ZERO  
END SUBROUTINE!( SEE IF ALL CONSTRAINED COEFFS ARE FEASIBLE )

SUBROUTINE MOVE_COEF_I_FROM_SET_P_TO_SET_Z
!   
   X(I)=BND(IBOUND,I)
   IF  (abs(X(I))   >  ZERO .and.  JJ > 0) B(1:JJ)=B(1:JJ)-A(1:JJ,I)*X(I)

!   The following loop can be null.
   DO J = JJ+1, NSETP 
      II=INDEX(J)
      INDEX(J-1)=II  
      call ROTG (A(J-1,II),A(J,II),CC,SS)
      SM=A(J-1,II)
!
!   The plane rotation is applied to two rows of A and the right-hand
!   side.  One row is moved to the scratch array S and then the updates
!   are computed.  The intent is for array operations to be performed 
!   and minimal extra data movement.  One extra rotation is applied 
!   to column II in this approach. 
      S=A(J-1,1:N); A(J-1,1:N)=CC*S+SS*A(J,1:N); A(J,1:N)=CC*A(J,1:N)-SS*S
      A(J-1,II)=SM; A(J,II)=ZERO
      SM=B(J-1); B(J-1)=CC*SM+SS*B(J); B(J)=CC*B(J)-SS*SM
   END DO
!
   NPP1=NSETP
   NSETP=NSETP-1 
   IZ1=IZ1-1 
   INDEX(IZ1)=I  
END SUBROUTINE! ( MOVE COEF I FROM SET P TO SET Z ) 

SUBROUTINE TERMINATION
!
   IF  (IERR   <=   0) then  
!
!   Compute the norm of the residual vector.
      SM=ZERO   
      IF  (NPP1   <=   M) then 
         SM=NRM2(B(NPP1:M))
      else  
         W(1:N)=ZERO
      END IF
      RNORM=SM
   END IF! ( IERR...) 
END SUBROUTINE! ( TERMINATION ) 

SUBROUTINE ROTG(SA,SB,C,S)
!
   REAL(KIND(ONE)) SA,SB,C,S,ROE,SCALE,R 
!
   ROE = SB
   IF( ABS(SA) .GT. ABS(SB) ) ROE = SA
   SCALE = ABS(SA) + ABS(SB)
   IF( SCALE .LE. ZERO ) THEN
      C = ONE
      S = ZERO
      RETURN
  END IF
   R = SCALE*SQRT((SA/SCALE)**2 + (SB/SCALE)**2)
   IF(ROE < ZERO)R=-R
   C = SA/R
   S = SB/R
   SA = R
   RETURN
END SUBROUTINE !ROTG

REAL(KIND(ONE)) FUNCTION NRM2 (X)
!
!   NRM2 returns the Euclidean norm of a vector via the function
!   name, so that
!
!   NRM2 := sqrt( x'*x )
!
   REAL(KIND(ONE)) ABSXI, X(:), NORM, SCALE, SSQ
   INTEGER N, IX
   N=SIZE(X)
   IF( N < 1)THEN
      NORM  = ZERO
   ELSE IF( N == 1 )THEN
      NORM  = ABS( X( 1 ) )
   ELSE
      SCALE = ZERO
      SSQ   = ONE
!
      DO IX = 1, N
         ABSXI = ABS( X( IX ) )
          IF(ABSXI > ZERO )THEN
             IF( SCALE < ABSXI )THEN
                SSQ   = ONE + SSQ*( SCALE/ABSXI )**2
                SCALE = ABSXI
             ELSE
                SSQ   = SSQ + ( ABSXI/SCALE )**2
             END IF
         END IF
     END DO
     NORM  = SCALE * SQRT( SSQ )
   END IF
!
   NRM2 = NORM
   RETURN
END FUNCTION

SUBROUTINE HTC (P, U, UP)
!
!   Construct a Householder transormation.
   INTEGER P
   REAL(KIND(ONE)) U(:)
   REAL UP, VNORM
   VNORM=NRM2(U(P:SIZE(U)))
   IF(U(P) > ZERO) VNORM=-VNORM
   UP=U(P)-VNORM
   U(P)=VNORM
END SUBROUTINE ! HTC

END SUBROUTINE ! BVLS    
    
    
    
end module solvers_mod
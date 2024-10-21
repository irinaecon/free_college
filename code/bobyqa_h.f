C  Important Notice:
C   These algorithms are modifications and based on the Software BOBYQA, authored by M. J. D. Powell,
C   to minimize sum of squares with bound constraints by taking advantage of the problem structure.
C   i.e. Min  F(x) := Sum_{i=1}^{mv}  v_err_i(x)^2, s.t. xl <= x <= xu, x \in R^n,
C   where v_err(x) : R^n \to R^{mv} is a vector function.
C   This subroutine seeks the least value of sum of the squres of the components of v_err(x)
C   by combing trust region method and Levenberg-Marquardt method 
C
C   References:
C
C   1.  M. J. D. Powell, The NEWUOA software for unconstrained optimization without derivatives,
C       DAMTP 2004/ NA 05
C   2.  M. J. D. Powell, The BOBYQA algorithm for bound constrained optimization without derivatives,
C       DAMTP 2009/ NA 06    
C   3.  H. Zhang, A. R. CONN, AND K. SCHEINBERG, A derivative-free algorithm for the least-squares 
C       minimization, SIAM Journal on Optimization, 20 (2010), pp. 3555-3576. 
C   4.  H. Zhang, A. R. CONN, On the local convergence of a derivative-free algorithm for least-squares 
C       minimization,Computational Optimization and Applications, DOI 10.1007/s10589-010-9367-x  
C
C      -----------------------------------------------------------------
C      | This program is free software; you can redistribute it and/or  |
C      |modify it under the terms of the GNU General Public License as  |
C      |published by the Free Software Foundation; either version 2 of  |
C      |the License, or (at your option) any later version.             |
C      |This program is distributed in the hope that it will be useful, |
C      |but WITHOUT ANY WARRANTY; without even the implied warranty of  |
C      |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   |
C      |GNU General Public License for more details.                    |
C      |                                                                |
C      |You should have received a copy of the GNU General Public       |
C      |License along with this program; if not, write to the Free      |
C      |Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, |
C      |MA  02110-1301  USA                                             |
C      -----------------------------------------------------------------|

      SUBROUTINE BOBYQA_H (N,NPT,X,XL,XU,RHOBEG,RHOEND,IPRINT,
     1  MAXFUN,W,mv,sub_func)
     
      IMPLICIT double precision (A-H,O-Z)
      DIMENSION X(*),XL(*),XU(*),W(*)
      
      
      
      interface
            subroutine sub_func(n, mv, x, v_err )
                implicit none
                integer n,mv
                double precision x(n), v_err(mv)
                
               
            end subroutine sub_func
      end interface
      
    
C
C     N must be set to the number of variables and must be at least two.
C     mv must be set to the lengh of the vector function v_err(x):  R^n \to R^{mv}. 
C     The maximum number variables in this codes: nmax =  100
C     The maximum lengh of the vector function v_err(x): mmax = 400
C     If n > 100 or m > 400, the parameter nmax and mmax need to be creased in
C     subroutine  BOBYQB_H, PRELIM_H, RESCUE_H and TRSBOX_H
C
C     NPT is the number of interpolation conditions. Its value must be in the
C     interval [N+2,2N+1].  Recommended: NPT = 2*N+1
C
C     Initial values of the variables must be set in X(1),X(2),...,X(N). They
C     will be changed to the values that give the least calculated F= Sum_{i=1}^{mv} v_err_i(x)^2..
C 
C     For I=1,2,...,N, XL(I) and XU(I) must provide the lower and upper
C       bounds, respectively, on X(I). The construction of quadratic models
C       requires XL(I) to be strictly less than XU(I) for each I. Further,
C       the contribution to a model from changes to the I-th variable is
C       damaged severely by rounding errors if XU(I)-XL(I) is too small.
C
C     RHOBEG and RHOEND must be set to the initial and final values of a trust
C       region radius, so both must be positive with RHOEND no greater than
C       RHOBEG. Typically, RHOBEG should be about one tenth of the greatest
C       expected change to a variable, while RHOEND should indicate the
C       accuracy that is required in the final values of the variables. An
C       error return occurs if any of the differences XU(I)-XL(I), I=1,...,N,
C       is less than 2*RHOBEG. Default: RHOBEG = 1.0, RHOEND = 10^{-8}
C
C     The value of IPRINT should be set to 0, 1, 2 or 3, which controls the
C       amount of printing. Specifically, there is no output if IPRINT=0 and
C       there is output only at the return if IPRINT=1. Otherwise, each new
C       value of RHO is printed, with the best vector of variables so far and
C       the corresponding value of the objective function. Further, each new
C       value of F with its variables are output if IPRINT=3.
C
C     MAXFUN must be set to an upper bound on the number of calls of subroutine
C     dfovec(n, mv, x, v_err) which provides the values of the vector function v_err(x).
C     Here: n, mv, x \in R^n are input, v_err \in R^{mv} are output.
C     Default:  MAXFUN= 400(n+1), i.e 400 (simplex) gradients for reasonable accuracy.
C               MAXFUN= infinity, to let the algorithm explore the lowest function value  
C                       as much as it could.
C
C     The array W will be used for working space. Its length must be at least
C       (NPT+5)*(NPT+N)+3*N*(N+5)/2.
C
C     SUBROUTINE dfovec(n, mv, x, v_err) must be provided by the user. 
C     It must provide the values of the vector function v_err(x) : R^n to R^{mv} 
C     at the variables X(1),X(2),...,X(N), which are generated automatically in 
C     a way that satisfies the bounds given in XL and XU.
C
C     Return if the value of NPT is unacceptable.
      
C
      NP=N+1
      IF (NPT .LT. N+2 .OR. NPT .GT. 2*N+1) THEN
          PRINT 10
   10     FORMAT (/4X,'Return from NEWUOA because NPT is not in',
     1      '[N+2, 2N+1]')     
          GO TO 40
      END IF
C
C     Partition the working space array, so that different parts of it can
C     be treated separately during the calculation of BOBYQB. The partition
C     requires the first (NPT+2)*(NPT+N)+3*N*(N+5)/2 elements of W plus the
C     space that is taken by the last array in the argument list of BOBYQB.
C
      NDIM=NPT+N
      IXB=1
      IXP=IXB+N
      IFV=IXP+N*NPT
      IXO=IFV+NPT
      IGO=IXO+N
      IHQ=IGO+N
      IPQ=IHQ+(N*NP)/2
      IBMAT=IPQ+NPT
      IZMAT=IBMAT+NDIM*N
      ISL=IZMAT+NPT*(NPT-NP)
      ISU=ISL+N
      IXN=ISU+N
      IXA=IXN+N
      ID=IXA+N
      IVL=ID+N
      IW=IVL+NDIM
C
C     Return if there is insufficient space between the bounds. Modify the
C     initial X if necessary in order to avoid conflicts between the bounds
C     and the construction of the first quadratic model. The lower and upper
C     bounds on moves from the updated X are set now, in the ISL and ISU
C     partitions of W, in order to provide useful and exact information about
C     components of X that become within distance RHOBEG from their bounds.
C
      ZERO=0.0D0
      DO 30 J=1,N
      TEMP=XU(J)-XL(J)
      IF (TEMP .LT. RHOBEG+RHOBEG) THEN
          PRINT 20
   20     FORMAT (/4X,'Return from BOBYQA_H because one of the',
     1      ' differences XU(I)-XL(I)'/6X,' is less than 2*RHOBEG.')
          GO TO 40
      END IF
      JSL=ISL+J-1
      JSU=JSL+N
      W(JSL)=XL(J)-X(J)
      W(JSU)=XU(J)-X(J)
      if (w(jsl) .ge. -rhobeg) then
          if (w(jsl) .ge. zero) then
              x(j)=xl(j)
              w(jsl)=zero
              w(jsu)=temp
          else
              x(j)=xl(j)+rhobeg
              w(jsl)=-rhobeg
              w(jsu)=dmax1(xu(j)-x(j),rhobeg)
          end if
      else if (w(jsu) .le. rhobeg) then
          if (w(jsu) .le. zero) then
              x(j)=xu(j)
              w(jsl)=-temp
              w(jsu)=zero
          else
              x(j)=xu(j)-rhobeg
              w(jsl)=dmin1(xl(j)-x(j),-rhobeg)
              w(jsu)=rhobeg
          end if
      end if

   30 CONTINUE
C
C     Make the call of BOBYQB_H.
C
      CALL BOBYQB_H (N,NPT,X,XL,XU,RHOBEG,RHOEND,IPRINT,MAXFUN,W(IXB),
     1  W(IXP),W(IFV),W(IXO),W(IGO),W(IHQ),W(IPQ),W(IBMAT),W(IZMAT),
     2  NDIM,W(ISL),W(ISU),W(IXN),W(IXA),W(ID),W(IVL),W(IW),mv,sub_func)
   40 RETURN
      END



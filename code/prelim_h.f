C
C   Important Notice:
C   This PRELIM_H are modifications and based on the subroutine PRELIM in the software 
C   BOBYQA, authored by M. J. D. Powell.
C 
      SUBROUTINE PRELIM_H (N,NPT,X,XL,XU,RHOBEG,IPRINT,MAXFUN,XBASE,
     1  XPT,FVAL,GOPT,HQ,PQ,BMAT,ZMAT,NDIM,SL,SU,NF,KOPT,
     1  mv,HQV,GQV,PQV,FVAL_V,v_err,v_beg,FBEG,sub_func)
      IMPLICIT double precision (A-H,O-Z)
      parameter (nmax = 100, mmax=400)
      DIMENSION X(*),XL(*),XU(*),XBASE(*),XPT(NPT,*),FVAL(*),GOPT(*),
     1  HQ(*),PQ(*),BMAT(NDIM,*),ZMAT(NPT,*),SL(*),SU(*)
      dimension HQV(mmax,*),GQV(mmax,*),PQV(mmax,*),FVAL_V(mmax,*)
      dimension v_err(*),v_beg(*)
      
       interface
            subroutine sub_func(n, mv, x, v_err )
                implicit none
                integer n,mv
                double precision x(n), v_err(mv)
                
               
            end subroutine sub_func
        end interface
C
C     Set some constants.
C
      HALF=0.5D0
      ONE=1.0D0
      TWO=2.0D0
      ZERO=0.0D0
      RHOSQ=RHOBEG*RHOBEG
      RECIP=ONE/RHOSQ
      NP=N+1
C
C     Set XBASE to the initial vector of variables, and set the initial
C     elements of XPT, BMAT, HQ, PQ and ZMAT to zero.
C
      DO 20 J=1,N
      XBASE(J)=X(J)
      DO 10 K=1,NPT
   10 XPT(K,J)=ZERO
      DO 20 I=1,NDIM
   20 BMAT(I,J)=ZERO
      DO 30 IH=1,(N*NP)/2
      do 25 m1=1, mv
   25 HQV(m1,IH)=zero
   30 HQ(IH)=ZERO
      DO 40 K=1,NPT
      do 35 m1=1, mv
   35 PQV(m1,K)=zero
      PQ(K)=ZERO
      DO 40 J=1,NPT-NP
   40 ZMAT(K,J)=ZERO
C
C     Begin the initialization procedure. NF becomes one more than the number
C     of function values so far. The coordinates of the displacement of the
C     next initial interpolation point from XBASE are set in XPT(NF+1,.).
C
      NF=0
   50 NFM=NF
      NFX=NF-N
      NF=NF+1
      IF (NFM .LE. 2*N) THEN
          IF (NFM .GE. 1 .AND. NFM .LE. N) THEN
              STEPA=RHOBEG
              IF (SU(NFM) .EQ. ZERO) STEPA=-STEPA
              XPT(NF,NFM)=STEPA
          ELSE IF (NFM .GT. N) THEN
              STEPA=XPT(NF-N,NFX)
              STEPB=-RHOBEG
              IF (SL(NFX) .EQ. ZERO) STEPB=DMIN1(TWO*RHOBEG,SU(NFX))
              IF (SU(NFX) .EQ. ZERO) STEPB=DMAX1(-TWO*RHOBEG,SL(NFX))
              XPT(NF,NFX)=STEPB
          END IF 
      END IF
C
C     Calculate the next value of F. The least function value so far and
C     its index are required.
C
      DO 60 J=1,N
      X(J)=DMIN1(DMAX1(XL(J),XBASE(J)+XPT(NF,J)),XU(J))
      IF (XPT(NF,J) .EQ. SL(J)) X(J)=XL(J)
      IF (XPT(NF,J) .EQ. SU(J)) X(J)=XU(J)
   60 CONTINUE
C
C     dfovec(n, mv, x, v_err) provides the values of the vector function v_err(x): R^n \to R^{mv}.
C     Here: n, mv, x \in R^n are input, v_err \in R^{mv} are output.
C      call dfovec(n, mv, x, v_err)
      call sub_func(n, mv, x, v_err)
C
C     f_value(mv,v_err,F) provides the value of the sum of the squres of the components of v_err(x)
C     i.e. F = sum_{i=1}^{mv} v_err_i (x)^2         
      call f_value(mv,v_err,F)     
C      
      IF (IPRINT .EQ. 3) THEN
          PRINT 70, NF,F,(X(I),I=1,N)
   70      FORMAT (/4X,'Function number',I6,'    F =',1PD18.10,
     1       '    The corresponding X is:'/(2X,5D15.6))
      END IF
      FVAL(NF)=F
      do 72 m1=1,mv
   72 FVAL_V(m1,NF)=v_err(m1)      
      IF (NF .EQ. 1) THEN
          FBEG=F          
          KOPT=1
          do 75 m1=1,mv
   75     v_beg(m1) = v_err(m1)        
      ELSE IF (F .LT. FVAL(KOPT)) THEN
          KOPT=NF      
      END IF
C
C     Set the nonzero initial elements of BMAT and the quadratic model in the
C     cases when NF is at most 2*N+1. 
C
      IF (NF .LE. 2*N+1) THEN
          IF (NF .GE. 2 .AND. NF .LE. N+1) THEN
              do 78 m1=1,mv
   78         GQV(m1,NFM)=(v_err(m1)-v_beg(m1))/STEPA              
              IF (NPT .LT. NF+N) THEN
                  BMAT(1,NFM)=-ONE/STEPA
                  BMAT(NF,NFM)=ONE/STEPA
                  BMAT(NPT+NFM,NFM)=-HALF*RHOSQ
              END IF
          ELSE IF (NF .GE. N+2) THEN
              IH=(NFX*(NFX+1))/2
              DIFF=STEPB-STEPA
              do 79 m1=1, mv
                TEMP=(v_err(m1)-v_beg(m1))/STEPB
                HQV(m1,IH)=TWO*(TEMP-GQV(m1,NFX))/DIFF
                GQV(m1,NFX)=(GQV(m1,NFX)*STEPB-TEMP*STEPA)/DIFF
   79         continue                
              IF (STEPA*STEPB .LT. ZERO) THEN
                  IF (F .LT. FVAL(NF-N)) THEN
                      FVAL(NF)=FVAL(NF-N)
                      FVAL(NF-N)=F
                      do 80 m1=1, mv
                         FVAL_V(m1,NF)=FVAL_V(m1,NF-N)
                         FVAL_V(m1,NF-N)=v_err(m1)                      
   80                 continue                      
                      IF (KOPT .EQ. NF) KOPT=NF-N
                      XPT(NF-N,NFX)=STEPB
                      XPT(NF,NFX)=STEPA
                  END IF
              END IF
              BMAT(1,NFX)=-(STEPA+STEPB)/(STEPA*STEPB)
              BMAT(NF,NFX)=-HALF/XPT(NF-N,NFX)
              BMAT(NF-N,NFX)=-BMAT(1,NFX)-BMAT(NF,NFX)
              ZMAT(1,NFX)=DSQRT(TWO)/(STEPA*STEPB)
              ZMAT(NF,NFX)=DSQRT(HALF)/RHOSQ
              ZMAT(NF-N,NFX)=-ZMAT(1,NFX)-ZMAT(NF,NFX)
          END IF
      END IF
      IF (NF .LT. NPT .AND. NF .LT. MAXFUN) GOTO 50
      RETURN
      END


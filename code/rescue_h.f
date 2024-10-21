C
C   Important Notice:
C   This RESCUE_H are modifications and based on the subroutine RESCUE in the software 
C   BOBYQA, authored by M. J. D. Powell.
C 
      SUBROUTINE RESCUE_H (N,NPT,XL,XU,IPRINT,MAXFUN,XBASE,XPT,
     1  FVAL,XOPT,GOPT,HQ,PQ,BMAT,ZMAT,NDIM,SL,SU,NF,DELTA,
     2  KOPT,VLAG,PTSAUX,PTSID,W,
     3  mv,HQV,GQV,PQV,FVAL_V,WV,v_err,v_sumpq,v_vquad,
     4  v_diff,v_temp,sub_func)
      IMPLICIT double precision (A-H,O-Z)
      parameter (nmax = 100, mmax=400, nptmax=2*nmax+1)      
      DIMENSION XL(*),XU(*),XBASE(*),XPT(NPT,*),FVAL(*),XOPT(*),
     1  GOPT(*),HQ(*),PQ(*),BMAT(NDIM,*),ZMAT(NPT,*),SL(*),SU(*),
     2  VLAG(*),PTSAUX(2,*),PTSID(*),W(*)
      dimension HQV(mmax,*),GQV(mmax,*),PQV(mmax,*),FVAL_V(mmax,*),
     1  WV(mmax,*),v_err(*),v_sumpq(*),v_vquad(*),v_diff(*),v_temp(*)
      dimension v_fbase(mmax)
      
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
      ZERO=0.0D0
      NP=N+1
      SFRAC=HALF/DFLOAT(NP)
      NPTM=NPT-NP
C
C     Shift the interpolation points so that XOPT becomes the origin, and set
C     the elements of ZMAT to zero. The value of SUMPQ is required in the
C     updating of HQ below. The squares of the distances from XOPT to the
C     other interpolation points are set at the end of W. Increments of WINC
C     may be added later to these squares to balance the consideration of
C     the choice of point that is going to become current.
C
C      SUMPQ=ZERO
      do 5 m1=1,mv
    5 v_sumpq(m1)=ZERO 
      WINC=ZERO
      DO 20 K=1,NPT
      DISTSQ=ZERO
      DO 10 J=1,N
      XPT(K,J)=XPT(K,J)-XOPT(J)
   10 DISTSQ=DISTSQ+XPT(K,J)**2
      do 15 m1=1,mv
   15 v_sumpq(m1)= v_sumpq(m1)+PQV(m1,k) 
      W(NDIM+K)=DISTSQ
      WINC=DMAX1(WINC,DISTSQ)
      DO 20 J=1,NPTM
   20 ZMAT(K,J)=ZERO
C
C     Update HQ so that HQ and PQ define the second derivatives of the model
C     after XBASE has been shifted to the trust region centre.
C
      IH=0
      DO 40 J=1,N
      do 25 m1=1,mv
   25 WV(m1,J)=HALF*v_sumpq(m1)*XOPT(J)
      DO 30 K=1,NPT
      do 30 m1=1,mv
   30 WV(m1,J)=WV(m1,J)+PQV(m1,K)*XPT(K,J)
      DO 40 I=1,J
      IH=IH+1
      do 40 m1=1,mv
         HQV(m1,IH)=HQV(m1,IH)+WV(m1,I)*XOPT(J)
     &              +WV(m1,J)*XOPT(I)
   40 continue  
C
C     Shift XBASE, SL, SU and XOPT. Set the elements of BMAT to zero, and
C     also set the elements of PTSAUX.
C
      DO 50 J=1,N
      XBASE(J)=XBASE(J)+XOPT(J)
      SL(J)=SL(J)-XOPT(J)
      SU(J)=SU(J)-XOPT(J)
      XOPT(J)=ZERO
      PTSAUX(1,J)=DMIN1(DELTA,SU(J))
      PTSAUX(2,J)=DMAX1(-DELTA,SL(J))
      IF (PTSAUX(1,J)+PTSAUX(2,J) .LT. ZERO) THEN
          TEMP=PTSAUX(1,J)
          PTSAUX(1,J)=PTSAUX(2,J)
          PTSAUX(2,J)=TEMP
      END IF
      IF (DABS(PTSAUX(2,J)) .LT. HALF*DABS(PTSAUX(1,J))) THEN
          PTSAUX(2,J)=HALF*PTSAUX(1,J)
      END IF
      DO 50 I=1,NDIM
   50 BMAT(I,J)=ZERO
      FBASE=FVAL(KOPT)
      do 55 m1=1,mv
   55 v_fbase(m1)=FVAL_V(m1,KOPT)
C
C     Set the identifiers of the artificial interpolation points that are
C     along a coordinate direction from XOPT, and set the corresponding
C     nonzero elements of BMAT and ZMAT.
C
      PTSID(1)=SFRAC
      DO 60 J=1,N
      JP=J+1
      JPN=JP+N
      PTSID(JP)=DFLOAT(J)+SFRAC
      IF (JPN .LE. NPT) THEN
          PTSID(JPN)=DFLOAT(J)/DFLOAT(NP)+SFRAC
          TEMP=ONE/(PTSAUX(1,J)-PTSAUX(2,J))
          BMAT(JP,J)=-TEMP+ONE/PTSAUX(1,J)
          BMAT(JPN,J)=TEMP+ONE/PTSAUX(2,J)
          BMAT(1,J)=-BMAT(JP,J)-BMAT(JPN,J)
          ZMAT(1,J)=DSQRT(2.0D0)/DABS(PTSAUX(1,J)*PTSAUX(2,J))
          ZMAT(JP,J)=ZMAT(1,J)*PTSAUX(2,J)*TEMP
          ZMAT(JPN,J)=-ZMAT(1,J)*PTSAUX(1,J)*TEMP
      ELSE
          BMAT(1,J)=-ONE/PTSAUX(1,J)
          BMAT(JP,J)=ONE/PTSAUX(1,J)
          BMAT(J+NPT,J)=-HALF*PTSAUX(1,J)**2
      END IF
   60 CONTINUE
C
C     Set any remaining identifiers with their nonzero elements of ZMAT.
C
      IF (NPT .GE. N+NP) THEN
          DO 70 K=2*NP,NPT
          IW=(DFLOAT(K-NP)-HALF)/DFLOAT(N)
          IP=K-NP-IW*N
          IQ=IP+IW
          IF (IQ .GT. N) IQ=IQ-N
          PTSID(K)=DFLOAT(IP)+DFLOAT(IQ)/DFLOAT(NP)+SFRAC
          TEMP=ONE/(PTSAUX(1,IP)*PTSAUX(1,IQ))
          ZMAT(1,K-NP)=TEMP
          ZMAT(IP+1,K-NP)=-TEMP
          ZMAT(IQ+1,K-NP)=-TEMP
   70     ZMAT(K,K-NP)=TEMP
      END IF
      NREM=NPT
      KOLD=1
      KNEW=KOPT
C
C     Reorder the provisional points in the way that exchanges PTSID(KOLD)
C     with PTSID(KNEW).
C
   80 DO 90 J=1,N
      TEMP=BMAT(KOLD,J)
      BMAT(KOLD,J)=BMAT(KNEW,J)
   90 BMAT(KNEW,J)=TEMP
      DO 100 J=1,NPTM
      TEMP=ZMAT(KOLD,J)
      ZMAT(KOLD,J)=ZMAT(KNEW,J)
  100 ZMAT(KNEW,J)=TEMP
      PTSID(KOLD)=PTSID(KNEW)
      PTSID(KNEW)=ZERO
      W(NDIM+KNEW)=ZERO
      NREM=NREM-1
      IF (KNEW .NE. KOPT) THEN
          TEMP=VLAG(KOLD)
          VLAG(KOLD)=VLAG(KNEW)
          VLAG(KNEW)=TEMP
C
C     Update the BMAT and ZMAT matrices so that the status of the KNEW-th
C     interpolation point can be changed from provisional to original. The
C     branch to label 350 occurs if all the original points are reinstated.
C     The nonnegative values of W(NDIM+K) are required in the search below.
C
          CALL UPDATE (N,NPT,BMAT,ZMAT,NDIM,VLAG,BETA,DENOM,KNEW,W)
          IF (NREM .EQ. 0) GOTO 350
          DO 110 K=1,NPT
  110     W(NDIM+K)=DABS(W(NDIM+K))
      END IF
C
C     Pick the index KNEW of an original interpolation point that has not
C     yet replaced one of the provisional interpolation points, giving
C     attention to the closeness to XOPT and to previous tries with KNEW.
C
  120 DSQMIN=ZERO
      DO 130 K=1,NPT
      IF (W(NDIM+K) .GT. ZERO) THEN
          IF (DSQMIN .EQ. ZERO .OR. W(NDIM+K) .LT. DSQMIN) THEN
              KNEW=K
              DSQMIN=W(NDIM+K)
          END IF
      END IF
  130 CONTINUE
      IF (DSQMIN .EQ. ZERO) GOTO 260
C
C     Form the W-vector of the chosen original interpolation point.
C
      DO 140 J=1,N
  140 W(NPT+J)=XPT(KNEW,J)
      DO 160 K=1,NPT
      SUM=ZERO
      IF (K .EQ. KOPT) THEN
          CONTINUE
      ELSE IF (PTSID(K) .EQ. ZERO) THEN
          DO 150 J=1,N
  150     SUM=SUM+W(NPT+J)*XPT(K,J)
      ELSE
          IP=PTSID(K)
          IF (IP .GT. 0) SUM=W(NPT+IP)*PTSAUX(1,IP)
          IQ=DFLOAT(NP)*PTSID(K)-DFLOAT(IP*NP)
          IF (IQ .GT. 0) THEN
              IW=1
              IF (IP .EQ. 0) IW=2
              SUM=SUM+W(NPT+IQ)*PTSAUX(IW,IQ)
          END IF
      END IF
  160 W(K)=HALF*SUM*SUM
C
C     Calculate VLAG and BETA for the required updating of the H matrix if
C     XPT(KNEW,.) is reinstated in the set of interpolation points.
C
      DO 180 K=1,NPT
      SUM=ZERO
      DO 170 J=1,N
  170 SUM=SUM+BMAT(K,J)*W(NPT+J)
  180 VLAG(K)=SUM
      BETA=ZERO
      DO 200 J=1,NPTM
      SUM=ZERO
      DO 190 K=1,NPT
  190 SUM=SUM+ZMAT(K,J)*W(K)
      BETA=BETA-SUM*SUM
      DO 200 K=1,NPT
  200 VLAG(K)=VLAG(K)+SUM*ZMAT(K,J)
      BSUM=ZERO
      DISTSQ=ZERO
      DO 230 J=1,N
      SUM=ZERO
      DO 210 K=1,NPT
  210 SUM=SUM+BMAT(K,J)*W(K)
      JP=J+NPT
      BSUM=BSUM+SUM*W(JP)
      DO 220 IP=NPT+1,NDIM
  220 SUM=SUM+BMAT(IP,J)*W(IP)
      BSUM=BSUM+SUM*W(JP)
      VLAG(JP)=SUM
  230 DISTSQ=DISTSQ+XPT(KNEW,J)**2
      BETA=HALF*DISTSQ*DISTSQ+BETA-BSUM
      VLAG(KOPT)=VLAG(KOPT)+ONE
C
C     KOLD is set to the index of the provisional interpolation point that is
C     going to be deleted to make way for the KNEW-th original interpolation
C     point. The choice of KOLD is governed by the avoidance of a small value
C     of the denominator in the updating calculation of UPDATE.
C
      DENOM=ZERO
      VLMXSQ=ZERO
      DO 250 K=1,NPT
      IF (PTSID(K) .NE. ZERO) THEN
          HDIAG=ZERO
          DO 240 J=1,NPTM
  240     HDIAG=HDIAG+ZMAT(K,J)**2
          DEN=BETA*HDIAG+VLAG(K)**2
          IF (DEN .GT. DENOM) THEN
              KOLD=K
              DENOM=DEN
          END IF
      END IF
  250 VLMXSQ=DMAX1(VLMXSQ,VLAG(K)**2)
      IF (DENOM .LE. 1.0D-2*VLMXSQ) THEN
          W(NDIM+KNEW)=-W(NDIM+KNEW)-WINC
          GOTO 120
      END IF
      GOTO 80
C
C     When label 260 is reached, all the final positions of the interpolation
C     points have been chosen although any changes have not been included yet
C     in XPT. Also the final BMAT and ZMAT matrices are complete, but, apart
C     from the shift of XBASE, the updating of the quadratic model remains to
C     be done. The following cycle through the new interpolation points begins
C     by putting the new point in XPT(KPT,.) and by setting PQ(KPT) to zero,
C     except that a RETURN occurs if MAXFUN prohibits another value of F.
C
  260 DO 340 KPT=1,NPT
      IF (PTSID(KPT) .EQ. ZERO) GOTO 340
      IF (NF .GE. MAXFUN) THEN
          NF=-1
          GOTO 350
      END IF
      IH=0
      DO 270 J=1,N
      W(J)=XPT(KPT,J)
      XPT(KPT,J)=ZERO
      do 265 m1=1,mv
  265 v_temp(m1)=PQV(m1,KPT)*W(J)      
      DO 270 I=1,J
      IH=IH+1
      do 270 m1=1,mv
  270 HQV(m1,IH)=HQV(m1,IH)+v_temp(m1)*W(I)      
      do 275 m1=1,mv
  275 PQV(m1,KPT)=ZERO
      IP=PTSID(KPT)
      IQ=DFLOAT(NP)*PTSID(KPT)-DFLOAT(IP*NP)
      IF (IP .GT. 0) THEN
          XP=PTSAUX(1,IP)
          XPT(KPT,IP)=XP
      END IF
      IF (IQ .GT. 0) THEN
          XQ=PTSAUX(1,IQ)
          IF (IP .EQ. 0) XQ=PTSAUX(2,IQ)
          XPT(KPT,IQ)=XQ
      END IF
C
C     Set VQUAD1 to the value of the current model at the new point.
C
      do 276 m1=1,mv
  276 v_vquad(m1)=v_fbase(m1)       
      IF (IP .GT. 0) THEN
          IHP=(IP+IP*IP)/2
          do 277 m1=1,mv
  277     v_vquad(m1)=v_vquad(m1)+XP*(GQV(m1,IP)+HALF*XP*HQV(m1,IHP))
      END IF
      IF (IQ .GT. 0) THEN
          IHQ=(IQ+IQ*IQ)/2
          do 278 m1=1,mv
  278     v_vquad(m1)=v_vquad(m1)+XQ*(GQV(m1,IQ)+HALF*XQ*HQV(m1,IHQ))          
          IF (IP .GT. 0) THEN
              IW=MAX0(IHP,IHQ)-IABS(IP-IQ)
              do 279 m1=1,mv
  279         v_vquad(m1)=v_vquad(m1)+XP*XQ*HQV(m1,IW)              
          END IF
      END IF
      DO 280 K=1,NPT
      TEMP=ZERO
      IF (IP .GT. 0) TEMP=TEMP+XP*XPT(K,IP)
      IF (IQ .GT. 0) TEMP=TEMP+XQ*XPT(K,IQ)
      do 280 m1=1,mv
  280 v_vquad(m1)=v_vquad(m1)+HALF*PQV(m1,K)*TEMP*TEMP
C
C     Calculate F at the new interpolation point, and set DIFF to the factor
C     that is going to multiply the KPT-th Lagrange function when the model
C     is updated to provide interpolation to the new function value.
C
      DO 290 I=1,N
      W(I)=DMIN1(DMAX1(XL(I),XBASE(I)+XPT(KPT,I)),XU(I))
      IF (XPT(KPT,I) .EQ. SL(I)) W(I)=XL(I)
      IF (XPT(KPT,I) .EQ. SU(I)) W(I)=XU(I)
  290 CONTINUE
      NF=NF+1
C
C     dfovec(n, mv, x, v_err) provides the values of the vector function v_err(x): R^n \to R^{mv}.
C     Here: n, mv, x \in R^n are input, v_err \in R^{mv} are output.
      !call dfovec(n, mv, W, v_err)
      call sub_func(n, mv, W, v_err)
C
C     f_value(mv,v_err,F) provides the value of the sum of the squres of the components of v_err(x)
C     i.e. F = sum_{i=1}^{mv} v_err_i (x)^2        
      call f_value(mv,v_err,F)
C      
      IF (IPRINT .EQ. 3) THEN
          PRINT 300, NF,F,(W(I),I=1,N)
  300     FORMAT (/4X,'Function number',I6,'    F =',1PD18.10,
     1      '    The corresponding X is:'/(2X,5D15.6))
      END IF
      FVAL(KPT)=F
      do 305 m1=1,mv
  305 FVAL_V(m1,KPT)=v_err(m1)      
      IF (F .LT. FVAL(KOPT)) KOPT=KPT
      do 308 m1=1,mv
  308 v_diff(m1)=v_err(m1)-v_vquad(m1)       
C
C     Update the quadratic model. The RETURN from the subroutine occurs when
C     all the new interpolation points are included in the model.
C
      DO 310 I=1,N
      do 310 m1=1,mv
  310 GQV(m1,I)=GQV(m1,I)+v_diff(m1)*BMAT(KPT,I)      
      DO 330 K=1,NPT
      SUM=ZERO
      DO 320 J=1,NPTM
  320 SUM=SUM+ZMAT(K,J)*ZMAT(KPT,J)
      do 322 m1=1,mv
  322 v_temp(m1)=v_diff(m1)*SUM      
      IF (PTSID(K) .EQ. ZERO) THEN
          do 324 m1=1,mv
  324     PQV(m1,K)=PQV(m1,K)*v_temp(m1)
      ELSE
          IP=PTSID(K)
          IQ=DFLOAT(NP)*PTSID(K)-DFLOAT(IP*NP)
          IHQ=(IQ*IQ+IQ)/2
          IF (IP .EQ. 0) THEN
              do 326 m1=1,mv
  326         HQV(m1,IHQ)=HQV(m1,IHQ)+v_temp(m1)*PTSAUX(2,IQ)**2    
          ELSE
              IHP=(IP*IP+IP)/2
              HQ(IHP)=HQ(IHP)+TEMP*PTSAUX(1,IP)**2
              IF (IQ .GT. 0) THEN
                IW=MAX0(IHP,IHQ)-IABS(IQ-IP)
                do 328 m1=1,mv
                HQV(m1,IHQ)=HQV(m1,IHQ)+v_temp(m1)*PTSAUX(1,IQ)**2
  328           HQV(m1,IW)=HQV(m1,IW)
     &                     +v_temp(m1)*PTSAUX(1,IP)*PTSAUX(1,IQ)                  
              END IF
          END IF
      END IF
  330 CONTINUE
      PTSID(KPT)=ZERO
  340 CONTINUE
  350 RETURN
      END


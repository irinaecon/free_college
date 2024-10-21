c
c  Test the algorithm DFBOLS using Powell's TRIGSSQS with bound constraints, 
c  in the case mv=20 and n=10,
c  i.e. Min F(x) = \sum_{j=1}^{mv} v_err(j)^2, s.t xl <= x <= xu. x \in R^n,
c       where v_err(j) = BP{j} - \sum_{i=1}^n (SP(j,i)*dsin(x(i))+CP(j,i)*dcos(x(i)))
c       and BP, SP, CP are data. For details, see 
c       M.J.D. Powell (2006), "the NEWUOA software for unconstrained optimization
c       without derivatives", in Large-Scale Optimization, editors G. Di Pillo and
c       M. Roma, Springer (New York), pp. 255-297.
c
      subroutine dfbols(sub_func,n,mv,x_real)
      
      use nrtype
c    
c  nmax: the maximum number of unknowns,  n \le nmax
c  mmax: the maximum number of euqations, mv \le mmax
      integer nmax, nptmax, nspace
      parameter (nmax=100,nptmax=2*nmax+1,
     &     nspace=(nptmax+5)*(nptmax+nmax)+3*nmax*(nmax+5)/2)
      double precision w(nspace)
      integer mmax
      parameter (mmax = 400 ) 
      integer i
      double precision f 
      
      integer nprob
      common /test/ nprob
      integer mv1
      common /NumberEquations/mv1
c
c     n denote the number of unknowns 
c     x gives the starting guess 
c     xl gives the lower bound, xu gives the upper bound
c
      integer n, npt, mv, iprint, maxfun
      double precision x_real(n)
      double precision x(nmax), v_err(mmax), xl(nmax), xu(nmax)
      double precision rhobeg, rhoend, bdl, bdu
      
      ! interface for the function
        interface
            subroutine sub_func(n, mv, x, v_err )
                implicit none
                integer n,mv
                double precision x(n), v_err(mv)
                
               
            end subroutine sub_func
        end interface
c
      iprint = 2
c     maxfun is the maximum number of function evaluation allowed 
c     maxfun is set later to be 400*(n+1)
c     maxfun = 100000
c     rhobeg is the starting trust region radius
      rhobeg = 1.0_dp
c     rhoend is the stopping trust region radius      
      rhoend = 1.0D-8

c
c The subroutine SETUP provides the starting point x, the dimension n
c and the number of equations mv. 
c nprob is the input which specifies the problem number 
c
      nprob = 2 
      
      x(1:n) = x_real(1:n)
     
      mv1 = mv 
c     the solver is allowed to use at most 400 (simplex) gradients
      maxfun =10*(n+1)  !400*(n+1)   
      
c     Set up the lower and upper bound
      bdl =0.d001
      bdu =  10.0_dp
      do i=1,n
         xl(i) = bdl
         xu(i) = bdu
      enddo
      !xu(1) = 0.9_dp
      !xu(2) = 328.0_dp
      !xu(3) = 328.0_dp
      !xu(4) = 328.0_dp
      !xu(5) = 328.0_dp
      !xu(6) = 328.0_dp
      !xu(7) = 328.0_dp
      !xu(8) = 0.9_dp
      !xu(8) = 0.9_dp
      !!xu(1)=0.1_dp
      !!xu(3)=0.99_dp
      !!xu(4)=0.99_dp
      
c     Set up the number of interpolating points:  npt \in [n+2, 2n+1] 
c     Recommended: npt = 2n+1
c
      npt = 2*n+1
      
      PRINT 20, nprob, n, mv, npt
   20 FORMAT (//4X,"Results with NPROB=",I5, ' N =',I5,
     &       ' MV=',I5,' and NPT =',I5) 
      !print*, x
c  The following code bobyqa_h are modifications and based on the Software BOBYQA 
c  It is a Derivative-Free alrotihm for minimizing Box constrainted Least Squares problem.
c  So, we call it DFBOLS.
     
      call bobyqa_h(n,npt,x,xl,xu,rhobeg,rhoend,iprint,maxfun,w,mv, 
     1  sub_func) 
c Calculate the final function value again 
c
        call sub_func(n, mv, x, v_err)
        f = 0.d0
        do i=1,mv
          f = f + v_err(i)**2
        enddo
        print *, "Final function value, f_final=", f
        
        x_real(1:n) = x(1:n)

      end subroutine dfbols

      subroutine CALFUN(n, x, F, sub_func)
      integer n
      double precision x(*), F
      integer mv1, j
      common /NumberEquations/mv1 
      integer mmax
      parameter (mmax = 400 )       
      double precision v_err(mmax)
      
       interface
            subroutine sub_func(n, mv, x, v_err )
                implicit none
                double precision :: x(n), v_err(n)
                integer n,mv
               
            end subroutine sub_func
        end interface
      
      call sub_func(n, mv1, x, v_err)
      F=0.d0
      do j=1,mv1
         F = F + v_err(j)**2
      enddo
      
      end subroutine CALFUN
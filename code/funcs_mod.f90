module funcs_mod
! This module is taken from Fortran routines written by Prof. Alexander Ludwig, Goethe-Univerity Frankfurt
    
use nrtype
use params_mod

implicit none

    contains

! --------------------------------------------------------------------------------------
function makegrid(x1,x2,n,c)

real(dp)::x1,x2,scale,c
integer::n,i
real(dp)::makegrid(n)

scale=x2-x1
makegrid(1)=x1
makegrid(n)=x2
do i=2,n-1
	makegrid(i)=x1+scale*((i-1.0)/(n-1.0))**c
end do

end function makegrid
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function func_intp(xv,fv,x,vals,inds,optextr,optsign)

implicit none

real(dp)::func_intp,fi
real(dp),dimension(:),intent(in)::xv,fv
real(dp),intent(in)::x
logical,intent(in)::optextr,optsign
real(dp),intent(out)::vals(2)
integer,intent(out)::inds(2)
integer::ni

ni=size(fv)
if ( x<=xv(1) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=1
    inds(2)=2
    if (optextr) then
        fi=func_extrapol(xv(2),xv(1),fv(2),fv(1),x,optsign)
    else
        fi=fv(1)
    endif
elseif ( x>=xv(ni) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=ni
    inds(2)=ni-1
    if (optextr) then
        fi=func_extrapol(xv(ni-1),xv(ni),fv(ni-1),fv(ni),x,optsign)
    else
        fi=fv(ni)
    endif
else
    call basefun(xv,ni,x,vals,inds)
    fi=vals(1)*fv(inds(1))+vals(2)*fv(inds(2))
endif

func_intp=fi

end function func_intp
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine basefun (grid_x,npx,x,vals,inds) 
! This subroutine returns the values and the indices of the two basis
! functions that are positive on a given x in the grid_x

implicit none

real(dp),intent(in) :: x
integer , intent(in):: npx
real(dp), intent(in) :: grid_x (npx)
real(dp), intent(out) ::vals(2)
integer ,intent(out) ::inds(2)
integer :: i

call lookup(i,x,grid_x,npx)

vals(2)=( x-grid_x(i-1) )/(grid_x(i)-grid_x(i-1))
vals(1)=( grid_x(i)-x )/(grid_x(i)-grid_x(i-1))
inds(2)=i
inds(1)=i-1

end subroutine basefun
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine lookup(i,x,grid_x,npx)
! lookup position of x in gridx 
! x must be increasing

integer::ju,jl,jm
integer,intent(in)::npx
real(dp),intent(in)::x,grid_x(npx)
integer, intent(out)::i

jl=1      
ju=npx   

do
	if (ju-jl<=1) exit
		
	jm=(ju+jl)/2
	if (x>=grid_x(jm)) then
		jl=jm
	else
		ju=jm
	endif

end do

i=jl+1

end subroutine lookup
! --------------------------------------------------------------------------------------

    
! --------------------------------------------------------------------------------------
function func_extrapol(x1,x2,y1,y2,x,optsign)
! simple linear extrapolation

implicit none

real(dp)::func_extrapol
real(dp),intent(in)::x1,x2,y1,y2,x
logical,intent(in)::optsign
real(dp)::y,m,ln_y1,ln_y2,ln_y
logical::optlin
real(dp),parameter::y_large=1000000000.0

optlin=.true.
if (optsign) then   ! sign preserving interpolation
    optlin=.false.
    if ( sign(1.0,y1)==sign(1.0,y2) ) then
        if (sign(1.0,y1)>0) then
            ln_y1=log(y1)
            ln_y2=log(y2)
        else
            ln_y1=log(-y1)
            ln_y2=log(-y2)
        endif
        m = (ln_y2-ln_y1)/(x2-x1)
        ln_y = ln_y2 + m*(x-x2)
        y = exp(ln_y)
        if (sign(1.0,y1)<0) y=-y
        if (abs(y)>y_large) optlin=.true.
    else
        optlin=.true.
    endif
endif

if (optlin) then
    m = (y2-y1)/(x2-x1)
    y = y2 + m*(x-x2)
endif

func_extrapol=y

end function func_extrapol
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
function func_intp_indlu(xv,fv,x,vals,inds,extrl,extru,opt_err)
! This is a slightly modified version of func_intp which also returns flag
! for extrapolation cases
! 
implicit none

real(dp)::func_intp_indlu,fi
real(dp),dimension(:),intent(in)::xv,fv
real(dp),intent(in)::x
real(dp),intent(out)::vals(2)
integer,intent(out)::inds(2),extrl,extru
integer,intent(in):: opt_err ! 1= account for interpolation error in NEGM, be more generous when punishing extrpolation
integer::ni
real(dp),parameter::epsi=1.0e-06

extrl=0
extru=0

ni=size(fv)
if ( x<=xv(1) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=1
    inds(2)=2
    
    fi=fv(1)
    
    if (opt_err==1)then
        if ( (xv(1)-x)>epsi ) extrl=1
    else
        if ( x<xv(1) ) extrl=1
    endif
        
elseif ( x>=xv(ni) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=ni
    inds(2)=ni-1
   
    fi=fv(ni)
    
    if (opt_err==1)then
        if ( (x-xv(ni))>0.01_dp ) extru=1
    else
        if (x>xv(ni)) extru=1
    endif
        
else
    call basefun(xv,ni,x,vals,inds)
    fi=vals(1)*fv(inds(1))+vals(2)*fv(inds(2))
endif

func_intp_indlu=fi

end function func_intp_indlu
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
function func_intp_ind(xv,fv,x,vals,inds,extr)
! This is a slightly modified version of func_intp which also returns flag
! for extrapolation cases
implicit none

real(dp)::func_intp_ind,fi
real(dp),dimension(:),intent(in)::xv,fv
real(dp),intent(in)::x
real(dp),intent(out)::vals(2)
integer,intent(out)::inds(2),extr
integer::ni

extr=0

ni=size(fv)
if ( x<=xv(1) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=1
    inds(2)=2
    
    fi=fv(1)
    
    if (x<xv(1)) extr=1
    
elseif ( x>=xv(ni) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=ni
    inds(2)=ni-1
   
    fi=fv(ni)
    
    if (x>xv(ni)) extr=1
    
else
    call basefun(xv,ni,x,vals,inds)
    fi=vals(1)*fv(inds(1))+vals(2)*fv(inds(2))
endif

func_intp_ind=fi

end function func_intp_ind
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function func_intp_mod(xv,fv,x,vals,inds)
! This is a slightly modified version of func_intp which assigns large negative number/NaN to function values
! in cases where extrpolation is needed
implicit none

real(dp)::func_intp_mod,fi
real(dp),dimension(:),intent(in)::xv,fv
real(dp),intent(in)::x
real(dp),intent(out)::vals(2)
integer,intent(out)::inds(2)
integer::ni
real(dp),parameter::epsi=1.0e-06_dp
!real(dp):: NaN

!NaN = sqrt(-1.0_dp)

ni=size(fv)
if ( x<=xv(1) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=1
    inds(2)=2
   
    if ( x<xv(1) ) then
        if (abs(x-xv(1))>epsi)then
            
            fi=-infty
        else
            fi=fv(1)
        endif
        
    else
        fi=fv(1)
    endif
   
elseif ( x>=xv(ni) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=ni
    inds(2)=ni-1
   
    if ( x>xv(ni) ) then
        if (abs(x-xv(ni)) >epsi)then
            
            fi=-infty
        else
            fi=fv(ni)
        endif
        
    else
        fi=fv(ni)
    endif
  
else
    call basefun(xv,ni,x,vals,inds)
    fi=vals(1)*fv(inds(1))+vals(2)*fv(inds(2))
endif

func_intp_mod=fi

end function func_intp_mod
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function func_intp_nan(xv,fv,x,vals,inds)
! This is a slightly modified version of func_intp which assigns large negative number/NaN to function values
! in cases where extrpolation is needed
implicit none

real(dp)::func_intp_nan,fi
real(dp),dimension(:),intent(in)::xv,fv
real(dp),intent(in)::x
real(dp),intent(out)::vals(2)
integer,intent(out)::inds(2)
integer::ni
real(dp),parameter::epsi=1.0e-04_dp
!real(dp):: NaN

!NaN = sqrt(-1.0_dp)

ni=size(fv)
if ( x<=xv(1) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=1
    inds(2)=2
   
    if ( x<xv(1) ) then
        if (abs(x-xv(1))>epsi)then
            
            fi=-infty
        else
            fi=fv(1)
        endif
        
    else
        fi=fv(1)
    endif
   
elseif ( x>=xv(ni) ) then
    vals(1)=1.0
    vals(2)=0.0
    inds(1)=ni
    inds(2)=ni-1
   
    if ( x>xv(ni) ) then
        if (abs(x-xv(ni)) >epsi)then
            
            fi=-infty
        else
            fi=fv(ni)
        endif
        
    else
        fi=fv(ni)
    endif
  
else
    call basefun(xv,ni,x,vals,inds)
    fi=vals(1)*fv(inds(1))+vals(2)*fv(inds(2))
endif

func_intp_nan=fi

end function func_intp_nan
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_hyb2d_inter(x,y,grid_x,grid_y,fun_xy)
! hybrid interpolation on rectangular object
! grid_x: endogenous grid, grid_y: exogenous grid
! fun_xy: function to be interpolated
implicit none
real(dp)::f_hyb2d_inter
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:,:),grid_y(:),fun_xy(:,:)
integer::nx,ny,ic,icc
integer::inds_y(2),inds_x(2)
real(dp)::vals_y(2),vals_x(2)
real(dp)::wght  ! interpolation weight
real(dp)::xi,yi
                
ny=size(grid_y,1)
nx=size(grid_x,1)
                
! look up query point in exogenous dimension grid_y:
!call basefun(grid_y,ny,y,vals_y,inds_y) 
yi = func_intp(grid_y,grid_y,y,vals_y,inds_y,.false.,.false.)
!vals_y(1) = 0.0_dp
!vals_y(2) = 1.0_dp

f_hyb2d_inter = 0.0_dp
do ic=1,2
    do icc=1,2        
    ! look up query point in endogenous dimension grid_x, putting on bound in case of bound violation:
    xi = func_intp(grid_x(:,inds_y(ic)),grid_x(:,inds_y(ic)),x,vals_x,inds_x,.false.,.false.)
    
    ! interpolation, given nodes and weights    
    wght = vals_y(ic)*vals_x(icc)
    f_hyb2d_inter = f_hyb2d_inter + wght*fun_xy(inds_x(icc),inds_y(ic)) 
    end do     
end do
                
end function f_hyb2d_inter
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
function f_hyb2d_inter_mod(x,y,grid_x,grid_y,fun_xy,vals_2x,inds_2x,vals_y,inds_y)
! hybrid interpolation on rectangular object
! grid_x: endogenous grid, grid_y: exogenous grid
! fun_xy: function to be interpolated
implicit none
real(dp)::f_hyb2d_inter_mod
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:,:),grid_y(:),fun_xy(:,:)
real(dp),intent(out):: vals_y(2),vals_2x(2,2)
integer,intent(out):: inds_y(2), inds_2x(2,2)
integer::nx,ny,ic,icc
integer::inds_x(2)
real(dp)::vals_x(2)
real(dp)::wght  ! interpolation weight
real(dp)::xi,yi
                
ny=size(grid_y,1)
nx=size(grid_x,1)
                
! look up query point in exogenous dimension grid_y:
!call basefun(grid_y,ny,y,vals_y,inds_y) 
yi = func_intp(grid_y,grid_y,y,vals_y,inds_y,.false.,.false.)
!vals_y(1) = 0.0_dp
!vals_y(2) = 1.0_dp


f_hyb2d_inter_mod = 0.0_dp

do ic=1,2 ! y dimension
    
    ! look up query point in endogenous dimension grid_x, putting on bound in case of bound violation:
    xi = func_intp(grid_x(:,inds_y(ic)),grid_x(:,inds_y(ic)),x,vals_x,inds_x,.false.,.false.)
    
    inds_2x(:,ic)=inds_x
    vals_2x(:,ic)=vals_x
    
    do icc=1,2        
    
        ! interpolation, given nodes and weights    
        wght = vals_y(ic)*vals_x(icc)
    
        !if (x < grid_x(1,inds_y(ic)) )then
        !
        !    f_hyb2d_inter_mod = f_hyb2d_inter_mod - 90000.0_dp
        !
        !else
            
            f_hyb2d_inter_mod = f_hyb2d_inter_mod + wght*fun_xy(inds_x(icc),inds_y(ic)) 
    
        !endif
    
        
        if (isnan(f_hyb2d_inter_mod))then
            print*, vals_y,inds_y,grid_y,y
            pause
        endif
    
    end do     
end do
                
end function f_hyb2d_inter_mod
! --------------------------------------------------------------------------------------

!! --------------------------------------------------------------------------------------
!function f_hyb2d_inter(x,y,grid_x,grid_y,fun_xy)
!! hybrid interpolation on rectangular object
!! grid_x: endogenous grid, grid_y: exogenous grid
!! fun_xy: function to be interpolated
!
!USE CSHER_INT
!USE UMACH_INT
!USE CSVAL_INT
!USE CSCON_INT
!
!implicit none
!real(dp)::f_hyb2d_inter
!real(dp),intent(in)::x,y
!real(dp),intent(in)::grid_x(:,:),grid_y(:),fun_xy(:,:)
!real(dp):: vals_y(2),vals_2x(2,2)
!integer:: inds_y(2), inds_2x(2,2)
!integer::nx,ny,icc,IBREAK    ,yc
!integer::inds_x(2) , ID(size(grid_y))
!real(dp)::vals_x(2)
!real(dp)::wght  ! interpolation weight
!real(dp)::xi,yi
!real(dp):: break_vec(2*size(grid_y)), cscoeff(4,2*size(grid_y)),f_hyb2d_inter_mod_y(size(grid_y)), XSRT(size(grid_y)),FSRT(size(grid_y)),A(size(grid_y)),YAR(size(grid_y)),DIVD(size(grid_y)),WK(5*(size(grid_y)-2))
!                
!ny=size(grid_y,1)
!nx=size(grid_x,1)
!                
!! look up query point in exogenous dimension grid_y:
!!call basefun(grid_y,ny,y,vals_y,inds_y) 
!yi = func_intp(grid_y,grid_y,y,vals_y,inds_y,.false.,.false.)
!vals_y(1) = 0.0_dp
!vals_y(2) = 1.0_dp
!
!f_hyb2d_inter = 0.0_dp
!f_hyb2d_inter_mod_y = 0.0_dp
!
!do yc=1,size(grid_y) ! y dimension
!    
!    ! look up query point in endogenous dimension grid_x, putting on bound in case of bound violation:
!    xi = func_intp(grid_x(:,yc),grid_x(:,yc),x,vals_x,inds_x,.false.,.false.)
!    
!    !inds_2x(:,yc)=inds_x
!    !vals_2x(:,yc)=vals_x
!    
!    do icc=1,2        
!    
!        ! interpolation, given nodes and weights    
!        wght = vals_x(icc)
!    
!        !if (x < grid_x(1,inds_y(ic)) )then
!        !
!        !    f_hyb2d_inter_mod = f_hyb2d_inter_mod - 90000.0_dp
!        !
!        !else
!            
!            f_hyb2d_inter_mod_y(yc) = f_hyb2d_inter_mod_y(yc) + wght*fun_xy(inds_x(icc),yc) 
!    
!        !endif
!    
!        
!        if (isnan(f_hyb2d_inter_mod_y(yc)))then
!            print*, vals_y,inds_y,grid_y,y
!            pause
!        endif
!    
!    end do     
!end do
!
!!CALL d_cscon(grid_y, f_hyb2d_inter_mod_y, IBREAK, break_vec, cscoeff)          
!!call DC2CON (size(grid_y), grid_y,f_hyb2d_inter_mod_y, IBREAK, break_vec, cscoeff, 150, XSRT, FSRT, A, YAR, DIVD, ID, WK)
!
!!f_hyb2d_inter_mod =func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.) ! d_csval(y,break_vec,cscoeff) 
!!f_hyb2d_inter =d_csval(y,break_vec,cscoeff) 
!
!!if (abs(f_hyb2d_inter - func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.)  ) > 0.1_dp )then
!!    f_hyb2d_inter=func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.)
!!endif
!
!!print*, f_hyb2d_inter, func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.) 
!!pause
!
!                
!end function f_hyb2d_inter
!! --------------------------------------------------------------------------------------
!
!! --------------------------------------------------------------------------------------
!function f_hyb2d_inter_mod(x,y,grid_x,grid_y,fun_xy,vals_2x,inds_2x,vals_y,inds_y)
!! hybrid interpolation on rectangular object
!! grid_x: endogenous grid, grid_y: exogenous grid
!! fun_xy: function to be interpolated
!
!USE CSHER_INT
!USE UMACH_INT
!USE CSVAL_INT
!USE CSCON_INT
!
!implicit none
!real(dp)::f_hyb2d_inter_mod
!real(dp),intent(in)::x,y
!real(dp),intent(in)::grid_x(:,:),grid_y(:),fun_xy(:,:)
!real(dp),intent(out):: vals_y(2),vals_2x(2,2)
!integer,intent(out):: inds_y(2), inds_2x(2,2)
!integer::nx,ny,icc,IBREAK    ,yc
!integer::inds_x(2)
!real(dp)::vals_x(2)
!real(dp)::wght  ! interpolation weight
!real(dp)::xi,yi
!real(dp):: break_vec(2*size(grid_y)), cscoeff(4,2*size(grid_y)),f_hyb2d_inter_mod_y(size(grid_y))
!                
!ny=size(grid_y,1)
!nx=size(grid_x,1)
!                
!! look up query point in exogenous dimension grid_y:
!!call basefun(grid_y,ny,y,vals_y,inds_y) 
!yi = func_intp(grid_y,grid_y,y,vals_y,inds_y,.false.,.false.)
!vals_y(1) = 0.0_dp
!vals_y(2) = 1.0_dp
!
!f_hyb2d_inter_mod = 0.0_dp
!f_hyb2d_inter_mod_y = 0.0_dp
!
!do yc=1,size(grid_y) ! y dimension
!    
!    ! look up query point in endogenous dimension grid_x, putting on bound in case of bound violation:
!    xi = func_intp(grid_x(:,yc),grid_x(:,yc),x,vals_x,inds_x,.false.,.false.)
!    
!    !inds_2x(:,yc)=inds_x
!    !vals_2x(:,yc)=vals_x
!    
!    do icc=1,2        
!    
!        ! interpolation, given nodes and weights    
!        wght = vals_x(icc)
!    
!        !if (x < grid_x(1,inds_y(ic)) )then
!        !
!        !    f_hyb2d_inter_mod = f_hyb2d_inter_mod - 90000.0_dp
!        !
!        !else
!            
!            f_hyb2d_inter_mod_y(yc) = f_hyb2d_inter_mod_y(yc) + wght*fun_xy(inds_x(icc),yc) 
!    
!        !endif
!    
!        
!        if (isnan(f_hyb2d_inter_mod_y(yc)))then
!            print*, vals_y,inds_y,grid_y,y
!            pause
!        endif
!    
!    end do     
!end do
!
!!CALL d_cscon(grid_y, f_hyb2d_inter_mod_y, IBREAK, break_vec, cscoeff)                           
!
!f_hyb2d_inter_mod =func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.) ! d_csval(y,break_vec,cscoeff) 
!!f_hyb2d_inter_mod =d_csval(y,break_vec,cscoeff) 
!!
!!
!!if (isnan(f_hyb2d_inter_mod))then
!!            print*, "how", y,f_hyb2d_inter_mod_y, break_vec, cscoeff
!!            pause
!!endif
!!
!!if (abs(f_hyb2d_inter_mod - func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.)  ) > 0.1_dp )then
!!    f_hyb2d_inter_mod=func_intp(grid_y,f_hyb2d_inter_mod_y,y,vals_y,inds_y,.false.,.false.)
!!endif
!                
!end function f_hyb2d_inter_mod
!! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_hyb2d_triang(x,y,grid_x,grid_y,fun_xy)
! hybrid interpolation on triangular object
! grid_x: endogenous grid, grid_y: exogenous grid
! fun_xy: function to be interpolated
implicit none
real(dp)::f_hyb2d_triang
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:,:),grid_y(:),fun_xy(:,:)
integer::nx,ny
integer::inds_y(2),inds_x2(2),inds_x1(2),kick_out
real(dp)::vals_y(2),vals_x2(2),vals_x1(2)
real(dp)::xi1,xi2,wght(4)
                
ny=size(grid_y,1)
nx=size(grid_x,1)
                
! look up query point in exogenous dimension grid_y:
call basefun(grid_y,ny,y,vals_y,inds_y) 

! look up query point in endogenous dimension grid_x, putting on bound in case of bound violation:
xi1 = func_intp(grid_x(:,inds_y(1)),grid_x(:,inds_y(1)),x,vals_x1,inds_x1,.false.,.false.)
xi2 = func_intp(grid_x(:,inds_y(2)),grid_x(:,inds_y(2)),x,vals_x2,inds_x2,.false.,.false.)

! choose three interpolation nodes
wght(1)=vals_x1(1)*vals_y(1)
wght(2)=vals_x2(1)*vals_y(2)
wght(3)=vals_x2(2)*vals_y(2)
wght(4)=vals_x1(2)*vals_y(1)
kick_out=minloc(wght,1)

! interpolation, given nodes and weights 
if (kick_out==1)then ! use vertices 2,3,4
    f_hyb2d_triang = vals_x2(1)*fun_xy(inds_x2(1),inds_y(2)) + (1.0_dp-vals_x2(1)-vals_y(1))*fun_xy(inds_x2(2),inds_y(2)) + vals_y(1)*fun_xy(inds_x1(2),inds_y(1))
elseif (kick_out==2)then ! use vertices 1,3,4
    f_hyb2d_triang = vals_x1(1)*fun_xy(inds_x1(1),inds_y(1)) + (1.0_dp-vals_x1(1)-vals_y(2))*fun_xy(inds_x1(2),inds_y(1)) + vals_y(2)*fun_xy(inds_x2(2),inds_y(2))
elseif (kick_out==3)then ! use vertices 1,2,4
    f_hyb2d_triang = vals_y(2)*fun_xy(inds_x2(1),inds_y(2)) + (1.0_dp-vals_y(2)-vals_x1(2))*fun_xy(inds_x1(1),inds_y(1)) + vals_x1(2)*fun_xy(inds_x1(2),inds_y(1))
else ! use vertices 1,2,3
    f_hyb2d_triang = vals_y(1)*fun_xy(inds_x1(1),inds_y(1))  + (1.0_dp-vals_y(1)-vals_x2(2))*fun_xy(inds_x2(1),inds_y(2)) + vals_x2(2)*fun_xy(inds_x2(2),inds_y(2))
endif
  
end function f_hyb2d_triang
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_reg2d_inter(x,y,grid_x,grid_y,fun_xy)
! bilinear interpolation on rectangular object
! grid_x, grid_y: exogenous regular grids
! fun_xy: function to be interpolated
implicit none
real(dp)::f_reg2d_inter
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:),grid_y(:),fun_xy(:,:)
integer::nx,ny,xc
integer::inds_y(2),inds_x(2)
real(dp)::vals_y(2),vals_x(2)
!real(dp)::wght  ! interpolation weight
real(dp)::fun_x(size(grid_x,1))
               
ny=size(grid_y,1)
nx=size(grid_x,1)

! interpolate in h dimension ATTENTION
do xc=1,nx
    fun_x(xc)=func_intp(grid_y,fun_xy(xc,:),y,vals_y,inds_y,.false.,.false.)
enddo

! interpolate in x dimension
f_reg2d_inter=func_intp(grid_x,fun_x,x,vals_x,inds_x,.false.,.false.)

end function f_reg2d_inter
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
function f_reg2d_inter_mod2(x,y,grid_x,grid_y,fun_xy)
! bilinear interpolation on rectangular object
! grid_x, grid_y: exogenous regular grids
! fun_xy: function to be interpolated
implicit none
real(dp)::f_reg2d_inter_mod2
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:),grid_y(:),fun_xy(:,:)
integer::nx,ny,xc
integer::inds_y(2),inds_x(2)
real(dp)::vals_y(2),vals_x(2)
!real(dp)::wght  ! interpolation weight
real(dp)::fun_x(size(grid_x,1))
               
ny=size(grid_y,1)
nx=size(grid_x,1)

! interpolate in h dimension ATTENTION
do xc=1,nx
    fun_x(xc)=func_intp(grid_y,fun_xy(xc,:),y,vals_y,inds_y,.false.,.false.)
enddo

! interpolate in x dimension
f_reg2d_inter_mod2=func_intp_mod(grid_x,fun_x,x,vals_x,inds_x)

end function f_reg2d_inter_mod2
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_reg2d_inter_mod(x,y,grid_x,grid_y,fun_xy,vals_x,inds_x,vals_y,inds_y)
! bilinear interpolation on rectangular object
! grid_x, grid_y: exogenous regular grids
! fun_xy: function to be interpolated
implicit none
real(dp)::f_reg2d_inter_mod
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:),grid_y(:),fun_xy(:,:)
integer::nx,ny,xc
integer,intent(out)::inds_y(2),inds_x(2)
real(dp),intent(out)::vals_y(2),vals_x(2)
!real(dp)::wght  ! interpolation weight
real(dp)::fun_x(size(grid_x,1))
               
ny=size(grid_y,1)
nx=size(grid_x,1)

! interpolate in h dimension ATTENTION
do xc=1,nx
    fun_x(xc)=func_intp(grid_y,fun_xy(xc,:),y,vals_y,inds_y,.false.,.false.)
enddo

! interpolate in x dimension
f_reg2d_inter_mod=func_intp(grid_x,fun_x,x,vals_x,inds_x,.false.,.false.)

end function f_reg2d_inter_mod
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_reg2d_triang(x,y,grid_x,grid_y,fun_xy)
! 2d linear interpolation on triangular object
! grid_x, grid_y: exogenous regular grids
! fun_xy: function to be interpolated
implicit none
real(dp)::f_reg2d_triang
real(dp),intent(in)::x,y
real(dp),intent(in)::grid_x(:),grid_y(:),fun_xy(:,:)
integer::nx,ny !,ic
integer::inds_y(2),inds_x(2)
real(dp)::vals_y(2),vals_x(2)
real(dp)::wght(4)  ! interpolation weight
real(dp)::xi
integer:: kick_out
                
ny=size(grid_y,1)
nx=size(grid_x,1)
                
! look up query point in dimension grid_y:
call basefun(grid_y,ny,y,vals_y,inds_y)

! look up query point in dimension grid_x, putting on bound in case of bound violation:
xi = func_intp(grid_x,grid_x,x,vals_x,inds_x,.false.,.false.)

! choose three interpolation nodes
wght(1)=vals_x(1)*vals_y(1)
wght(2)=vals_x(1)*vals_y(2)
wght(3)=vals_x(2)*vals_y(2)
wght(4)=vals_x(2)*vals_y(1)
kick_out=minloc(wght,1)

! interpolation, given nodes and weights 
if (kick_out==1)then ! use vertices 2,3,4
    f_reg2d_triang = vals_x(1)*fun_xy(inds_x(1),inds_y(2)) + (1.0_dp-vals_x(1)-vals_y(1))*fun_xy(inds_x(2),inds_y(2)) + vals_y(1)*fun_xy(inds_x(2),inds_y(1))
elseif (kick_out==2)then ! use vertices 1,3,4
    f_reg2d_triang = vals_x(1)*fun_xy(inds_x(1),inds_y(1)) + (1.0_dp-vals_x(1)-vals_y(2))*fun_xy(inds_x(2),inds_y(1)) + vals_y(2)*fun_xy(inds_x(2),inds_y(2))
elseif (kick_out==3)then ! use vertices 1,2,4
    f_reg2d_triang = vals_y(2)*fun_xy(inds_x(1),inds_y(2)) + (1.0_dp-vals_y(2)-vals_x(2))*fun_xy(inds_x(1),inds_y(1)) + vals_x(2)*fun_xy(inds_x(2),inds_y(1))
else ! use vertices 1,2,3
    f_reg2d_triang = vals_y(1)*fun_xy(inds_x(1),inds_y(1))  + (1.0_dp-vals_y(1)-vals_x(2))*fun_xy(inds_x(1),inds_y(2)) + vals_x(2)*fun_xy(inds_x(2),inds_y(2))
endif
                
end function f_reg2d_triang
! --------------------------------------------------------------------------------------




end module funcs_mod
    




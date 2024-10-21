module inst_mod
! This module contains subroutines and functions
! related to institutational structure: tax system, unemployment benefits, pension, and not only: organize better LATER
    
use nrtype
use params_mod
use types_mod
use funcs_mod
use dc_egm_mod
use solvers_mod
use omp_lib    
use rlse_int
    
implicit none

    contains

! ------------------------------------------------------------------- 
subroutine sub_norm_ageprod(ap,ap_dat,wage_s,ret) 
! normalization of age productivity profiles so that model meets wage premium in data

implicit none
real(dp),intent(in)::ap_dat(:,:),wage_s(:),ret
real(dp),intent(inout)::ap(:,:)
real(dp)::wage_prem_dat,mpl
integer::sc,jc
real(dp),parameter::epsi=1.0e-08

! compute MPL from rate of return (not needed if mpl normalization is applied in prod function)
!mpl=(1.0_dp-alph)*(alph/(ret+delt))**(alph/(1.0_dp-alph))
mpl = lvl_ref

do sc=1,ns
    
    wage_prem_dat=ap_dat(js,sc)/ap_dat(js,1)             ! wage premium according to data
       
    ! normalization of age productivity such that (ap(sc)*w(sc))/(ap(nsc)*w(nsc))=wage_prem_dat
    ap(js,sc)=mpl/(wage_s(sc))*wage_prem_dat  
    
    ap(:,sc) =ap(js,sc) *  ap_dat(:,sc) / ap_dat(js,sc)
        
end do

     
end subroutine sub_norm_ageprod
! -------------------------------------------------------------------     
    
! -------------------------------------------------------------------------------
subroutine sub_wage(gr_wage,net_wage,wage_totax,agg_wage,ap,shk,h,tau_p,sc,kc,dc,ec,jc,gen_id,gammah,abgrad,cutoff) 
! function returning the age-specific gross and net_wage of the household
implicit none

real(dp),intent(out)::gr_wage,net_wage,wage_totax
real(dp),intent(in)::agg_wage,ap,shk,tau_p,h,gammah,abgrad(:),cutoff(:)
integer,intent(in):: kc,dc,ec,jc,gen_id,sc

if (gen_id<3)then
    
    gr_wage=agg_wage*ap  * shk
    
   ! if (jc>=jstud(sc))then
        !gr_wage = gr_wage * (1.0_dp - wght_abgrad) + gr_wage * exp(gammah + abgrad(2)*log(h/hk_str )) * wght_abgrad
        gr_wage = gr_wage * (1.0_dp - wght_abgrad) + gr_wage * gammafe(sc,kc) * wght_abgrad
        !if (kc==1)then
        !    gr_wage = gr_wage * (1.0_dp - wght_abgrad) + gr_wage * exp(gammah + abgrad(2)*log(0.2_dp/hk_str )) * wght_abgrad
        !else
        !    gr_wage = gr_wage * (1.0_dp - wght_abgrad) + gr_wage * exp(gammah + abgrad(2)*log(1.2_dp/hk_str )) * wght_abgrad
        !endif
        
   ! endif
        
    net_wage=gr_wage*(1.0_dp-tau_p) ! recall in the moment tau_h is in params
    wage_totax=gr_wage*(1.0_dp-0.5_dp*tau_p) 
    
    !gr_wage=agg_wage*ap*shk*(h)*ab_grad(sc) * norm_param(sc)    ! ap: age productivity, shk: income shock
    !net_wage=gr_wage*(1.0_dp-tau_p) ! recall in the moment tau_h is in params
    !wage_totax=gr_wage*(1.0_dp-0.5_dp*tau_p) 
else

!    gr_wage=agg_wage*ap*shk     ! ap: age productivity, shk: income shock
!    net_wage=gr_wage*(1.0_dp-tau_p) ! recall in the moment tau_h is in params
!    wage_totax=gr_wage*(1.0_dp-0.5_dp*tau_p) 
!endif !else

    gr_wage=agg_wage*ap*shk     ! ap: age productivity, shk: income shock
    net_wage=gr_wage*(1.0_dp-tau_p) ! recall in the moment tau_h is in params
    wage_totax=gr_wage*(1.0_dp-0.5_dp*tau_p) 
endif
net_wage = max(0.0_dp,net_wage)

if (wage_totax<0.0_dp)then 
    print*, gr_wage,agg_wage,ap ,shk,gammafe(sc,kc), wght_abgrad
    pause
endif


end subroutine sub_wage 
! -------------------------------------------------------------------------------

! ---------------------------------------------------------------------------------------------
function f_tau_c(cons,gconst,expend,labrv,pensrv,caprv)
! computes update of consumption tax rate
implicit none

real(dp)::f_tau_c
real(dp),intent(in)::cons,gconst,expend,labrv,pensrv,caprv ! agg consumption, G (exo),E,labor,pension, capital inc tax revenues
real(dp):: allrv,netexp

! all tax revenues
allrv = labrv+pensrv+caprv

! net (of other revenues) gov expenditures
netexp = gconst + expend - allrv

! update tauc
f_tau_c=f_tau(cons,netexp)

end function f_tau_c
! ---------------------------------------------------------------------------------------------


! ---------------------------------------------------------------------------------------------
function f_tau(inc,ben)
! computes update of unemployment insurance contribution rate
implicit none

real(dp)::f_tau
real(dp),intent(in)::inc,ben

f_tau=ben/inc

end function f_tau
! ---------------------------------------------------------------------------------------------

! ---------------------------------------------------------------------------------------------
function f_rho(pcontr,benp,rhoold)
! computes update of unemployment insurance contribution rate
implicit none

real(dp)::f_rho
real(dp),intent(in)::pcontr,benp,rhoold
real(dp):: pind

! aggregate individual pension component
pind = benp/rhoold
! new rho
f_rho = pcontr/pind

!print*, pcontr,pind,benp
!pause

end function f_rho
! ---------------------------------------------------------------------------------------------

! ---------------------------------------------------------------
function fun_averwage(earn,hrs)
! compute average wage given aggregate earnings and aggregate hours
implicit none
real(dp):: fun_averwage
real(dp),intent(in):: earn,hrs

fun_averwage = earn/hrs

end function fun_averwage
! ---------------------------------------------------------------

! ---------------------------------------------------------------
function f_averearn(earn,wrkpop)
! compute average earnings given aggregate earnings and all working population (end outcome)
implicit none
real(dp):: f_averearn
real(dp),intent(in):: earn,wrkpop

f_averearn = earn/wrkpop

end function f_averearn
! ---------------------------------------------------------------
 

    
! ---------------------------------------------------------------- 
function f_capint(ret)
! compute capital intensity, given rate of return
implicit none
real(dp),intent(in):: ret
real(dp):: f_capint

f_capint = ((ret+delt)/alph)**(1.0_dp/(alph-1.0_dp))    

end function f_capint
!---------------------------------------------------------------- 

! ---------------------------------------------------------------- 
function f_netcons(cons,tau_c)
! compute consumption net of cons tax, given gross consumption
implicit none
real(dp),intent(in):: cons,tau_c
real(dp):: f_netcons

f_netcons = cons/(1.0_dp+tau_c)    

end function f_netcons
!---------------------------------------------------------------- 

! ---------------------------------------------------------------- 
function f_captax(ass,R,tau_k)
! compute amount of capital income tax, given assets and rate of return
implicit none
real(dp),intent(in):: ass,R,tau_k
real(dp):: f_captax

f_captax = ass*(R-1.0_dp)*(tau_k)  

end function f_captax
!----------------------------------------------------------------

! ---------------------------------------------------------------- 
function f_aftertaxR(R,tau_k)
! compute rate of return net of capital income tax
implicit none
real(dp),intent(in):: R,tau_k
real(dp):: f_aftertaxR

f_aftertaxR =(R-1.0_dp)*(1.0_dp-tau_k)+1.0_dp

!print*, f_aftertaxR, R, tau_k
!pause

end function f_aftertaxR
!----------------------------------------------------------------

! ---------------------------------------------------------------- 
function f_aftertaxRnet(R,tau_k)
! compute rate of return net of capital income tax
implicit none
real(dp),intent(in):: R,tau_k
real(dp):: f_aftertaxRnet

f_aftertaxRnet =(R-1.0_dp)*(1.0_dp-tau_k) !+1.0_dp

!print*, f_aftertaxR, R, tau_k
!pause

end function f_aftertaxRnet
!----------------------------------------------------------------

! --------------------------------------------------------------------------------------
function fun_hsv_pens(p,p1,p2,mc,gc,ec,dc,ic,jc,tc,avearn)
! Tax function: HSV (2-param)

implicit none
real(dp)::fun_hsv_pens
real(dp),intent(in):: p,p1,p2,avearn
integer,intent(in):: mc,gc,ec,dc,jc,tc,ic
real(dp):: lp,tbas,netp,netp1,netp2,netp_cpl
real(dp):: min_pens

min_pens = 0.0_dp

if (mc==1)then ! couple
    tbas = (p1+p2)*0.5_dp / avearn
    fun_hsv_pens = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr) 
    fun_hsv_pens = fun_hsv_pens  * avearn
    netp1 = p1 - fun_hsv_pens
    netp2 = p2 - fun_hsv_pens
    fun_hsv_pens = fun_hsv_pens *2.0_dp
    netp_cpl = netp1+netp2-fun_hsv_pens
   
   
else ! single
    tbas = p ! avearn
    fun_hsv_pens = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr) !* avearn**tau_pr
    fun_hsv_pens =0.0_dp ! fun_hsv_pens  
    netp = p !-  fun_hsv_pens
   
endif
     
end function fun_hsv_pens
! --------------------------------------------------------------------------------------
    



! -------------------------------------------------------------------------------
function fun_netpens(grben_p,grben_p1,grben_p2,mc,gc,ec,dc,ic,jc,tc,avearn)
! compute pension income, net of tax

implicit none
real(dp):: fun_netpens
real(dp),intent(in):: grben_p,grben_p1,grben_p2,avearn
integer,intent(in):: mc,gc,ec,dc,jc,tc,ic

if (mc==1)then ! couple
    fun_netpens = grben_p1 + grben_p2  - fun_hsv_pens(0.0_dp,grben_p1,grben_p2,mc,gc,ec,dc,ic,jc,tc,avearn)
else ! single
    fun_netpens = grben_p  - fun_hsv_pens(grben_p,0.0_dp,0.0_dp,mc,gc,ec,dc,ic,jc,tc,avearn)
endif


end function fun_netpens
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_pens_new(gr_ben_p,varrho,taup,tauh,agg_wage,pia,prodref,shk_inc,afac,jc,gc,dc,ec,tc,sc,kc,ycm,nd_in)
! computes pension income
implicit none
real(dp),intent(in)::varrho,taup,agg_wage,shk_inc,afac,prodref,tauh,pia
integer,intent(in):: jc,dc,tc,sc,kc,ycm,gc,ec,nd_in
real(dp),intent(out)::gr_ben_p
real(dp)::temp

! pension payments
!temp=(1.0_dp-taup-afac)*agg_wage*shk_inc *prodref

temp=(1.0_dp-taup)*agg_wage * pia !  *prodref

gr_ben_p = varrho  * temp 

gr_ben_p = max(0.0_dp,gr_ben_p)

if (jc<jr(tc))then ! retirement window
     
    if (opt_test_noer==0)then ! end retirement => retirement window
    
        if (ec==1)then ! chose in this period
            if (jc<jrr) then ! early
                gr_ben_p= er_pen * gr_ben_p
            else ! regular
                gr_ben_p = gr_ben_p
            endif
        else ! in ret state
            if (jc<jrr)then ! early
                gr_ben_p = er_pen * gr_ben_p
                if (dc/=nd_in-1)then
                    print*, "early ret is always nd-1!"
                    pause
                endif
            else ! reg and early
                if (dc==nd_in-1)then ! early
                    gr_ben_p = er_pen * gr_ben_p
                else !if (dc==nd)then ! reg                
                    gr_ben_p =gr_ben_p
                !else
                !    print*, "here only early and reg!", dc,nd
                !    pause
                endif
                       
            endif
        endif
    else
        !print*, "with no end ret should not be here!",dc,ec,jc
        !pause
    endif
   
else ! after mandatory retirement age
    
    if (opt_test_noer==0)then
    
        if (dc==nd_in)then
            gr_ben_p = gr_ben_p !0.0_dp ! (1.0_dp-er_pen)*ben_p
        elseif (dc==nd_in-1)then ! early
            gr_ben_p = er_pen * gr_ben_p
        elseif (dc==nd_in-2)then ! late
            gr_ben_p = lat_rew * gr_ben_p
        endif
        
    else
        gr_ben_p = gr_ben_p
        if (dc/=nd_in)then
            print*, "ret index went wrong",jc,nj,dc,nd_in,nd_cpl
            pause
        endif
    endif
            
endif


end subroutine sub_pens_new
! -------------------------------------------------------------------------------
    

! --------------------------------------------------------------------------------------
! pension payments 
subroutine sub_pia(agg,demo,grid,t0,t1) 

implicit none
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
integer,intent(in)::t0,t1
real(dp),dimension(ng,nk,ns)::aiyw        ! average indexed yearly wages
real(dp)::mpia
integer::sc,kc,jc,tc,tcc,nt,gc
real(dp):: gr_wage,net_wage
real(dp),parameter:: min_inc_fac=0.03182594_dp

! average indexed yearly wages
nt=size(agg%pia0,4)
do tc=t0,t1      ! retirement in year tc (first year in retirement in year tc+1)
    
    aiyw=0.0
    do jc=jstud(3),jr(tc)-1     ! jr-1 is last age of work, start in js to avoid anything going on in college
        do gc=1,ng
            do sc=1,ns
                do kc=1,nk
                    if ( (t0==nt) .and. (t1==nt) ) then     ! steady state
                        tcc=nt
                    else
                        tcc=tc-((jr(tc)-1)-jc)
                        tcc=max(1,tcc)
                    endif
                    aiyw(gc,kc,sc)=aiyw(gc,kc,sc)+agg%wage_s(sc,tc)*demo%ageprod(gc,jc,sc) ! TBC fixed effect, if have them 
                end do
            end do
        enddo
    end do
    aiyw(:,:,:)=aiyw(:,:,:)/sum(aiyw(:,:,:))
    
    ! primary insurance amount
    do gc=1,ng
        do sc=1,ns
            do kc=1,nk
                if ( aiyw(gc,kc,sc)<bp(1) ) then
                    agg%pia0(gc,kc,sc,tc)=slp_bp(1)*aiyw(gc,kc,sc)
                else
                    if ( aiyw(gc,kc,sc)<bp(2) ) then
                        agg%pia0(gc,kc,sc,tc)=slp_bp(1)*bp(1)+slp_bp(2)*(aiyw(gc,kc,sc)-bp(1))
                    else
                        if ( aiyw(gc,kc,sc)<bp(3) ) then
                            agg%pia0(gc,kc,sc,tc)=slp_bp(1)*bp(1)+slp_bp(2)*(bp(2)-bp(1))+slp_bp(3)*(aiyw(gc,kc,sc)-bp(2))
                        else
                            agg%pia0(gc,kc,sc,tc)=slp_bp(1)*bp(1)+slp_bp(2)*(bp(2)-bp(1))+slp_bp(3)*(bp(3)-bp(2))
                        endif
                    endif
                endif
                
                agg%pia0(gc,kc,sc,tc) = max(agg%pia0(gc,kc,sc,tc) , min_inc_fac * bp(1) )
                
            end do  ! end do kc
        end do  ! end do sc
    enddo
    
    ! centralize (so that rhops becomes an interpretable number):
    mpia=sum(agg%pia0(:,:,:,tc))/(ng*ns*nk)
    agg%pia0(:,:,:,tc)=agg%pia0(:,:,:,tc)/mpia
end do  ! end do tc

! superimpose steady state
if ( (t0==nt) .and. (t1==nt) .and. (nt>1) ) then     ! steady state
    do tc=2,nt
        agg%pia0(:,:,:,tc)=agg%pia0(:,:,:,nt) 
    end do
endif

!do tc = 2,nt
!    agg%pia0(:,:,:,tc)=agg%pia0(:,:,:,1)
!enddo

 
end subroutine sub_pia
! --------------------------------------------------------------------------------------

!! --------------------------------------------------------------------------------------
!subroutine fun_hsv_new_thrs(yroot,y1,y2,mc,gc,ec,dc,ic,jc,tc,numchld,avearn,avearn_base,ind_below)
!
!use toolbox,only: fzero
!! Tax function: HSV (2-param)
!
!implicit none
!!real(dp)::fun_hsv_new
!real(dp),intent(in)::y1,y2,numchld,avearn,avearn_base
!real(dp),intent(out):: yroot
!integer,intent(in):: mc,gc,ec,dc,jc,tc,ic,ind_below ! ind_ass = 1 receive SA, 0 not
!real(dp):: lp,tbas, tau_loc
!real(dp):: n=0.0_dp ! number of children
!logical:: check_return
!
!yroot = 0.3_dp
!call fzero(yroot,fun_hsv_new_breakeven,check_return)
!
!
!contains
!
!! --------------------------------------------------------------------------------------
!function fun_hsv_new_breakeven(y)
!! Tax function: HSV (2-param)
!
!implicit none
!real(dp)::fun_hsv_new_breakeven
!real(dp),intent(in):: y
!real(dp):: lp,tbas, tau_loc
!real(dp):: n=0.0_dp ! number of children
!
!fun_hsv_new_breakeven = fun_hsv_new(y,y1,y2,mc,gc,ec,dc,ic,jc,tc,numchld,avearn,ind_below)
!
!end function fun_hsv_new_breakeven
!! --------------------------------------------------------------------------------------
!
!end subroutine fun_hsv_new_thrs
!! --------------------------------------------------------------------------------------



! --------------------------------------------------------------------------------------
function fun_hsv_new(y,y1,y2,lab,lab1,lab2,mc,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,indbelow)
! Tax function: HSV (2-param)

implicit none
real(dp)::fun_hsv_new
real(dp),intent(in):: y,y1,y2,numchld,avearn,lab,lab1,lab2
integer,intent(in):: mc,gc,ec,dc,jc,tc,ic ,sc,indbelow! ind_ass = 1 receive SA, 0 not
real(dp):: lp,tbas, tau_loc
real(dp):: n=0.0_dp ! number of children

n = 0.0_dp
if (mc==1)then
    n = numchld
else
    if (gc==1)then
        n = 0.0_dp
    else
        n = numchld
    endif
endif

if (mc==1)then ! couple
   
    if (lab1 > 0.0_dp .and. lab2> 0.0_dp)then
    
    tbas = (y1+y2)*0.5_dp  !/ avearn
    fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr) !* lvl_ref**tau_pr
    fun_hsv_new =fun_hsv_new !* avearn
    fun_hsv_new = fun_hsv_new *2.0_dp - earn_si_lo * tr_param * 2.0_dp

    !if (opt_trref==1)  fun_hsv_new =fun_hsv_new - earn_si_lo * 0.01_dp

    else
    
        if (lab1==0.0_dp .and. lab2>0.0_dp)then
            tbas = y2
            fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr)   !* lvl_ref**tau_pr
            fun_hsv_new =fun_hsv_new - earn_si_lo * tr_param
        
            fun_hsv_new = fun_hsv_new - 0.202_dp* earn_si_lo
            
        elseif (lab2==0.0_dp .and. lab1>0.0_dp)then
            
            tbas = y1
            fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr)   !* lvl_ref**tau_pr
            fun_hsv_new =fun_hsv_new - earn_si_lo * tr_param
        
            fun_hsv_new = fun_hsv_new - 0.202_dp* earn_si_lo
            
        else
            
            fun_hsv_new = - 0.202_dp* earn_si_lo * 2.0_dp
            
            
        endif
        
        
    endif
    
    
else ! single
    
    if (lab> 0.0_dp)then
    
    tbas = y
    fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr)   !* lvl_ref**tau_pr
    fun_hsv_new =fun_hsv_new - earn_si_lo * tr_param
 
    else
        if (jc==js)then
            if (sc<3)then
                fun_hsv_new = - 0.202_dp* earn_si_lo 
            else
                fun_hsv_new = 0.0_dp
            endif
        else
            
            
            fun_hsv_new = - 0.202_dp* earn_si_lo 
        endif
        
    endif
    
    
    !if ( jc>js .and. opt_trref==1 ) fun_hsv_new =fun_hsv_new - earn_si_lo * 0.05_dp
    
    !if (opt_mt == 0)then
    !
    !    if ( opt_trref==1 .and. opt_margprg==0 .and. jc>jt) fun_hsv_new =fun_hsv_new - tr_param * earn_si_lo
    !    
    !else
    !    if ( opt_trref==1 .and. opt_margprg==0)then
    !        
    !        if (indbelow==1)then
    !            fun_hsv_new =fun_hsv_new - tr_param * earn_si_lo
    !        endif
    !        
    !    endif
    !    
    !    
    !endif
    
    !if (opt_trref==1)  fun_hsv_new =fun_hsv_new - earn_si_lo * 0.01_dp

    !if (opt_trref==1)then
    !    ! conditional on employment transfer
    !    if (dc==1) fun_hsv_new =fun_hsv_new - earn_si_lo * 0.13_dp
    !endif
     
endif

end function fun_hsv_new
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_tax(margtax,avtax,y,y1,y2,lab,lab1,lab2,mc,gc,ec,dc,ic,jc,tc,numchld,avearn,tau_p)
! Tax function: HSV (2-param)

implicit none
real(dp),intent(in):: y,y1,y2,lab,lab1,lab2,numchld,avearn,tau_p
integer,intent(in):: mc,gc,ec,dc,jc,tc,ic ! ind_ass = 1 receive SA, 0 not
real(dp),intent(out):: margtax,avtax
real(dp):: lp,tbas, tau_loc, fun_hsv_new
real(dp):: n=0.0_dp ! number of children

n = 0.0_dp
if (mc==1)then
    n = numchld
else
    if (gc==1)then
        n = 0.0_dp
    else
        n = numchld
    endif
endif

if (mc==1)then ! couple
   
    if (lab1 > 0.0_dp .and. lab2> 0.0_dp)then
    
    tbas = (y1+y2)  !/ avearn
    fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr) !* lvl_ref**tau_pr
    fun_hsv_new =fun_hsv_new !* avearn
    fun_hsv_new = fun_hsv_new  - earn_si_lo * tr_param * 2.0_dp

    !if (opt_trref==1)  fun_hsv_new =fun_hsv_new - earn_si_lo * 0.01_dp

    margtax = 1.0_dp - (1.0_dp-tau_pr) * lambda_pr * tbas**(-tau_pr)
    avtax = 1.0_dp -  lambda_pr * tbas**(-tau_pr)
    else
    
        if (lab1==0.0_dp .and. lab2>0.0_dp)then
            tbas = y2
            fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr)   !* lvl_ref**tau_pr
            fun_hsv_new =fun_hsv_new - earn_si_lo * tr_param
        
            fun_hsv_new = fun_hsv_new - 0.202_dp* earn_si_lo
            margtax = tbas - (1.0_dp-tau_pr)  * lambda_pr *tbas**(-tau_pr)
            avtax = tbas -  lambda_pr *tbas**(-tau_pr)
        elseif (lab2==0.0_dp .and. lab1>0.0_dp)then
            
            tbas = y1
            fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr)   !* lvl_ref**tau_pr
            fun_hsv_new =fun_hsv_new - earn_si_lo * tr_param
        
            fun_hsv_new = fun_hsv_new - 0.202_dp* earn_si_lo
            margtax = tbas - (1.0_dp-tau_pr)  * lambda_pr *tbas**(-tau_pr)
            avtax = tbas -  lambda_pr *tbas**(-tau_pr)
        else
            
            fun_hsv_new = - 0.202_dp* earn_si_lo * 2.0_dp
            
            margtax = 0.0_dp
            avtax = 0.0_dp
        endif
        
        
    endif
    
else ! single
    
    if (lab> 0.0_dp)then
    
    tbas = y
    fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr)   !* lvl_ref**tau_pr
    fun_hsv_new =fun_hsv_new - earn_si_lo * tr_param
    margtax = tbas - (1.0_dp-tau_pr)  *lambda_pr * tbas**(-tau_pr)
    avtax = tbas - lambda_pr * tbas**(-tau_pr)
    else
        margtax = 0.0_dp
        !if (jc==js)then
        !    if (sc<3)then
        !        fun_hsv_new = - 0.202_dp* earn_si_lo 
        !    else
        !        fun_hsv_new = 0.0_dp
        !    endif
        !else
            
            
            fun_hsv_new = - 0.202_dp* earn_si_lo 
       ! endif
        
    endif
    
    !if ( jc>js .and. opt_trref==1 ) fun_hsv_new =fun_hsv_new - earn_si_lo * 0.05_dp
    
    !if (opt_mt == 0)then
    !
    !    if ( opt_trref==1 .and. opt_margprg==0 .and. jc>jt) fun_hsv_new =fun_hsv_new - tr_param * earn_si_lo
    !    
    !else
    !    if ( opt_trref==1 .and. opt_margprg==0)then
    !        
    !        if (indbelow==1)then
    !            fun_hsv_new =fun_hsv_new - tr_param * earn_si_lo
    !        endif
    !        
    !    endif
    !    
    !    
    !endif
    
    !if (opt_trref==1)  fun_hsv_new =fun_hsv_new - earn_si_lo * 0.01_dp

    !if (opt_trref==1)then
    !    ! conditional on employment transfer
    !    if (dc==1) fun_hsv_new =fun_hsv_new - earn_si_lo * 0.13_dp
    !endif
     
endif

end subroutine sub_tax
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_hsv_tmp(y,y1,y2,lab,lab1,lab2,mc,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,tmp1,tmp2,ind_below)
! Tax function: HSV (2-param)

implicit none
real(dp),intent(in):: y,y1,y2,numchld,avearn,lab,lab1,lab2
real(dp),intent(out):: tmp1,tmp2
integer,intent(in):: mc,gc,ec,dc,jc,tc,ic,ind_below,sc ! ind_ass = 1 receive SA, 0 not
real(dp):: lp,tbas
real(dp):: n=0.0_dp ! number of children


n = 0.0_dp
if (mc==1)then
    n = numchld
else
    if (gc==1)then
        n = 0.0_dp
    else
        n = numchld
    endif
endif 

if (mc==1)then ! couple
   
    
    if (lab1> 0.0_dp .and. lab2>0.0_dp)then
    
        tbas = (y1+y2)*0.5_dp !/ avearn
    
        tmp1 = tbas 
        tmp1 = tmp1 - earn_si_lo * tr_param
        tmp1 = tmp1 * 2.0_dp
    
        tmp2 = (tbas)**(1.0_dp-tau_pr)
        tmp2 = tmp2 * 2.0_dp
        !fun_hsv_new = tbas - lambda_pr * (tbas)**(1.0_dp-tau_pr) !* lvl_ref**tau_pr
    !    fun_hsv_new =fun_hsv_new* avearn
    !    fun_hsv_new = fun_hsv_new *2.0_dp
        
    else
        
        if (lab1> 0.0_dp .and. lab2==0.0_dp)then
            
            tbas = y1
    
            tmp1 = tbas
            tmp1 =tmp1 - earn_si_lo * tr_param
            tmp1 = tmp1 - 0.202_dp *earn_si_lo
            tmp2 = (tbas)**(1.0_dp-tau_pr)
            
        elseif (lab2> 0.0_dp .and. lab1==0.0_dp)then
            
            tbas = y2
    
            tmp1 = tbas
            tmp1 =tmp1 - earn_si_lo * tr_param
            tmp1 = tmp1 - 0.202_dp * earn_si_lo
            tmp2 = (tbas)**(1.0_dp-tau_pr)
            
        else
            
            tmp1 = 0.0_dp
            tmp1 = tmp1 - 0.202_dp * earn_si_lo * 2.0_dp
            tmp2 = 0.0_dp
            
        endif
        
        
        
    endif
    
        
        
    
else ! single 
    
    if (lab>0.0_dp)then
    
        tbas = y
    
        tmp1 = tbas
        tmp1 =tmp1 - earn_si_lo * tr_param
    
        !if (opt_mt==0)then
        !
        !    if (  opt_trref==1 .and. opt_margprg==0) tmp1 =tmp1 - earn_si_lo * tr_param
        !else
        !    if (  opt_trref==1 .and. opt_margprg==0 .and. ind_below==1)then
        !        tmp1 =tmp1 - earn_si_lo * tr_param
        !    endif
        !    
        !    
        !endif
    
    
        tmp2 = (tbas)**(1.0_dp-tau_pr) !* lambda_pr !* avearn**0.18_dp
    
    
    
        !if (opt_trref==1)then
        !    ! conditional on employment transfer
        !    if (dc==1) fun_hsv_new =fun_hsv_new - earn_si_lo * 0.13_dp
        !endif
        
    else
        
        tmp1 = 0.0_dp
        if (jc==js)then
            if (sc<3) tmp1 = tmp1 - 0.202_dp * earn_si_lo
        else
            tmp1 = tmp1 - 0.202_dp * earn_si_lo
        endif
        
        tmp2 = 0.0_dp
        
        
    endif
    
        
    
endif

end subroutine sub_hsv_tmp
! --------------------------------------------------------------------------------------


! -----------------------------------------------------------------
function fun_cev(vbase,vscen,af)

implicit none
real(dp):: fun_cev
real(dp),intent(in):: vbase,vscen,af 
real(dp):: vdist

vdist = vscen - vbase

fun_cev = exp(vdist / af ) - 1.0_dp

end function fun_cev
! -----------------------------------------------------------------

! -----------------------------------------------------------------
function fun_gcons(labrv,pensrv,caprv,consrv,expend)

implicit none
real(dp):: fun_gcons
real(dp),intent(in):: labrv,pensrv,caprv,consrv,expend 

fun_gcons = labrv+pensrv+caprv+consrv-expend  

end function fun_gcons
! -----------------------------------------------------------------

! ---------------------------------------------------------------------------------------------
function f_tau_k(cap,ret,gconst,expend,labrv,pensrv,consrv)
! computes update of unemployment insurance contribution rate
implicit none

real(dp)::f_tau_k
real(dp),intent(in)::cap,ret,gconst,expend,labrv,pensrv,consrv
real(dp):: allrv,netexp

! all tax revenues
allrv = labrv+pensrv+consrv

! net gov expenditures
netexp = gconst + expend - allrv

f_tau_k=max(0.0_dp,f_tau(cap*ret,netexp) )

end function f_tau_k
! ---------------------------------------------------------------------------------------------


! ---------------------------------------------------------------------------------------------
function f_debtss(gconst,expend,caprv,pensrv,consrv,labrv,ret)
! computes government deficit
! notice: expend DO NOT nclude debt service expenditures, because in ss
implicit none

real(dp)::f_debtss
real(dp),intent(in)::labrv,gconst,expend,caprv,pensrv,consrv,ret
real(dp):: allrv,netexp,help

! all tax revenues
allrv = caprv+pensrv+consrv+labrv

! net gov expenditures
netexp = gconst + expend - allrv

f_debtss = -netexp /(ret - popgr - lamb - popgr * lamb)

end function f_debtss
! ---------------------------------------------------------------------------------------------


! ---------------------------------------------------------------------------------------------
function f_debt(gconst,expend,caprv,pensrv,consrv,labrv)
! computes government deficit
! notice: expend also includes debt service expenditures
implicit none

real(dp)::f_debt
real(dp),intent(in)::labrv,gconst,expend,caprv,pensrv,consrv
real(dp):: allrv,netexp,help

! all tax revenues
allrv = caprv+pensrv+consrv+labrv

! net gov expenditures
netexp = gconst + expend - allrv

f_debt = netexp 

end function f_debt
! ---------------------------------------------------------------------------------------------


! ---------------------------------------------------------------------------------------------
function f_deficit(hsvtmp1,hsvtmp2,gconst,expend,caprv,pensrv,consrv,bp1)
! computes government deficit
! notice: expend also includes debt service expenditures
implicit none

real(dp)::f_deficit
real(dp),intent(in)::hsvtmp1,hsvtmp2,gconst,expend,caprv,pensrv,consrv,bp1
real(dp):: allrv,netexp,help

! all tax revenues
allrv = caprv+pensrv+consrv

! net gov expenditures
netexp = gconst + expend - allrv

f_deficit = bp1 - netexp

end function f_deficit
! ---------------------------------------------------------------------------------------------



! ---------------------------------------------------------------------------------------------
function f_lambda(hsvtmp1,hsvtmp2,gconst,expend,caprv,pensrv,consrv)
! computes update of unemployment insurance contribution rate
implicit none

real(dp)::f_lambda
real(dp),intent(in)::hsvtmp1,hsvtmp2,gconst,expend,caprv,pensrv,consrv
real(dp):: allrv,netexp,help

! all tax revenues
allrv = caprv+pensrv+consrv

! net gov expenditures
netexp = gconst + expend - allrv

help =  hsvtmp1 - netexp

f_lambda=max(0.0_dp,f_tau(hsvtmp2,help) )

!! this gave avearn**0.18 back
!f_lambda = f_lambda**(1.0_dp/0.18_dp)
!f_lambda = f_lambda/(1.99_dp* 0.4_dp)

end function f_lambda
! ---------------------------------------------------------------------------------------------



! ----------------------------------------------------------------------------
function f_hh_tp1(h,gamma,m,t,jct,inv_ces)
! human capital production function (nested CES)

implicit none
real(dp):: f_hh_tp1
real(dp),intent(in):: h,m,t
integer,intent(in):: jct
real(dp),intent(in):: gamma
real(dp),intent(out):: inv_ces
real(dp):: inv 
real(dp):: m0,t0,inv_temp,h0
real(dp),parameter::epsi=1.0e-12
real(dp),parameter::epsi1=1.0e-4
real(dp),parameter::pen_scale=100000
real(dp),parameter:: mmin=0.01_dp, tmin=0.01_dp
real(dp):: fi0_loc,kappah_loc, hh_tp1_temp, tgov,tgov_input, kappai_tilde,kappa0_loc,abar_loc,inv_comp,inv_comp_temp

m0=m /mean_moninv  + epsi1

tgov = schweek / 60_dp * ft_lab

t0= t /(mean_tinv ) + epsi1

if (opt_trref==0)then
    tgov_input= tgov/(tgov) !* edu_spend_fac !*1.5_dp
else
    if (jct>1 .and. opt_trref_hk==1)then
        tgov_input= tgov/(tgov) * igov_ref
    else
        tgov_input= tgov/(tgov)
    endif
    
endif

!tgov_input= tgov

kappai_tilde =1.0_dp/( kappai_j_param(jct)  + 1.0_dp)

h0 = h

! compute parental investment
if (abs(fitm_param) > epsi)then
    inv_temp = kappai_tilde*m0**(fitm_param) + (1.0_dp-kappai_tilde)*t0**(fitm_param)
    inv = inv_temp **(1.0_dp/fitm_param) 
else
    inv_temp = m0**kappai_tilde * t0**(1.0_dp-kappai_tilde)
    inv = inv_temp
endif

inv_ces = inv

inv = inv_ces / inv_mean

! compute CES aggregate of parental and public inv
if (jct==1)then
    inv_comp_temp = kappap4*  inv**rho_g +(1.0_dp - kappap4) * tgov_input**rho_g
    !inv_comp_temp = kappap4*  inv +(1.0_dp - kappap4) * tgov_input

else
    inv_comp_temp = kappap*  inv**rho_g +(1.0_dp - kappap) * tgov_input**rho_g
   ! inv_comp_temp = kappap*  inv +(1.0_dp - kappap) * tgov_input

endif

inv_comp = inv_comp_temp**(1.0_dp/rho_g) 

!inv_comp = inv**kappap * tgov_input**(1.0_dp - kappap)

inv = inv_comp

fi0_loc = fi0_j_param(jct)
kappa0_loc =1.0_dp / (kappah_j_param(jct) + 1.0_dp )
abar_loc = abar_param / (1.0_dp-kkappa(1))

if (opt_cd==1)then
    f_hh_tp1=h0**kappa0_loc  * abar_loc* ( inv   )**(1.0_dp-kappa0_loc)
else
      
    f_hh_tp1= ( kappa0_loc* ( h0**fi0_loc) +   (1.0_dp-kappa0_loc)* ((abar_loc* inv   )**fi0_loc) )**(1.0_dp/fi0_loc)
endif


!f_hh_tp1=h0**kappa0_loc  *  (abar_loc* inv   )**(1.0_dp-kappa0_loc)   !  ( kappa0_loc* ( h0**fi0_loc) +   (1.0_dp-kappa0_loc)* ((abar_loc* inv   )**fi0_loc) )**(1.0_dp/fi0_loc)


end function f_hh_tp1
!----------------------------------------------------------------------------

!! ----------------------------------------------------------------------------
!function f_hh_tp1_couple(h,gamma1,gamma2,m,t1,t2,jct)
!! human capital production function (nested CES)
!
!implicit none
!real(dp):: f_hh_tp1_couple
!real(dp),intent(in):: h,m,t1,t2
!integer,intent(in):: jct
!real(dp),intent(in):: gamma1,gamma2
!real(dp):: inv 
!real(dp):: m0,t0, t10,t20
!real(dp),parameter::epsi=1.0e-12
!real(dp),parameter::pen_scale=100000
!real(dp),dimension(3):: zi1_j,fi0_j,fi1_j,fi2_j,kappa_j,psi_j
!
!m0=m !+ibar
!t10=t1
!t20=t2
!
!zi1_j=zi1_age
!fi0_j=fi0_age
!kappa_j=kappa_age
!psi_j=psi_age
!   
!t0 =t1 ! gamma1*t10 +   gamma2*t20 
!
!! determine scaling parameter(s) for translating time input into monetary terms
!inv=m0**zi1_j(jct) * (  t0**(1.0_dp-zi1_j(jct)) )
!
!inv = inv + ibar
!
!f_hh_tp1_couple=( kappa_j(jct)* ( h**fi0_j(jct)) + (1.0_dp-kappa_j(jct))* ((psi_age(jct)*(inv)  )**fi0_j(jct)) )**(1.0_dp/fi0_j(jct))
!
!end function f_hh_tp1_couple
!!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
function func_tcost(h,sc,scpar)

implicit none
real(dp):: func_tcost
real(dp),intent(in):: h ! child hk
integer,intent(in):: sc,scpar ! child skill level
real(dp):: zeta

if (scpar<ns)then
    zeta = zeta_cl
else
    zeta = taste_cl
endif

if (sc==1 .or. sc==2)then

    func_tcost = 0.0_dp

elseif (sc==3)then
    
    func_tcost = (hk_str/  h) + 0.15_dp * zeta
    
elseif (sc==4)then
    
    func_tcost = (hk_str/  h)  + ( zeta)
    
endif

!func_tcost = 0.0_dp
end function func_tcost
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
function pc_max(gen_id,jc)

implicit none
integer:: pc_max
integer,intent(in):: gen_id,jc

if (gen_id==1 .and. jc<=jt .and. jc>=jf)then
    
    
    pc_max = np
    
    
else
    
    pc_max = 1
endif

end function pc_max
!----------------------------------------------------------------------------

 
!----------------------------------------------------------------------------
function pc_min(gen_id,hc,jc)

implicit none
integer:: pc_min
integer,intent(in):: gen_id,jc,hc

if (gen_id==1 .and. jc<=jt .and. jc>=jf)then
    
    
    pc_min = 1
    
    
elseif (jc==js)then
    
    pc_min = hc
else
    pc_min = 1
endif

end function pc_min
!----------------------------------------------------------------------------



!----------------------------------------------------------------------------
function hcmax(jc,sc)

implicit none
integer:: hcmax
integer,intent(in):: jc,sc

if (jc==js )then
    
    hcmax = np
    
else
    
    hcmax = 1
endif

end function hcmax
!----------------------------------------------------------------------------

 

!----------------------------------------------------------------------------
function ec_max(gen_id,jc,tc)

implicit none
integer:: ec_max
integer,intent(in):: gen_id,jc,tc

if (jc<jr(tc) )then ! before mandatory ret age
                
    if (jc>=jer)then ! retirement available
                
        if (opt_test_noer)then ! no end ret => ret state not possible
            ec_max=1
        else ! end ret => ret state possible
            ec_max = 2
        endif
                    
    else ! ret not available before age jer
        ec_max=1
    endif
            
else ! after mandatory ret age, everybody is retired
    ec_max=2
endif

end function ec_max
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
function ec_min(gen_id,jc,tc)

implicit none
integer:: ec_min
integer,intent(in):: gen_id,jc,tc

if (jc<jr(tc) )then ! before mandatory ret age
                
    if (jc>=jer)then ! retirement available
                
        if (opt_test_noer)then ! no end ret => ret state not possible
            ec_min=1
        else ! end ret => ret state possible        
            ec_min = 1              
        endif
                    
    else ! ret not available before age jer
        ec_min=1
    endif
            
else ! after mandatory ret age, everybody is retired
    ec_min=2
endif

end function ec_min
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
function pc_tp1_min(gen_id,jc,pc)

implicit none
integer:: pc_tp1_min
integer,intent(in):: gen_id,jc,pc

if ( gen_id ==1  .and. (jc<jt .and. jc>=jf) .and. opt_inv==1)then ! parenting
    
    pc_tp1_min = 1
    
else
    if (jc<jt .and. jc>=jf)then
        pc_tp1_min = pc
    else
        pc_tp1_min = 1
    endif
    
endif
    

end function pc_tp1_min
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
function pc_tp1_max(gen_id,jc,pc)

implicit none
integer:: pc_tp1_max
integer,intent(in):: gen_id,jc,pc

if ( gen_id ==1  .and. (jc<jt .and. jc>=jf) .and. opt_inv==1)then ! parenting
    
    pc_tp1_max = np
    
else
    
    if (jc<jt .and. jc>=jf)then
        pc_tp1_max = pc
    else
        pc_tp1_max = 1
    endif
    
endif
    

end function pc_tp1_max
!----------------------------------------------------------------------------


 
! -------------------------------------------------------------------------------
function ec1(jc,tc)
! function returning the first labor market group conditional on age
implicit none

integer::ec1
integer,intent(in)::jc,tc

if (jc<jr(tc)) then
    ec1=1
else
    ec1=ne
endif
    
end function ec1
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
subroutine sub_rep_child(amin_age,R,amin_param,tc)
! borrowing limit for COLLEGE hhs ONLY, starts at age js+1 when college starts
implicit none
real(dp),intent(inout)::amin_age(:,:,:)
real(dp),intent(in):: R,amin_param
integer,intent(in):: tc
real(dp):: help1,help2,rp
integer:: jc,jcc,sc,gc

amin_age = 0.0_dp

amin_age(:,4,js) =amin_param
amin_age(:,3,js) =amin_param * 0.5_dp

do sc=3,4

do gc=1,2

    do jc=js,js
        help1=amin_age(gc,sc,jc)*R
        help2=1.0_dp
    
        do jcc=jc+2,jr(tc)-1    
            help1=help1*R
            help2=help2*R+1.0_dp
        end do
        rp=help1/help2
    
      !  print*, "rp child", rp
      !  pause

        do jcc=jc+1,jr(tc)-1      
            amin_age(gc,sc,jcc)=amin_age(gc,sc,jcc-1)*R-rp        
        end do
    end do

enddo

enddo

end subroutine sub_rep_child
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_rep_parent(amin_age,R,amin_param,tc)
! borrowing limit for parents, as implied by asset holdings at age jf
implicit none
real(dp),intent(inout)::amin_age(:,:,:)
real(dp),intent(in):: R,amin_param(:,:) ! first dimension is marital status, 2d is sc
integer,intent(in):: tc
real(dp):: help1,help2,rp
integer:: jc,jcc,sc,gc

amin_age = 0.0_dp

do gc = 1,2
    do sc=1,ns
        amin_age(gc,sc,jf) = amin_param(gc,sc)
    enddo
enddo

do gc=1,2
    do sc=1,ns

        do jc=jf,jf
            help1=amin_age(gc,sc,jc)*R
            help2=1.0_dp
    
            do jcc=jf+2,jr(tc)-1    
                help1=help1*R
                help2=help2*R+1.0_dp
            end do
            rp=help1/help2
    
          !  print*, "rp parent", rp
          !  pause

            do jcc=jf+1,jr(tc)-1      
                amin_age(gc,sc,jcc)=amin_age(gc,sc,jcc-1)*R-rp        
            end do
        end do

    enddo

enddo

end subroutine sub_rep_parent
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
function func_rho(distr_long,pairs_long,n_long,n_short,opt_rank)

use alexutils
USE RANKS_INT

      !USE UMACH_INT

      !USE RNSET_INT

implicit none
real(dp),intent(in)::distr_long(:),pairs_long(:,:)
integer(kind=8),intent(in)::n_long
integer,intent(out)::n_short
logical,intent(in):: opt_rank
real(dp)::func_rho
logical::distr_ind(n_long)
real(dp),allocatable::distr_short(:),pairs_short(:,:)
real(dp),allocatable::x_reg(:,:),y_reg(:)
integer,parameter::ncoeff=2
real(dp)::b_reg(ncoeff)
integer::gc,ic
REAL(DP) :: d,zd,probd,rs,probrs
!integer,parameter:: intcep=0

distr_ind=.true.
where (distr_long(:)>0.0_dp) distr_ind(:)=.false.
if (any(distr_ind(:))) then
    n_short=ifirstloc(distr_ind)
    n_short=n_short-1
else
    n_short=n_long
endif


allocate(distr_short(n_short))
allocate(pairs_short(n_short,2))
distr_short(:)=distr_long(1:n_short)
pairs_short(:,:)=pairs_long(1:n_short,:)

allocate(x_reg(n_short,2))
allocate(y_reg(n_short))
x_reg(:,1)=1.0_dp
x_reg(:,2)=pairs_short(:,1)
y_reg(:)=pairs_short(:,2)

                
do ic=1,ncoeff
    call dhprod(n_short,distr_short,1,x_reg(:,ic),1,x_reg(:,ic),1)
end do
call dhprod(n_short,distr_short,1,y_reg(:),1,y_reg(:),1)

print*, "inside func_rho", n_short, size(y_reg ), n_long

if (opt_rank)then

    !call spear(x_reg(:,2),y_reg(:),d,zd,probd,rs,probrs)
    !func_rho = rs
    CALL RANKS (x_reg(:,2), x_reg(:,2))
    CALL RANKS (y_reg, y_reg)
    
    call d_rlse(y_reg,x_reg(:,1:2),b_reg,intcep=0)

    func_rho=b_reg(2)
    
else
    
    !x_reg(:,2) = log(x_reg(:,2))
    !y_reg(:)= log(y_reg(:))
    
    call d_rlse(y_reg,x_reg(:,1:2),b_reg,intcep=0)

    func_rho=b_reg(2)

endif
    
end function func_rho
! -------------------------------------------------------------------------------


!! -------------------------------------------------------------------------------
!function func_quint(distr_long,pairs_long,n_long,n_short,opt_rank)
!
!use alexutils
!
!implicit none
!real(dp),intent(in)::distr_long(:),pairs_long(:,:)
!integer(kind=8),intent(in)::n_long
!integer,intent(out)::n_short
!logical,intent(in):: opt_rank
!real(dp)::func_quint
!logical::distr_ind(n_long)
!real(dp),allocatable::distr_short(:),pairs_short(:,:)
!real(dp),allocatable::x_reg(:,:),y_reg(:)
!integer,parameter::ncoeff=2
!real(dp)::b_reg(ncoeff)
!integer::gc,ic
!REAL(DP) :: d,zd,probd,rs,probrs
!!integer,parameter:: intcep=0
!
!distr_ind=.true.
!where (distr_long(:)>0.0_dp) distr_ind(:)=.false.
!if (any(distr_ind(:))) then
!    n_short=ifirstloc(distr_ind)
!    n_short=n_short-1
!else
!    n_short=n_long
!endif
!
!
!allocate(distr_short(n_short))
!allocate(pairs_short(n_short,2))
!distr_short(:)=distr_long(1:n_short)
!pairs_short(:,:)=pairs_long(1:n_short,:)
!
!allocate(x_reg(n_short,2))
!allocate(y_reg(n_short))
!x_reg(:,1)=1.0_dp
!x_reg(:,2)=pairs_short(:,1)
!y_reg(:)=pairs_short(:,2)
!
!do icpar = 1,n_short
!    do ickid = 1,n_short
!
!        
!        wght_quint(quint_par,quint_kid) = wght_quint(1,quint_kid) + wght
!    
!enddo
!
!
!                
!do ic=1,ncoeff
!    call dhprod(n_short,distr_short,1,x_reg(:,ic),1,x_reg(:,ic),1)
!end do
!call dhprod(n_short,distr_short,1,y_reg(:),1,y_reg(:),1)
!
!print*, "inside func_quint", n_short, size(y_reg ), n_long
!
!
!
!if (opt_rank)then
!
!   ! call spear(x_reg(:,2),y_reg(:),d,zd,probd,rs,probrs)
!    func_rho = rs
!else
!    
!    call d_rlse(y_reg,x_reg(:,1:2),b_reg,intcep=0)
!
!    func_rho=b_reg(2)
!
!endif
!    
!end function func_quint
!! -------------------------------------------------------------------------------

!----------------------------------------------------------------------------
subroutine sub_fochk_check(moninv,tinv,cons,lab,lab_eff,nkid,gc,jc,dist_foc)

implicit none
real(dp),intent(in):: tinv,moninv,lab,cons,nkid,lab_eff
real(dp),intent(inout):: dist_foc
integer,intent(in):: gc,jc
real(dp),parameter::tolf=1.0e-09_dp,epsi=1.0e-06_dp
real(dp):: mut ,inv_ces !,muc

mut = f_mut(tinv) 
dist_foc = foc_moninv(moninv)
!muc =cons**(-1.0_dp) /(1.0_dp +tau_c)
contains

    ! -------------------------------------------------------------------
    function foc_moninv(m0)
    
    implicit none
    real(dp):: foc_moninv
    real(dp),intent(in):: m0
    real(dp):: rhs,muc1
    
    muc1 = cons**(-1.0_dp) / (1.0_dp + tau_c)
    
            
    rhs =( mut / muc1 )**(1.0_dp/(1.0_dp-fitm_param)) * (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)) &
        * tinv * (mean_tinv/mean_moninv)**(fitm_param/(1.0_dp-fitm_param) )
   
    foc_moninv = m0 - rhs
    
    !print*, m0,tinv,hp1,mcoh,cons,muc,rhs,foc_moninv
    
    end function foc_moninv
    ! -------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------
    function f_mut(t0)
    
    implicit none
    real(dp):: f_mut, temp
    real(dp),intent(in):: t0
    
    temp = lab_eff + t0 * nkid * kappat_param 
    if (gc==2) temp = temp / 2.0_dp
    
    f_mut = temp**(pssi(gc)) * pheye(1,gc) * kappat_param
    
    if (gc==2) f_mut = f_mut / 2.0_dp
    
    end function f_mut
    ! -------------------------------------------------------------------

end subroutine sub_fochk_check
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
subroutine sub_solvemoney_cpl(hk,coh,net_wage,wage_totax,moninv,tinv1,tinv2,muc,lab1,lab1_eff,lab2,lab2_eff,nkid,mcoh,hp1,cons_temp,grid_hk,cons_endo_2dim,lab1_endo_2dim,lab1eff_endo_2dim,lab2_endo_2dim,lab2eff_endo_2dim,coh_endo_2dim,evp_coh_tp1_vec_2dim,grid_sav2dim,R,ec,dc,ic,jc,tc,min_m,max_m,flag_conv) !,ind_below)
use toolbox
implicit none
real(dp),intent(in):: hk,coh,net_wage(:),wage_totax(:),tinv1,nkid,min_m,max_m,cons_endo_2dim(:,:),coh_endo_2dim(:,:), &
    grid_hk(:),evp_coh_tp1_vec_2dim(:,:),R,grid_sav2dim(:,:),lab1_endo_2dim(:,:),lab1eff_endo_2dim(:,:),lab2_endo_2dim(:,:),lab2eff_endo_2dim(:,:)
real(dp),intent(inout):: moninv,cons_temp,hp1,mcoh,muc,lab1,lab1_eff,lab2,lab2_eff,tinv2
integer,intent(in):: ec,dc,ic,jc,tc
integer,intent(out):: flag_conv
real(dp),parameter::tolf=1.0e-09_dp,epsi=1.0e-04_dp
real(dp):: mut1 ,mut2,cons,dist_foc,inv_ces, bc_sav, taggr,cons_str
logical:: check_return
integer:: ind_below

flag_conv = 1

mut1 = f_mut(tinv1,1)

! for given t1, compute t2
tinv2 = f_tinv(mut1,2)

cons_str = zbrent(foc_moninv,epsi,coh,tolf)
call fzero(cons_str,foc_moninv,check_return)

dist_foc =abs( foc_moninv(cons_str) )
if (dist_foc>epsi)then
    !print*, "brent didnt converge",jc, dist_foc,coh,hp1,minval(coh_endo_2dim),abs( foc_moninv(0.0_dp) )
    flag_conv=0
  !  pause
endif

!if (isnan(moninv))then
!    print*, moninv, cons
!    pause
!endif


cons_str = max(epsi, cons_str)

!print*, "here",tinv, moninv,foc_moninv(moninv),hp1
!pause
moninv = max(0.0_dp,func_moninv(cons_str,mut1)) 

mcoh = coh - moninv * nkid

cons_temp = cons_str

muc = cons_temp**(-1.0_dp) / (1.0_dp + tau_c)


contains 

    ! -------------------------------------------------------------------
    function func_moninv(cons,mut1)
    
    implicit none
    real(dp):: func_moninv
    real(dp),intent(in):: cons,mut1
    real(dp):: rhs, cons_temp, bc_sav, sav, mtemp
    real(dp):: vals(2)
    integer:: inds(2)
    
    
    muc = cons**(-1.0_dp) / (1.0_dp + tau_c)
    
    if (opt_igov_ps==1)then
        
        print*, "TBC FOC for time and money for PS case"
        
    else
            
        rhs =( mut1  )**(1.0_dp/(1.0_dp-fitm_param)) * (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)) &
            * tinv1 * (mean_tinv/mean_moninv)**(fitm_param/(1.0_dp-fitm_param) )
        
      !  print*, ( mut / muc )**(1.0_dp/(1.0_dp-fitm_param)), (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)), tinv
        
    endif
    
    func_moninv = rhs / (( muc )**(1.0_dp/(1.0_dp-fitm_param)) )
    
    !print*, m0,tinv,hp1,mcoh,cons,muc,rhs,foc_moninv
    
    end function func_moninv
    ! -------------------------------------------------------------------


    ! -------------------------------------------------------------------
    function foc_moninv(cons0)
    
    implicit none
    real(dp):: foc_moninv
    real(dp),intent(in):: cons0
    real(dp):: rhs, cons_temp, bc_sav, sav, mtemp
    real(dp):: vals(2)
    integer:: inds(2)
    
    mtemp = func_moninv(cons0,mut1)
    
    taggr = tinv1 + tinv2
    
    hp1 = f_hh_tp1(hk,1.0_dp,mtemp,taggr,jc-jf+1,inv_ces)    
    
    mcoh = coh - mtemp * nkid
    
    cons = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid_hk,cons_endo_2dim)  !(x,y,grid_x,grid_y,fun_xy)
    
    
    
    foc_moninv = cons - cons0
    
    !print*, m0,tinv,hp1,mcoh,cons,muc,rhs,foc_moninv
    
    end function foc_moninv
    ! -------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------
    function f_mut(t0,gc)
    
    implicit none
    real(dp):: f_mut, temp
    real(dp),intent(in):: t0
    integer,intent(in):: gc
    
    temp = t0 * nkid  
   
    f_mut = temp**(pssi(gc)) *  kappat_param
     
    end function f_mut
    ! -------------------------------------------------------------------
    
    ! -------------------------------------------------------------------
    function f_tinv(mut,gc)
    
    implicit none
    real(dp):: f_tinv, temp
    real(dp),intent(in):: mut
    integer,intent(in):: gc
    
    temp = mut / kappat_param
    temp = temp**(1.0_dp/pssi(gc))
    
    f_tinv = temp / nkid
     
    end function f_tinv
    ! -------------------------------------------------------------------

end subroutine sub_solvemoney_cpl
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
subroutine sub_solvemoney(hk,coh,net_wage,wage_totax,moninv,tinv,muc,lab,lab_eff,nkid,mcoh,hp1,cons_temp,grid_hk,cons_endo_2dim,lab_endo_2dim,labeff_endo_2dim,coh_endo_2dim,evp_coh_tp1_vec_2dim,grid_sav2dim,R,gc,ec,dc,ic,jc,tc,min_m,max_m,flag_conv) !,ind_below)
use toolbox
implicit none
real(dp),intent(in):: hk,coh,net_wage,wage_totax,tinv,nkid,min_m,max_m,cons_endo_2dim(:,:),coh_endo_2dim(:,:),grid_hk(:),evp_coh_tp1_vec_2dim(:,:),R,grid_sav2dim(:,:),lab_endo_2dim(:,:),labeff_endo_2dim(:,:)
real(dp),intent(inout):: moninv,cons_temp,hp1,mcoh,muc,lab,lab_eff
integer,intent(in):: gc,ec,dc,ic,jc,tc
integer,intent(out):: flag_conv
real(dp),parameter::tolf=1.0e-09_dp,epsi=1.0e-03_dp
real(dp):: mut ,cons,dist_foc,inv_ces, bc_sav,cons_str
logical:: check_return
integer:: ind_below

flag_conv = 1

mut = f_mut(tinv)

cons_str = zbrent(foc_moninv,epsi,coh,tolf)
call fzero(cons_str,foc_moninv,check_return)

dist_foc =abs( foc_moninv(cons_str) )
if (dist_foc>epsi .and. hp1>0.1_dp .and. tinv>0.15_dp)then
    print*, "brent didnt converge",jc, dist_foc,coh,moninv,tinv,hp1,minval(coh_endo_2dim),abs( foc_moninv(0.0_dp) )
    flag_conv=0
  ! pause
endif

!if (isnan(moninv))then
!    print*, moninv, cons
!    pause
!endif


cons_str = max(epsi, cons_str)
moninv  = max(0.0_dp,func_moninv(cons_str,mut))
!print*, "here",tinv, moninv,foc_moninv(moninv),hp1
!pause
   

mcoh = coh - moninv * nkid

cons_temp = cons_str

muc = cons_temp**(-1.0_dp) / (1.0_dp + tau_c)



contains 
    
    ! -------------------------------------------------------------------
    function func_moninv(cons,mut)
    
    implicit none
    real(dp):: func_moninv
    real(dp),intent(in):: cons,mut
    real(dp):: rhs, cons_temp, bc_sav, sav, mtemp
    real(dp):: vals(2)
    integer:: inds(2)
    
    
         
    muc = cons**(-1.0_dp) / (1.0_dp + tau_c)
    
    if (opt_igov_ps==1)then
        
        print*, "TBC FOC for time and money for PS case"
        
    else
            
        rhs =( mut  )**(1.0_dp/(1.0_dp-fitm_param)) * (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)) &
            * tinv * (mean_tinv/mean_moninv)**(fitm_param/(1.0_dp-fitm_param) )
        
      !  print*, ( mut / muc )**(1.0_dp/(1.0_dp-fitm_param)), (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)), tinv
        
    endif
    
    func_moninv =  rhs / (( muc )**(1.0_dp/(1.0_dp-fitm_param)) )
    
    !print*, m0,tinv,hp1,mcoh,cons,muc,rhs,foc_moninv
    
    end function func_moninv
    ! -------------------------------------------------------------------


    ! -------------------------------------------------------------------
    function foc_moninv(cons0)
    
    implicit none
    real(dp):: foc_moninv
    real(dp),intent(in):: cons0
    real(dp):: rhs, cons_temp, bc_sav, sav, mtemp
    real(dp):: vals(2)
    integer:: inds(2)
    
    mtemp =  func_moninv(cons0,mut)
    
    hp1 = f_hh_tp1(hk,1.0_dp,mtemp,tinv,jc-jf+1,inv_ces)    
    
    mcoh = coh - mtemp * nkid
    
    cons = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid_hk,cons_endo_2dim)  !(x,y,grid_x,grid_y,fun_xy)
    
    foc_moninv = cons - cons0
    
    end function foc_moninv
    ! -------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------
    function f_mut(t0)
    
    implicit none
    real(dp):: f_mut, temp
    real(dp),intent(in):: t0
    
    temp = t0 * nkid  
   
    f_mut = temp**(pssi(gc)) *  kappat_param
     
    end function f_mut
    ! -------------------------------------------------------------------

end subroutine sub_solvemoney
!----------------------------------------------------------------------------

! -------------------------------------------------------------------
function func_sav(coh,netwage,wage_totax,lab,cons,expend,nkid,gc,sc,ec,dc,ic,jc,tc,ind_below )
! compute savings as implied by BC
    
implicit none
real(dp):: func_sav
real(dp),intent(in):: coh,netwage,wage_totax,lab,cons,expend,nkid
integer,intent(in):: gc,ec,dc,ic,jc,tc,ind_below,sc
    
func_sav = coh - cons*(1.0_dp+tau_c) - netwage*(biggam-lab) - fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,lab,lab,2,gc,sc,ec,dc,ic,jc,tc,nkid,avearn,ind_below )  & 
    - expend *  nkid
    
end function func_sav
! -------------------------------------------------------------------



! -------------------------------------------------------------------
function func_sav_cpl(coh,netwage,wage_totax,lab1,lab2,cons,expend,nkid,ec,dc,ic,jc,tc,ind_below )
! compute savings as implied by BC
    
implicit none
real(dp):: func_sav_cpl
real(dp),intent(in):: coh,netwage(:),wage_totax(:),lab1,lab2,cons,expend,nkid
integer,intent(in):: ec,dc,ic,jc,tc,ind_below
    
func_sav_cpl = coh - cons*(1.0_dp+tau_c) - netwage(1)*(biggam-lab1) - netwage(2)*(biggam-lab2) - fun_hsv_new(wage_totax(1)*lab1 +wage_totax(2)*lab2  ,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,nkid,avearn,ind_below )  & 
    - expend *  nkid
    
end function func_sav_cpl
! -------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_h0distr(fh0_no,fh0_cl,h0mean,h0prop,h0input) !,delta)
! function returning initial h0 by groups
implicit none

real(dp),intent(in):: h0mean,h0prop(:,:),fh0_no,fh0_cl !,delta
real(dp),intent(out):: h0input(:,:)
integer:: kc,sc

do kc=1,2
    do sc=1,ns
    
    	h0input(kc,sc) =h0mean* h0prop(kc,sc)
    
        !if (sc==1)then
        !    h0input(kc,sc) =h0mean* (1.0_dp - h0delta_param)
        !elseif (sc==2)then
        !    h0input(kc,sc) =h0mean
        !else
        !    h0input(kc,sc) =h0mean* (1.0_dp + h0delta_param)
        !endif
            
    enddo
enddo

 
!! first single
!gc=1
!
!do sc=1,ns
!    if (sc<ns)then
!        h0input(gc,sc) =min(0.25_dp,max(0.04_dp, h0mean* h0prop(gc,sc) / exp(fh0_cl) ) )
!    else
!        h0input(gc,sc) =min(0.25_dp,max(0.04_dp, h0mean*  h0prop(gc,sc) * exp(fh0_cl) ) )
!    endif
!    
!enddo
!
!
!gc=2
!do sc=1,ns
!    h0input(gc,sc) = h0input(1,sc) * h0prop(2,sc)/h0prop(1,sc)
!enddo

end subroutine sub_h0distr 
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
function func_reg(distr_long,vec_long,n_long,n_short)


implicit none
real(dp),intent(in)::distr_long(:),vec_long(:,:)
integer,intent(in)::n_long
integer,intent(out)::n_short
real(dp)::func_reg(2)
logical::distr_ind(n_long)
real(dp),allocatable::distr_short(:),vec_short(:,:)
real(dp),allocatable::x_reg(:,:),y_reg(:)
integer,parameter::ncoeff=2
real(dp)::b_reg(ncoeff)
integer::gc,ic

distr_ind=.true.
where (distr_long(:)>0.0_dp) distr_ind(:)=.false.
if (any(distr_ind(:))) then
    n_short=ifirstloc(distr_ind)
    n_short=n_short-1
else
    n_short=n_long
endif


allocate(distr_short(n_short))
allocate(vec_short(n_short,2))
distr_short(:)=distr_long(1:n_short)
vec_short(:,:)=vec_long(1:n_short,:)

allocate(x_reg(n_short,2))
allocate(y_reg(n_short))
x_reg(:,1)=1.0_dp
x_reg(:,2)=vec_short(:,2) ! money inv
y_reg(:)=vec_short(:,1)  ! time inv

!y_reg = log(y_reg)
!x_reg(:,2) = log(x_reg(:,2))

call dhprod(n_short,distr_short,1,x_reg(:,2),1,x_reg(:,2),1)            
call dhprod(n_short,distr_short,1,y_reg(:),1,y_reg(:),1)
!y_reg = y_reg/ft_lab * 40.0_dp 
!y_reg =   log(y_reg + sqrt(y_reg**(2.0_dp) + 1.0_dp))
!x_reg(:,2) =   log(x_reg(:,2) + sqrt(x_reg(:,2)**(2.0_dp) + 1.0_dp))

!print*,"y", y_reg(1:20)
!print*,"x", x_reg(1:20,2)

call d_rlse(y_reg,x_reg(:,1:2),b_reg,intcep=0)

func_reg=b_reg

!print*, b_reg
!pause

end function func_reg
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
subroutine sub_reg_ind(vec,alpha0,alpha1,alpha2)


implicit none
real(dp),intent(in)::vec(:)
real(dp),intent(out)::alpha0, alpha1,alpha2
integer,parameter::ncoeff=3
real(dp)::b_reg(ncoeff)
integer::gc,ic,njkid,jc
real(dp):: y_reg(size(vec,1)), x_reg(size(vec,1),3), age_vec(size(vec,1))

njkid = size(vec,1)
do jc=3,njkid+2 
    age_vec(jc-2) = real(jc)
enddo


x_reg(:,1)=1.0_dp
x_reg(:,2)= age_vec
x_reg(:,3)= age_vec**(2.0_dp)

y_reg(:)=vec  ! time inv

y_reg =   log(vec )

!print*,"y", y_reg(1:20)
!print*,"x", x_reg(1:20,2)

call d_rlse(y_reg,x_reg(:,1:3),b_reg(1:3),intcep=0)

alpha0 = b_reg(1)
alpha1 = b_reg(2)
alpha2 = b_reg(3)

end subroutine sub_reg_ind
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
subroutine sub_reg_ind_lin(vec,alpha0,alpha1,alpha2)


implicit none
real(dp),intent(in)::vec(:)
real(dp),intent(out)::alpha0, alpha1,alpha2
integer,parameter::ncoeff=3
real(dp)::b_reg(ncoeff)
integer::gc,ic,njkid,jc
real(dp):: y_reg(size(vec,1)), x_reg(size(vec,1),3), age_vec(size(vec,1))

njkid = size(vec,1)
do jc=3,njkid+2
    age_vec(jc) = real(jc)
enddo


x_reg(:,1)=1.0_dp
x_reg(:,2)= age_vec
x_reg(:,3)= age_vec**(2.0_dp)

y_reg(:)=vec  ! time inv

y_reg =   log(vec )

!print*,"y", y_reg(1:20)
!print*,"x", x_reg(1:20,2)

call d_rlse(y_reg,x_reg(:,1:2),b_reg(1:2),intcep=0)

alpha0 = b_reg(1)
alpha1 = b_reg(2)
alpha2 =1.0_dp ! b_reg(3)
 
end subroutine sub_reg_ind_lin
! -------------------------------------------------------------------------------

! ------------------------------------------------------------------- 
subroutine sub_welfare(agg,demo,grid,pol,stat,lc,t0,t1) 
! welfare
use esplot
use toolbox !,only: fzero 
implicit none
type(t_agg),intent(inout)::agg 
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
type(t_lc),intent(inout)::lc
type(t_stat),intent(inout)::stat
integer,intent(in)::t0,t1
real(dp)::assmin,wght_j,welf_agg_xc_full(nx),welf_xj(nx,nj),grid_ass(nx),wght_x,gr_rho_p(nt),cev_loc,cev_loc_nokid,afcev,afcev1,v1_int,welf_agg,dist_phi,coh,prodret,ass,v2_int,v1_int_sp(ns),v2_int_sp(ns),welf_agg_sp(ns), & 
    welf_agg_gp_sp(ng,ns),welf_agg_gp_sp_nokid(ng,ns),welf_agg_gp(ng), &
    v1_int_gp_sp(ng,ns),v2_int_gp_sp(ng,ns),  v1_int_gp(ng),v2_int_gp(ng), v1_int_y(ny),v2_int_y(ny),welf_agg_y(ny), welf_agg_x(5),welf_agg_x_new(5),v1_int_x_new(5),v2_int_x_new(5)
real(dp):: v1_int_gp_sp_new(ng,ns),v2_int_gp_sp_new(ng,ns),v1_int_gp_sp_new_nokid(ng,ns),v2_int_gp_sp_new_nokid(ng,ns),  v1_int_gp_new(ng),v2_int_gp_new(ng),welf_agg_xc_new(nx),wght_x_vec(nx),wght_x_vec_full(nx),wght_x_vec_j(nx,nj), v1_int_y_new(ny),v2_int_y_new(ny),welf_agg_y_new(ny)
real(dp)::  v1_int_new,v1_int_new_nokid,welf_agg_new,welf_agg_new_nokid,v2_int_new,v2_int_new_nokid,v1_int_sp_new(ns),v2_int_sp_new(ns),welf_agg_sp_new(ns),welf_agg_sp_new_nokid(ns),welf_agg_kc_new(nk),welf_agg_ks_new(nk,ns),welf_agg_gp_sp_new(ng,ns),welf_agg_gp_sp_new_nokid(ng,ns),welf_agg_gp_new(ng),v1_int_gsx(2,ns,5),v2_int_gsx(2,ns,5), welf_agg_gsx(2,ns,5)
real(dp):: coh_glb_min,coh_glb_max,coh_grid(nx),v1_int_ds(nd,ns),v2_int_ds(nd,ns),logsum,logsum_base,prob_kid(ns),prob_kid_base(ns),ass_glb_max,ass_glb_min,ass_grid(nx),v1_int_x(5),v2_int_x(5),phisng(ns),phicpl(ns), wght_chk,wght,wght_s,wght_sp,frac_tot,frac_sc,frac_qsc,wght_qsp,wght_k,wght_ks
integer::inds(2)
real(dp)::vals(2)
integer::xc,pc,yc,ec,ic,jc,kc,sc,tc,tcc,tccc,dc,gc,scc,icc,jcc,jc_temp,tc_temp,sc1,sc2,kc1,kc2,zc,hc,scpar,gcpar,xcpar,ycpar,epsc,jmin,jmax,xc_dec, ind_benef,mc_max,mc,yc1,yc2,epsc1,epsc2
real(dp):: labinctaxrv_base, labinctaxrv_new, cons_base, cons_new, lab_base, lab_new
real(dp):: quint_frac_exper_new(5),quint_frac_exper(5),quint_frac_base_new(5),quint_frac_base(5),tmp,tmp_nokid,Phi_scpar(ns),Phi_scown(ns),Phi_scown2(ns),Phi_ks(nk,ns),v2_int_phi0,logsum_finvf,val_sc_initvf(ns),val_sc_finvf(ns),wghttmp,welf_agg_phi0, & 
    lsra_tr,lsra_aggr,prob_fin_hs(2),prob_fin(2),val_col,prob_col(2),ev_kid_choice(2),prob_sc(ns),probfe(nk),wght_sng, pdvterm,pdv_term,welfj_mean,welfj_mean_nokid,v1_int_new_tmp,v1_int_new_tmp_nokid
character(1)::i_char
real(dp),parameter::epsi=1.0e-06
logical:: check_return
 
ind_benef = 0

if (opt_trref==0 .and. t1==1 )then ! save baseline
   
    
    
    print*, "WELF: saved baseline objects"
   
elseif  (t1>1)then
    
    print*, ' '
    print*, '----------------------------------------'
    print*, 'Computing welfare'
    print*, '----------------------------------------'
    print*, ' '
    
    
    ! welfare of newborns
    if (t0==t1)then
        open(unit=88,file='output/cevNB.txt')
        open(unit=878,file='output/cevNBqs.txt')
        open(unit=8787,file='output/cevNBqsnokid.txt')
    else
        open(unit=88,file='output/cevNBtrans.txt')
        open(unit=878,file='output/cevNBtransqs.txt')
        open(unit=8787,file='output/cevNBtransqsnokid.txt')
    endif
    
    open(unit=90,file='output/cevAGEss.txt')
        
    if (t0==t1)then
        jmin = js
        jmax = nj
    else
        jmin = js
        jmax = js
    endif
    
    
    do tc =t0,t1 ! tclb
       
    ! welfare of newborns
    do jc=jmin,jmax
        
        ! first compute adjustment factor
        afcev = 0.0_dp
        do jcc=jc,nj
            afcev =afcev + betta**(jcc-jc) *demo%sr(jcc,1)
        enddo
        
        ! first compute adjustment factor
        afcev1 = 0.0_dp
        if (t0==t1) then ! ss
        	do jcc=jc,nj !jstud(3),nj
            	afcev1 =afcev1 + betta**(jcc-jc) *demo%sr(jcc,1)
        	enddo
        else
            do jcc=jc,nj !jstud(3),nj
            	afcev1 =afcev1 + betta**(jcc-jc) *demo%sr(jcc,1)
        	enddo
        endif
        	
       
       ! open(unit=88,file='output'//i_char//'cevU.txt')
       
        welf_agg = 0.0_dp
        v1_int = 0.0_dp
        v2_int = 0.0_dp
        v1_int_sp = 0.0_dp
        v2_int_sp = 0.0_dp
        v1_int_gp_sp = 0.0_dp
        v2_int_gp_sp = 0.0_dp
        v1_int_gp = 0.0_dp
        v2_int_gp = 0.0_dp
        v1_int_y = 0.0_dp
        v2_int_y = 0.0_dp
        v1_int_x = 0.0_dp
        v2_int_x = 0.0_dp
        
        welf_agg_new = 0.0_dp
        welf_agg_new_nokid = 0.0_dp
        v1_int_new = 0.0_dp
        v2_int_new = 0.0_dp
        v1_int_new_nokid = 0.0_dp
        v2_int_new_nokid = 0.0_dp   
        v1_int_sp_new = 0.0_dp
        v2_int_sp_new = 0.0_dp
        v1_int_gp_sp_new = 0.0_dp
        v2_int_gp_sp_new = 0.0_dp
        v1_int_gp_sp_new_nokid = 0.0_dp
        v2_int_gp_sp_new_nokid = 0.0_dp
        v1_int_gp_new = 0.0_dp
        v2_int_gp_new = 0.0_dp
        v1_int_y_new = 0.0_dp
        v2_int_y_new = 0.0_dp
        v1_int_x_new = 0.0_dp
        v2_int_x_new = 0.0_dp
        
        v1_int_gsx = 0.0_dp
        v2_int_gsx = 0.0_dp
        
        quint_frac_exper_new = 0.0_dp
        quint_frac_exper = 0.0_dp
        quint_frac_base_new = 0.0_dp
        quint_frac_base = 0.0_dp
        
        wght_chk = 0.0_dp
        
        
        v2_int_phi0 = 0.0_dp
        
        
        
        do mc = 1,2
            
            do sc = 1,ns
            Phi_scown(sc) = sum(grid%Phi_childja(:,:,:,:,:,:,:,:,:,sc,:,:,mc,1))
            do kc=1,nk
                Phi_ks(kc,sc) = sum(grid%Phi(:,:,:,:,:,kc,:,:,:,sc,:,:,js,1))
            enddo
            
        enddo
            
        do scpar=1,ns
            Phi_scpar(scpar)= sum(grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,scpar,mc,1))
            do gcpar=1,1 !ng
           
                do hc=1,np
                    do gc=1,ng
                        do ic=1,ni
                            do pc=1,np
                                            
                                do xc=1,nx
                                       
                                                    
                                    val_sc_initvf = 0.0_dp
                                    val_sc_finvf = 0.0_dp                    
                                    do sc=1,ns
                                    
                                        if (nk>1)then
                                            probfe(nk) = f_probfe(grid%hk_grid(hc),sc,agg%gammah_s(sc,tc))
                                            probfe(1) = 1.0_dp -probfe(nk) 
                                        else
                                            probfe(1) = 1.0_dp
                                        endif
                                                    
                                        do kc=1,nk                                            
                                            do ec=1,1 !ne                
                                                do yc=1,ny
                                                    do epsc=1,nw            
                                                        do dc=1,1 !nd !l
                                                        
                                                          
                                                            tmp = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc), & 
                                                                grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1),vals,inds,.false.,.false.)
                                                           
                                                           ! val_sc_initvf(sc) = val_sc_initvf(sc) + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght / Phi_scown(sc)
                                                            
                                                            val_sc_finvf(sc) = val_sc_finvf(sc) + tmp *  demo%pini(yc) * demo%prob_epsi(epsc) * probfe(kc)
                                                              
                                           
                                                        end do
                                                    end do
                                                end do
                                            end do
                                        enddo
                                    end do
                                    wghttmp = sum(grid%Phi(:,xc,1,:,:,:,gc,:,hc,:,1,scpar,jc,1) )   
                                
                                    prob_fin(1) = f_probfin(grid%hk_grid(hc)) ! CL completion prob
                                    prob_fin(2) = 1.0_dp - prob_fin(1) ! CL dropout prob
                                    prob_fin_hs(1) = f_probfin_hs(grid%hk_grid(hc)) ! HS completion prob
                                    prob_fin_hs(2) = 1.0_dp - prob_fin_hs(1) ! HS dropout prob
                                    val_col = prob_fin(1) * val_sc_finvf(ns) + prob_fin(2) * val_sc_finvf(ns-1) ! value of choosing college
                                    prob_col = 0.0_dp
            
                                    ev_kid_choice(1)= val_sc_finvf(2) ! not choose CL => end up as HS graduate
                                    ev_kid_choice(2) = val_col ! choose CL =>  get val col
 
                                    ! compute prob of choosing college
                                    call sub_logsumprob(ev_kid_choice,logsum,prob_col,2,sigma_emp)
                                
                                    prob_sc(1)=prob_fin_hs(2) ! HS dropout prob
                                    prob_sc(2)=prob_fin_hs(1) *prob_col(1) ! HS graduate, choose NO CL
                                    prob_sc(3)=prob_fin_hs(1) *prob_col(2) * prob_fin(2) ! HS graduate, choose CL, but dropout
                                    prob_sc(4)=prob_fin_hs(1) *prob_col(2) * prob_fin(1) ! HS graduate, choose CL, finish CL

                                    ! logsum for init and fin
                                    !call sub_logsumprob(val2,logsum_finvf,prob_kid(1:2),2,sigma_emp)
                                    do sc = 1,ns
                                        v2_int_phi0 = v2_int_phi0 +  val_sc_finvf(sc) * prob_sc(sc) * wghttmp
                                    enddo
                                
                               
                                
                                    do sc=1,ns
                                 
                                                    
                                        do kc=1,nk                                            
                                            do ec=1,ne                
                                                do yc=1,ny
                                                    do epsc=1,nw            
                                                        do dc=1,nd !l
                                                        
                                                        
                                                            wght = grid%Phi_childja(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,mc,1) 
                                                            wght_s = wght / Phi_scown(sc) !Phi_scpar(scpar)
                                                            wght_sp = wght / Phi_scpar(scpar)
                                                            wght_ks = wght / Phi_ks(kc,sc) 
                                                            if (wght==0.0_dp) cycle
                                                            wght_chk = wght_chk + grid%Phi_childja(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,mc,1) 
                                                                
                                                            
                                                            ! new CEV
                                                            v1_int_new = v1_int_new  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                            v1_int_new_nokid = v1_int_new_nokid  + pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                            
                                                            !v1_int_sp_new(scpar) = v1_int_sp_new(scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght_s
                                                            v1_int_sp_new(scpar) = v1_int_sp_new(scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght_sp
                                                            v1_int_gp_sp_new(mc,scpar) = v1_int_gp_sp_new(mc,scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght_sp
                                                            v1_int_gp_sp_new_nokid(mc,scpar) = v1_int_gp_sp_new_nokid(mc,scpar)  + pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) * wght_sp
                                                            tmp = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc), & 
                                                                grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1),vals,inds,.false.,.false.)
                                                            v2_int = v2_int  + tmp * wght 
                                                            v2_int_sp(sc) = v2_int_sp(sc)  + tmp * wght_s 
                                                            !do scc = 1,ns
                                                            !    v2_int_phi0 = v2_int_phi0 + tmp  * prob_sc(scc) * wght
                                                            !enddo
                                                            
                                                        end do
                                                    end do
                                                end do
                                            end do
                                        enddo
                                    end do
                                enddo  
                            end do
                        enddo
                    enddo
                enddo
            enddo
        enddo
        enddo
        
        if (abs(1.0_dp - wght_chk)  > epsi)then
            print*, "wght chk violation1"
        endif
        
        
        
      !  print*, "wght", wght_chk
        
        wght_chk = 0.0_dp
        
        do mc = 1,2
        do sc = 1,ns
            Phi_scown2(sc) = sum(grid%Phi_childja(:,:,:,:,:,:,:,:,:,sc,:,:,mc,tc))
            do kc=1,nk
                Phi_ks(kc,sc) = sum(grid%Phi(:,:,:,:,:,kc,:,:,:,sc,:,:,js,tc))
            enddo
            
        enddo
        
        do scpar=1,ns 
            Phi_scpar(scpar)= sum(grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,scpar,mc,tc))
            do gcpar=1,1 !ng
           
                do hc=1,np
                    
        
                    do kc=1,nk
                        do gc=1,ng
                            do ic=1,ni
                                do ec=1,ne
                                    do yc=1,ny
                                        do epsc=1,nw
                                        do pc=1,np !1
                                            do sc=1,ns
                                                do xc=1,nx
                                                    do dc=1,nd !l
                                                        
                                                            wght = grid%Phi_childja(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,mc,tc) 
                                                            wght_s = wght / Phi_scown2(sc) !Phi_scpar(scpar)
                                                            wght_sp = wght / Phi_scpar(scpar)
                                                            wght_ks= wght / Phi_ks(kc,sc)
                                                            if (wght==0.0_dp) cycle
                                                            wght_chk = wght_chk + grid%Phi_childja(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,mc,tc) 
                                                                
                                                            ! new CEV
                                                            v2_int_new = v2_int_new  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                            v2_int_new_nokid = v2_int_new_nokid  + pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght
                                                            !v2_int_sp_new(scpar) = v2_int_sp_new(scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght_s
                                                            v2_int_sp_new(scpar) = v2_int_sp_new(scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght_sp
                                                            v2_int_gp_sp_new(mc,scpar) = v2_int_gp_sp_new(mc,scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght_sp
                                                            v2_int_gp_sp_new_nokid(mc,scpar) = v2_int_gp_sp_new_nokid(mc,scpar)  + pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght_sp
                                                            ! v2_int_new =  func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc), &
                                                           !     grid%coh_base(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,vals,inds,.false.,.false.)!* grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc)! * agg%educ_matrix(scpar,sc,tc)/ sum(agg%educ_matrix(:,sc,tc) )
                                                            !cev_loc = fun_cev(v1_int_new,v2_int_new,afcev1) 
                                                           ! welf_agg_new = welf_agg_new + wght * cev_loc
                                                                
                                                              
                                           
                                                    end do
                                                end do
                                            end do
                                        end do
                                    enddo
                                        
                            end do
                        enddo  
                    end do
                enddo
            enddo
        enddo
            enddo
        enddo
        enddo
        
        !print*, "wght2", wght_chk
        
        ! compute CEV
        welf_agg_new = fun_cev(v1_int_new,v2_int_new,afcev1) 
        welf_agg_new_nokid = fun_cev(v1_int_new_nokid,v2_int_new_nokid,afcev1) 
        welf_agg= fun_cev(v1_int_new,v2_int,afcev1) 
        welf_agg_phi0 = fun_cev(v1_int_new,v2_int_phi0,afcev1) 
        
        do scpar = 1,ns
            welf_agg_sp_new(scpar) = fun_cev(v1_int_sp_new(scpar),v2_int_sp_new(scpar),afcev1) 
            welf_agg_sp(scpar)= fun_cev(v1_int_sp_new(scpar),v2_int_sp(scpar),afcev1)
            welf_agg_sp_new(scpar) = welf_agg_sp_new(scpar) * 100.0_dp
            welf_agg_sp(scpar) = welf_agg_sp(scpar) * 100.0_dp
            do kc=1,nk
                welf_agg_gp_sp_new(kc,scpar) = fun_cev(v1_int_gp_sp_new(kc,scpar),v2_int_gp_sp_new(kc,scpar),afcev1) 
                welf_agg_gp_sp_new(kc,scpar) =welf_agg_gp_sp_new(kc,scpar) * 100_dp
                welf_agg_gp_sp_new_nokid(kc,scpar) = fun_cev(v1_int_gp_sp_new_nokid(kc,scpar),v2_int_gp_sp_new_nokid(kc,scpar),afcev1) 
                welf_agg_gp_sp_new_nokid(kc,scpar) =welf_agg_gp_sp_new_nokid(kc,scpar) * 100_dp
            enddo
            
        enddo
        
        !welf_agg_new = fun_cev(v1_int_new,v2_int_new,afcev1) 
        welf_agg_new = welf_agg_new * 100.0_dp
        welf_agg_new_nokid = welf_agg_new_nokid * 100.0_dp
        welf_agg = welf_agg * 100.0_dp
        welf_agg_phi0 = welf_agg_phi0 * 100.0_dp
        
        
       ! print*,"WELFF" ,welf_agg_new, v1_int_new,v2_int_new,afcev1, sum( grid%Phi_base ) !,pol%v_base(1,:,1,1,1,1,1,:,1,1,1,1,jstud(3),tc)
        !pause
        agg%welf_t(tc) = welf_agg_new
        
        if (jmin/=jmax)then
            write(90,'(1f50.16)') welf_agg_new !, welf_agg
        endif
        
        if (jc==js)then
            lsra_tr = 0.0_dp
            !lsra_tr =zbrent(fun_lsra,-3.0_dp,0.5_dp,epsi)
            call fzero(lsra_tr,fun_lsra,check_return)
            print*,tc,jc, "lsra NB", lsra_tr,fun_lsra(lsra_tr)
            ! pdv_term = pdv_term + (1.0_dp+agg%ret(tc) )**(- (tc-1)  )
        endif
        
        
        if (jc==js)then 
            write(88,'(12f50.16)') welf_agg_new,welf_agg_new_nokid, welf_agg, welf_agg_phi0 ,  welf_agg_sp_new(1), welf_agg_sp_new(2),welf_agg_sp_new(3),welf_agg_sp_new(4),  welf_agg_sp(1), welf_agg_sp(2),welf_agg_sp(3),welf_agg_sp(4)
            write(878,'(8f50.16)')  welf_agg_gp_sp_new(1,1),welf_agg_gp_sp_new(1,2),welf_agg_gp_sp_new(1,3),welf_agg_gp_sp_new(1,4), welf_agg_gp_sp_new(2,1),welf_agg_gp_sp_new(2,2),welf_agg_gp_sp_new(2,3),welf_agg_gp_sp_new(2,4)
            write(8787,'(8f50.16)')  welf_agg_gp_sp_new_nokid(1,1),welf_agg_gp_sp_new_nokid(1,2),welf_agg_gp_sp_new(1,3),welf_agg_gp_sp_new_nokid(1,4), welf_agg_gp_sp_new_nokid(2,1),welf_agg_gp_sp_new_nokid(2,2),welf_agg_gp_sp_new_nokid(2,3),welf_agg_gp_sp_new_nokid(2,4)
            
            !write(33,'(4f50.16)') welf_agg,  welf_agg_sp(1), welf_agg_sp(2),welf_agg_sp(3) 
            print*, "av welfNB", welf_agg_new,welf_agg_new_nokid, welf_agg, welf_agg_phi0,sum(agg%welf_t(1:nt))/nt
            agg%lsra(tc) = lsra_tr
        endif
        
       
        enddo
    enddo
    
    close ( 88)
    close (90)
    close (878)
    close (8787)
    
    pdv_term = 0.0_dp
        
    if (t1>t0)then
        lsra_tr = 0.0_dp
        
        do tc = 2,nt
            lsra_tr = lsra_tr + agg%lsra(tc) / (1.0_dp+agg%ret(tc))**(real(tc) - 1.0_dp)
            
            pdv_term = pdv_term + 1.0_dp / ((1.0_dp+agg%ret(tc))**(real(tc) - 1.0_dp) )
        enddo
        print*, "pdv lsra", lsra_tr, pdv_term,agg%ass(1)
    endif
    
    
    ! for transition, welfare of currently alive popualtion
    ! if (t0/=t1)then
        
        if (t0==t1) then
            open(unit=89,file='output/cevAGE.txt')
            open(unit=899,file='output/cevAGEqs.txt')
            open(unit=8992,file='output/cevAGEqsnokid.txt')
            open(unit=8911,file='output/cevAGEaver.txt')
            open(unit=893,file='output/cevAGExdec.txt')
        else
            open(unit=89,file='output/cevAGET.txt') !,action='write',position='append')
            open(unit=899,file='output/cevAGETqs.txt')
            open(unit=8992,file='output/cevAGETqsnokid.txt')
            open(unit=8911,file='output/cevAGEaverT.txt') !,action='write',position='append')
            open(unit=893,file='output/cevAGETxdec.txt')
        endif
        
            
        if (t0/=t1)then 
            tc = 1
            tcc = 6 !2
        else
            tc = 1
            tcc = nt
        endif
        
        assmin = minval(grid%ass(:,1,:,:,:,:,:,:,:,:,:,:,js:nj,1))
        if (max_x_fac>epsi) then
            grid_ass(1:nx-1)=makegrid(assmin,max_x/max_x_fac,nx-1,3.0_dp)
            grid_ass(nx)=max_x
        else
            grid_ass(1:nx)=makegrid(assmin,max_x,nx,3.0_dp)
        endif
        
            welf_agg_xc_new = 0.0_dp
            wght_x_vec = 0.0_dp
            agg%lsra_j = 0.0_dp
            
            
            do jc=js,nj !js,js
            
            wght_j = demo%sr(jc,1)/sum(demo%sr(js+1:jr(1)-1,1))
        
            ! first compute adjustment factor
            afcev = 0.0_dp
            do jcc=jc,nj
                afcev =afcev + betta**(jcc-jc) *demo%sr(jcc,1)
            enddo
        
            ! first compute adjustment factor
            afcev1 = 0.0_dp
            if (t0==t1) then ! ss
        	    do jcc=js,nj !jc,nj !jstud(3),nj
            	    afcev1 =afcev1 + betta**(jcc-jc) *demo%sr(jcc,1)
        	    enddo
            else
                do jcc=js,nj !jc,nj !jstud(3),nj
            	    afcev1 =afcev1 + betta**(jcc-jc) *demo%sr(jcc,1)
        	    enddo
            endif
        	
       
           ! open(unit=88,file='output'//i_char//'cevU.txt')
       
                welf_agg = 0.0_dp
                v1_int = 0.0_dp
                v2_int = 0.0_dp
                v1_int_new = 0.0_dp
                v2_int_new = 0.0_dp
                v1_int_new_nokid = 0.0_dp
                v2_int_new_nokid = 0.0_dp
                v1_int_sp_new = 0.0_dp
                v2_int_sp_new = 0.0_dp
                v1_int_gp_sp_new = 0.0_dp
                v2_int_gp_sp_new = 0.0_dp
                v1_int_gp_sp_new_nokid = 0.0_dp
                v2_int_gp_sp_new_nokid = 0.0_dp
            
                welf_agg_new = 0.0_dp
                welf_agg_new_nokid = 0.0_dp
                welf_agg_sp_new = 0.0_dp
                welf_agg_sp_new_nokid = 0.0_dp
                welf_agg_gp_sp_new = 0.0_dp
                welf_agg_gp_sp_new_nokid = 0.0_dp
                welf_agg_kc_new = 0.0_dp
                welf_agg_ks_new = 0.0_dp
                !welf_agg_xc_new = 0.0_dp
                !wght_x_vec = 0.0_dp
        
                wght_chk = 0.0_dp
                lsra_aggr = 0.0_dp
            
                grid_ass = 0.0_dp
                
                if (jc<jf-1)then
                    
                    mc_max = 1
                    wght_sng = 1.0_dp
                else
                    mc_max = 2
                    wght_sng = 0.5_dp / 0.75_dp
                endif

                phicpl(:) = 0.0_dp
                do sc=1,ns
                    phisng(sc) = sum(grid%Phi(:,:,:,:,:,:,2:2,:,:,sc,:,:,jc,tc))
                    do sc2=1,ns
                         if (sc>sc2)then
                            phicpl(sc) = phicpl(sc) + sum(grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,sc,sc2,jc,tc))
                         else
                             phicpl(sc2) = phicpl(sc2) + sum(grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,sc,sc2,jc,tc))
                         endif
         
                    enddo
   
                enddo

                do mc = 1,mc_max
                    
                    if (mc ==1)then
                        do sc=1,ns
           
                        do scpar=1,ns
                            do gcpar=1,1 !ng 
           
                                do hc=1,np
                    
        
                                    do kc=1,nk
                                        do gc=2,2 !2 !1,2 !ng
                                            do ic=1,ni
                                                do ec=1,ne
                                                    do yc=1,ny
                                                        do epsc=1,nw
                                                            do pc=1,np !1
                                                               ! do sc=1,ns
                                                                    do sc2 = 1,1!, !ns
                                                                    do xc=1,nx
                                                                        do dc=1,nd !l
                                                            
                                                                            ! tc is period 1 (init ss)
                                                                            ! tcc is period 2 (nt)
                                                                            
                                                                            frac_tot = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) *  wght_sng !* ns**(-1.0_dp)
                                                                            
                                                                            
                                                                            wght = frac_tot
                                                        
                                                                           
                                                                            
                                                                            if (wght==0.0_dp) cycle
                                                                        
                                                                            frac_qsc = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) /  phisng(sc)
                                                                            if (sc>sc2)then
                                                                                frac_sc = wght_sng * grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) /( phisng(sc)*wght_sng  + phicpl(sc)*(1.0_dp - wght_sng)  )
                                                                            else
                                                                                frac_sc = wght_sng * grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) /( phisng(sc)*wght_sng  + phicpl(sc2)*(1.0_dp - wght_sng)  )
                                                                            endif
                                                                            
                                                                          
                                                                           wght_s =   frac_sc
                                                                            
                                                                            
                                                                            
                                                                            wght_sp = wght_s 

                                                                            wght_qsp =  frac_qsc !* ns**(-1.0_dp)
                                                                                                                                                       wght_chk = wght_chk +wght
                                                                        
                                                                            !wght_s = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,:,:,:,:,:,:,:,:,sc,:,:,jc,tc))
                                                                
                                                                            wght_k= grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,:,:,:,:,kc,:,:,:,:,:,:,jc,tc))
                                                                
                                                                            xc_dec = fun_dec(grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pvec_ass)
                                                                
                                                                            wght_x= grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,xc,:,yc,:,kc,:,epsc,:,sc,:,:,jc,tc))
                                                                
                                                                
                                                                            wght_ks = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,:,:,:,:,kc,:,:,:,sc,:,:,jc,tc))
                                                                            ! base CEV 
                                                               
                                                                            
                                                                            
                                                                            v1_int_new_tmp_nokid =  pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                            
                                                                            v1_int_new_tmp = pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) -  v1_int_new_tmp_nokid
                                                                            v1_int_new_tmp = v1_int_new_tmp / nu_param
                                                                            
                                                                            v1_int_sp_new(sc) =v1_int_sp_new(sc)  + v1_int_new_tmp * wght_sp
                                                                            v1_int_gp_sp_new(1,sc) =v1_int_gp_sp_new(1,sc)  + v1_int_new_tmp * wght_qsp
                                                                            
                                                                           ! v1_int_sp_new_nokid(sc) =v1_int_sp_new_nokid(sc)  + v1_int_new_tmp_nokid * wght_sp
                                                                            v1_int_gp_sp_new_nokid(1,sc) =v1_int_gp_sp_new_nokid(1,sc)  + v1_int_new_tmp_nokid * wght_qsp
                                                                            
                                                                            v1_int_new = v1_int_new + v1_int_new_tmp * wght
                                                                            
                                                                            v1_int_new_nokid =v1_int_new_nokid +  pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght
                                                                            lsra_tr = 0.0_dp
                                                                            call fzero(lsra_tr,fun_lsra_ss,check_return)
                                                                            !print*, "computed lsratr", lsra_tr,check_return
                                                                
                                                                
                                                                            !v1_int_new = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc), & 
                                                                            !    grid_ass(xc),vals,inds,.false.,.false.)
                                                                
                                                                            ! new CEV
                                                                            tmp = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                                grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                            !v2_int_new = tmp   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                                            tmp_nokid = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%v_nokid(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                                grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                            
                                                                            
                                                                            tmp = tmp - tmp_nokid
                                                                            tmp = tmp / nu_param
                                                                            
                                                                            v2_int_new_nokid = v2_int_new_nokid + tmp_nokid * wght
                                                                            v2_int_new = v2_int_new + tmp * wght ! tmp - tmp_nokid
                                                                            
                                                                            v2_int_sp_new(sc) =v2_int_sp_new(sc)  + tmp * wght_sp
                                                                            v2_int_gp_sp_new(1,sc) =v2_int_gp_sp_new(1,sc)  + tmp * wght_qsp
                                                                            
                                                                           ! v2_int_sp_new_nokid(sc) =v2_int_sp_new_nokid(sc)  + tmp_nokid * wght_sp
                                                                            v2_int_gp_sp_new_nokid(1,sc) =v2_int_gp_sp_new_nokid(1,sc)  + tmp_nokid * wght_qsp
                                                                            
                                                                            !v2_int_new = v2_int_new / nu_param
                                                                            
                                                                            ! v2_int_new =  func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc), &
                                                                           !     grid%coh_base(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,vals,inds,.false.,.false.)!* grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc)! * agg%educ_matrix(scpar,sc,tc)/ sum(agg%educ_matrix(:,sc,tc) )
                                                                
                                                                            !if (grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) < grid%ass(dc,1,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc) )then
                                                                            !    cev_loc = 0.0_dp
                                                                            !else                                                                    
                                                                            cev_loc = fun_cev(v1_int_new,v2_int_new,afcev1)
                                                                            cev_loc_nokid = fun_cev(v1_int_new_nokid,v2_int_new_nokid,afcev1)
                                                                            labinctaxrv_base = pol%netinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) 
                                                                            labinctaxrv_new = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%netinc(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                            grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                    
                                                                            cons_base = pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                            cons_new = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%cons(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                            grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                            lab_base = pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                            lab_new =func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%lab(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                            grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                    
                                                                            if ( (labinctaxrv_new - labinctaxrv_base) > epsi )then ! i.e. if taxes to be paid are reduced in reform
                                                                                ind_benef = 1
                                                                               !if (cev_loc < 0.0_dp .and. jc>jt)then
                                                                               !    print*, "less taxes/ more tr, but welf loss?",cev_loc,labinctaxrv_base,labinctaxrv_new, cons_base,cons_new, lab_base, lab_new,jc,sc
                                                                               !endif
                                                                            !elseif (labinctaxrv_base<0.0_dp .and. labinctaxrv_new < 0.0_dp .and.  (abs(labinctaxrv_new) - abs(labinctaxrv_base)) > epsi )then ! i.e. if taxes to be paid are reduced in reform
                                                                            !    ind_benef = 1
                                                                            !   if (cev_loc < 0.0_dp)then
                                                                            !       print*, "more transfers, but welf loss?",labinctaxrv_base,labinctaxrv_new,  cons_base,cons_new, lab_base, lab_new,jc,sc
                                                                            !   endif   
                                                                            else
                                                                                ind_benef = 0
                                                                            endif
                                                                    
                                                                            !endif
                                                                            ! store only CEV of those who are net beneficiaries of the reform:
                                                                               ! if (cev_loc>0.0_dp)then
                                                                            welf_agg_new = welf_agg_new + wght * cev_loc
                                                                            welf_agg_new_nokid = welf_agg_new_nokid + wght * cev_loc_nokid
                                                                
                                                                            welf_agg_sp_new(sc) = welf_agg_sp_new(sc) + wght_sp * cev_loc 
                                                                            welf_agg_sp_new_nokid(sc) = welf_agg_sp_new_nokid(sc) + wght_sp * cev_loc_nokid 
                                                                            welf_agg_gp_sp_new(1,sc) = welf_agg_gp_sp_new(1,sc) + wght_qsp * cev_loc 
                                                                            welf_agg_gp_sp_new_nokid(1,sc) = welf_agg_gp_sp_new_nokid(1,sc) + wght_qsp * cev_loc_nokid
                                                                
                                                                            welf_agg_kc_new(kc) = welf_agg_kc_new(kc) + wght_k * cev_loc
                                                                
                                                                            welf_agg_ks_new(kc,sc) = welf_agg_ks_new(kc,sc) + wght_ks * cev_loc
                                                                           ! endif
                                                                          !  if (jc==js+2 .and. yc==1 .and. epsc==1 .and. kc==1 .and. sc==1)then
                                                                
                                                                            lsra_aggr = lsra_aggr + wght * lsra_tr
                                                                            !if (jc>js .and. jc<=jr(1)-1)then
                                                                            !    welf_agg_xc_new(xc_dec) = welf_agg_xc_new(xc_dec) + wght *cev_loc * wght_j
                                                                            !    wght_x_vec(xc_dec) = wght_x_vec(xc_dec) + wght * wght_j
                                                                            !endif
                                                                
                                                                
                                                                
                                                                            grid_ass(xc) = grid_ass(xc) + grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght
                                                                
                                                                            !v1_int_sp_new(sc) =v1_int_sp_new(sc)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght_s
                                                                            ! new CEV
                                                                            !v2_int_sp_new(sc) = v2_int_sp_new(sc)  + tmp * wght_s
                                                              
                                                                           ! wght_all = wght_all + wght
                                                                            enddo
                                                                        end do
                                                                    end do
                                                                end do
                                                            end do
                                                        enddo
                                        
                                                    end do
                                                enddo  
                                            end do
                                        enddo
                                    enddo
                                enddo
                            enddo
                        enddo
                        
                    else
                        
                        ! tbc couples
                        
                        !do scpar=1,ns
                        !    do gcpar=1,1 !ng 
           
                              !  do hc=1,np
                    
        
                                    do kc1=1,nk
                                        do kc2 = 1,nk
                                        !do gc=1,2 !ng
                                            do ic=1,1 !ni
                                                do ec=1,ne
                                                    do yc1=1,ny
                                                        do yc2=1,ny
                                                            do epsc1=1,nw
                                                                do epsc2 = 1,nw
                                                                    do pc=1,np !1
                                                                        do sc1=1,ns
                                                                            do sc2=1,ns
                                                                                do xc=1,nx
                                                                                    do dc=1,nd !l
                                                            
                                                                                        ! tc is period 1 (init ss)
                                                                                        ! tcc is period 2 (nt)
                                                                                        
                                                                                        frac_tot = grid%Phi_cpl(dc,xc,pc,yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2,jc,tc) * (1.0_dp - wght_sng)
                                                        
                                                                                        wght = frac_tot
                                                                                        
                                                                                        
                                                                                        
                                                                                        sc = max(sc1,sc2)
                                                                                        
                                                                                      
                                                                                        
                                                                                        if (wght==0.0_dp) cycle
                                                                                        
                                                                                        
                                                                                        frac_sc =(1.0_dp - wght_sng) * grid%Phi_cpl(dc,xc,pc,yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2,jc,tc)/ (  phicpl(sc) *(1.0_dp - wght_sng)   +   phisng(sc) *wght_sng  )
                                                                                        
                                                                                        frac_qsc = grid%Phi_cpl(dc,xc,pc,yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2,jc,tc)/phicpl(sc)
                                                                                        
                                                                                        wght_qsp = frac_qsc
                                                                                        
                                                                                        wght_sp = frac_sc
                                                                                        
                                                                                                                                                                                
                                                                                        wght_chk = wght_chk + wght
                                                                                        !wght_s = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,:,:,:,:,:,:,:,:,sc,:,:,jc,tc))
                                                                                        !
                                                                                        !wght_k= grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,:,:,:,:,kc,:,:,:,:,:,:,jc,tc))
                                                                                        !
                                                                                        !xc_dec = fun_dec(grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pvec_ass)
                                                                                        !
                                                                                        !wght_x= grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,xc,:,yc,:,kc,:,epsc,:,sc,:,:,jc,tc))
                                                                                        !
                                                                                        !
                                                                                        !wght_ks = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) / sum(grid%Phi(:,:,:,:,:,kc,:,:,:,sc,:,:,jc,tc))
                                                                                        !! base CEV 
                                                               
                                                                                        v1_int_new_tmp = pol%v_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) 
                                                                                        v1_int_new_tmp_nokid = pol%v_nokid_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
                                                                                        
                                                                                        v1_int_new_tmp = v1_int_new_tmp - v1_int_new_tmp_nokid
                                                                                        v1_int_new_tmp = v1_int_new_tmp / nu_param
                                                                                        
                                                                                        v1_int_new = v1_int_new + v1_int_new_tmp * wght
                                                                                        v1_int_new_nokid = v1_int_new_nokid + v1_int_new_tmp_nokid * wght
                                                                                        
                                                                                        v1_int_sp_new(sc) = v1_int_sp_new(sc) + v1_int_new_tmp * wght_sp
                                                                                       ! v1_int_sp_new_nokid(sc) = v1_int_sp_new_nokid(sc) + v1_int_new_nokid_tmp * wght_sp
                                                                                        
                                                                                        v1_int_gp_sp_new(2,sc) = v1_int_gp_sp_new(2,sc) + v1_int_new_tmp * wght_qsp
                                                                                        v1_int_gp_sp_new_nokid(2,sc) = v1_int_gp_sp_new_nokid(2,sc) + v1_int_new_tmp_nokid * wght_qsp
                                                                                        
                                                                                        lsra_tr = 0.0_dp
                                                                                        call fzero(lsra_tr,fun_lsra_ss,check_return)
                                                                                        !print*, "computed lsratr", lsra_tr,check_return
                                                                
                                                                
                                                                                        !v1_int_new = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc), & 
                                                                                        !    grid_ass(xc),vals,inds,.false.,.false.)
                                                                
                                                                                        ! new CEV
                                                                                        tmp = func_intp(grid%coh_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tcc),pol%v_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tcc), & 
                                                                                            grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc), vals,inds,.false.,.false.)
                                                                                        !v2_int_new = tmp   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                                                        tmp_nokid = func_intp(grid%coh_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tcc),pol%v_nokid_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tcc), & 
                                                                                            grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc), vals,inds,.false.,.false.)
                                                                                        !v2_int_new_nokid = tmp_nokid
                                                                                        
                                                                                        tmp = tmp - tmp_nokid
                                                                                        tmp = tmp / nu_param
                                                                                        
                                                                                        
                                                                                        v2_int_new = v2_int_new + tmp * wght
                                                                                        v2_int_new_nokid = v2_int_new_nokid + tmp_nokid * wght
                                                                                        
                                                                                        v2_int_sp_new(sc) = v2_int_sp_new(sc) + tmp * wght_sp
                                                                                       ! v2_int_sp_new_nokid(sc) = v2_int_sp_new_nokid(sc) + tmp_nokid * wght_sp
                                                                                        
                                                                                        v2_int_gp_sp_new(2,sc) = v2_int_gp_sp_new(2,sc) + tmp * wght_qsp
                                                                                        v2_int_gp_sp_new_nokid(2,sc) = v2_int_gp_sp_new_nokid(2,sc) + tmp_nokid * wght_qsp
                                                                                        
                                                                                        ! v2_int_new =  func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc), &
                                                                                       !     grid%coh_base(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,vals,inds,.false.,.false.)!* grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc)! * agg%educ_matrix(scpar,sc,tc)/ sum(agg%educ_matrix(:,sc,tc) )
                                                                
                                                                                        !if (grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) < grid%ass(dc,1,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc) )then
                                                                                        !    cev_loc = 0.0_dp
                                                                                        !else                                                                    
                                                                                        cev_loc = fun_cev(v1_int_new,v2_int_new,afcev1)
                                                                                        cev_loc_nokid = fun_cev(v1_int_new_nokid,v2_int_new_nokid,afcev1)
                                                                                        !labinctaxrv_base = pol%netinc_cpl(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) 
                                                                                        !labinctaxrv_new = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%netinc(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                                        !grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                                        !
                                                                                        !cons_base = pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                                        !cons_new = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%cons(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                                        !grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                                        !lab_base = pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                                        !lab_new =func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%lab(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                                        !grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),vals,inds,.false.,.false.)
                                                                                        !
                                                                                        !if ( (labinctaxrv_new - labinctaxrv_base) > epsi )then ! i.e. if taxes to be paid are reduced in reform
                                                                                        !    ind_benef = 1
                                                                                        !   !if (cev_loc < 0.0_dp .and. jc>jt)then
                                                                                        !   !    print*, "less taxes/ more tr, but welf loss?",cev_loc,labinctaxrv_base,labinctaxrv_new, cons_base,cons_new, lab_base, lab_new,jc,sc
                                                                                        !   !endif
                                                                                        !!elseif (labinctaxrv_base<0.0_dp .and. labinctaxrv_new < 0.0_dp .and.  (abs(labinctaxrv_new) - abs(labinctaxrv_base)) > epsi )then ! i.e. if taxes to be paid are reduced in reform
                                                                                        !!    ind_benef = 1
                                                                                        !!   if (cev_loc < 0.0_dp)then
                                                                                        !!       print*, "more transfers, but welf loss?",labinctaxrv_base,labinctaxrv_new,  cons_base,cons_new, lab_base, lab_new,jc,sc
                                                                                        !!   endif   
                                                                                        !else
                                                                                        !    ind_benef = 0
                                                                                        !endif
                                                                    
                                                                                        !endif
                                                                                      !  ! store only CEV of those who are net beneficiaries of the reform:
                                                                                      !     ! if (cev_loc>0.0_dp)then
                                                                                        welf_agg_new = welf_agg_new + wght * cev_loc
                                                                                        welf_agg_new_nokid = welf_agg_new_nokid + wght * cev_loc_nokid 
                                                                                        
                                                                                        welf_agg_sp_new(sc) = welf_agg_sp_new(sc) + wght_sp * cev_loc 
                                                                                        welf_agg_sp_new_nokid(sc) = welf_agg_sp_new_nokid(sc) + wght_sp * cev_loc_nokid 
                                                                                        welf_agg_gp_sp_new(2,sc) = welf_agg_gp_sp_new(2,sc) + wght_qsp * cev_loc 
                                                                                        welf_agg_gp_sp_new_nokid(2,sc) = welf_agg_gp_sp_new_nokid(2,sc) + wght_qsp * cev_loc_nokid
                                                                                      !
                                                                                      !  welf_agg_sp_new(sc) = welf_agg_sp_new(sc) + wght_s * cev_loc 
                                                                                      !
                                                                                      !  welf_agg_kc_new(kc) = welf_agg_kc_new(kc) + wght_k * cev_loc
                                                                                      !
                                                                                      !  welf_agg_ks_new(kc,sc) = welf_agg_ks_new(kc,sc) + wght_ks * cev_loc
                                                                                      ! ! endif
                                                                                      !!  if (jc==js+2 .and. yc==1 .and. epsc==1 .and. kc==1 .and. sc==1)then
                                                                                      !
                                                                                        lsra_aggr = lsra_aggr + wght * lsra_tr
                                                                                        !if (jc>js .and. jc<=jr(1)-1)then
                                                                                        !    welf_agg_xc_new(xc_dec) = welf_agg_xc_new(xc_dec) + wght *cev_loc * wght_j
                                                                                        !    wght_x_vec(xc_dec) = wght_x_vec(xc_dec) + wght * wght_j
                                                                                        !endif
                                                                
                                                                
                                                                
                                                                                        !!grid_ass(xc) = grid_ass(xc) + grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght
                                                                                        !
                                                                                        !v1_int_sp_new(sc) =v1_int_sp_new(sc)  + pol%v_cpl(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc) * wght_s
                                                                                        !! new CEV
                                                                                        !v2_int_sp_new(sc) = v2_int_sp_new(sc)  + tmp * wght_s
                                                              
                                                                                       ! wght_all = wght_all + wght
                                                                                end do
                                                                            end do
                                                                        end do
                                                                    end do
                                                                enddo
                                        
                                                            end do
                                                        enddo  
                                                    end do
                                                enddo
                                            enddo
                                        enddo
                                    enddo
                                enddo
                        
                             endif
                    
                     
                        
                enddo
           ! ! compute CEV 
                
                
            welf_agg_new = fun_cev(v1_int_new,v2_int_new,afcev1) 
            welf_agg_new_nokid = fun_cev(v1_int_new_nokid,v2_int_new_nokid,afcev1)
            do sc = 1,ns
                welf_agg_sp_new(sc) = fun_cev(v1_int_sp_new(sc),v2_int_sp_new(sc),afcev1)
                welf_agg_gp_sp_new(1,sc) = fun_cev(v1_int_gp_sp_new(1,sc),v2_int_gp_sp_new(1,sc),afcev1)
                welf_agg_gp_sp_new(2,sc) = fun_cev(v1_int_gp_sp_new(2,sc),v2_int_gp_sp_new(2,sc),afcev1)
                !welf_agg_sp_new_nokid(sc) = fun_cev(v1_int_sp_new_nokid(sc),v2_int_sp_new_nokid(sc),afcev1)
                
                welf_agg_gp_sp_new_nokid(1,sc) = fun_cev(v1_int_gp_sp_new_nokid(1,sc),v2_int_gp_sp_new_nokid(1,sc),afcev1)
                welf_agg_gp_sp_new_nokid(2,sc) = fun_cev(v1_int_gp_sp_new_nokid(2,sc),v2_int_gp_sp_new_nokid(2,sc),afcev1)
            enddo
            
           if (abs(1.0_dp - wght_chk) > epsi) print*, "wghtcheck cur alive", wght_chk,jc
        
            !welf_agg_new = fun_cev(v1_int_new,v2_int_new,afcev1) 
            welf_agg_new = welf_agg_new * 100.0_dp
            welf_agg_new_nokid = welf_agg_new_nokid * 100.0_dp
            welf_agg_sp_new = welf_agg_sp_new * 100_dp
            welf_agg_sp_new_nokid = welf_agg_sp_new_nokid * 100_dp
            welf_agg_gp_sp_new = welf_agg_gp_sp_new * 100_dp
            welf_agg_gp_sp_new_nokid = welf_agg_gp_sp_new_nokid * 100_dp
            welf_agg_kc_new = welf_agg_kc_new * 100_dp
            welf_agg_ks_new = welf_agg_ks_new * 100_dp
           ! welf_agg_xc_new = welf_agg_xc_new * 100.0_dp
            
            !welf_xj(:,jc) = welf_agg_xc_new
            !wght_x_vec_j(:,jc) = wght_x_vec
            
            
            !do sc = 1,ns
            !    welf_agg_sp_new(sc) = fun_cev(v1_int_sp_new(sc),v2_int_sp_new(sc),afcev1)
            !    welf_agg_sp_new(sc) = welf_agg_sp_new(sc) * 100.0_dp
            !enddo
            
        
           ! print*,"WELFF" ,welf_agg_new !, v1_int_new,v2_int_new,afcev1, sum( grid%Phi_base ) !,pol%v_base(1,:,1,1,1,1,1,:,1,1,1,1,jstud(3),tc)
            !pause
            agg%welf_j(jc) = welf_agg_new
            agg%welf_j_nokid(jc) = welf_agg_new_nokid
           ! print*, "av welf", sum(agg%welf_j(js:nj))/(nj-js+1)
            
            !print*, "ass distr",jc,  wght_x_vec
            !print*, "cev x", welf_agg_xc_new
            !pause
         
            write(89,'(11f50.16)') welf_agg_new, welf_agg_new_nokid ,  welf_agg_sp_new(1), welf_agg_sp_new(2),welf_agg_sp_new(3),welf_agg_sp_new(4) ,  welf_agg_sp_new_nokid(1), welf_agg_sp_new_nokid(2),welf_agg_sp_new_nokid(3),welf_agg_sp_new_nokid(4),lsra_aggr
            write(899,'(8f50.16)') welf_agg_gp_sp_new(1,1),welf_agg_gp_sp_new(1,2), welf_agg_gp_sp_new(1,3), welf_agg_gp_sp_new(1,4),welf_agg_gp_sp_new(2,1),welf_agg_gp_sp_new(2,2), welf_agg_gp_sp_new(2,3), welf_agg_gp_sp_new(2,4)
            write(8992,'(8f50.16)') welf_agg_gp_sp_new_nokid(1,1),welf_agg_gp_sp_new_nokid(1,2), welf_agg_gp_sp_new_nokid(1,3), welf_agg_gp_sp_new_nokid(1,4),welf_agg_gp_sp_new_nokid(2,1),welf_agg_gp_sp_new_nokid(2,2), welf_agg_gp_sp_new_nokid(2,3), welf_agg_gp_sp_new_nokid(2,4)
            
            !write(33,'(4f50.16)') welf_agg,  welf_agg_sp(1), welf_agg_sp(2),welf_agg_sp(3) 
            agg%lsra_j(jc) = lsra_aggr
       
            enddo
            
        ! average across ages
            welfj_mean = 0.0_dp
            welfj_mean_nokid = 0.0_dp
            do jc=js,nj
            
                welfj_mean = welfj_mean + agg%pop_j(jc,tc)/sum(agg%pop_j(:,tc)) * agg%welf_j(jc)
                welfj_mean_nokid = welfj_mean_nokid + agg%pop_j(jc,tc)/sum(agg%pop_j(:,tc)) * agg%welf_j_nokid(jc)
            enddo
             
            
        write(8911,'(1f50.16)') welfj_mean !sum(agg%welf_j(js:nj))/(nj-js+1)
        do xc_dec = 1,10
            write(893,'(3f50.16)') welf_agg_xc_new(xc_dec) * 100.0_dp, wght_x_vec(xc_dec), welf_agg_xc_new(xc_dec) * 100.0_dp/ wght_x_vec(xc_dec)
        enddo
        
        close ( 89)
        close ( 899)
        close ( 8992)
        close ( 893)
        close ( 8911)
        
        lsra_aggr = 0.0_dp
        do jc = js,nj !, jr(1)
            lsra_aggr = lsra_aggr + agg%lsra_j(jc) * agg%pop_j(jc,tc)/sum(agg%pop_j(:,tc))
        enddo
        print*, "lsra cur alive", agg%lsra_j,lsra_aggr
        print*, agg%pop_j(:,1)
        
        !do jc=js,nj-3
        !    wght_j = demo%sr(jc,1)/sum(demo%sr(js:nj,1))
        !    welf_agg_xc_full(:) = welf_agg_xc_full(:) + wght_j * welf_xj(:,jc)
        !    wght_x_vec_full(:) = wght_x_vec_full(:) + wght_j * wght_x_vec_j(:,jc)
        !enddo
        
        
        !print*, "ass", grid_ass
        !print*, "assd", wght_x_vec_full
        !print*, "cevass", welf_agg_xc_full
        
        !print*, "ass distr",wght_x_vec
        !print*, "cev x", welf_agg_xc_new
        !print*, "cev x cond", welf_agg_xc_new/wght_x_vec
        !pause
        
    !endif
    
    !do xc=1,nx
    !    print*, "xc ", xc, sum(grid%Phi(:,xc,:,:,:,:,:,:,:,:,:,:,jt,1))
    !    
    !enddo    
        
    !pause
endif

contains

 
! ------------------------------------------------------------------------------
function fun_lsra(tr)

real(dp):: fun_lsra
!real(dp),intent(in):: v1,v2(:),coh2(:)
real(dp),intent(in):: tr
real(dp):: v2tild
real(dp)::assmin,wght_j,welf_agg_xc_full(nx),welf_xj(nx,nj),grid_ass(nx),wght_x,gr_rho_p(nt),cev_loc,afcev,afcev1,v1_int,welf_agg,dist_phi,coh,prodret,ass,v2_int,v1_int_sp(ns),v2_int_sp(ns),welf_agg_sp(ns),welf_agg_gp_sp(ng,ns),welf_agg_gp(ng), &
    v1_int_gp_sp(ng,ns),v2_int_gp_sp(ng,ns),  v1_int_gp(ng),v2_int_gp(ng), v1_int_y(ny),v2_int_y(ny),welf_agg_y(ny), welf_agg_x(5),welf_agg_x_new(5),v1_int_x_new(5),v2_int_x_new(5)
real(dp):: v1_int_gp_sp_new(ng,ns),v2_int_gp_sp_new(ng,ns),  v1_int_gp_new(ng),v2_int_gp_new(ng),welf_agg_xc_new(nx),wght_x_vec(nx),wght_x_vec_full(nx),wght_x_vec_j(nx,nj), v1_int_y_new(ny),v2_int_y_new(ny),welf_agg_y_new(ny)
real(dp)::  v1_int_new,welf_agg_new,v2_int_new,v1_int_sp_new(ns),v2_int_sp_new(ns),welf_agg_sp_new(ns),welf_agg_kc_new(nk),welf_agg_ks_new(nk,ns),welf_agg_gp_sp_new(ng,ns),welf_agg_gp_new(ng),v1_int_gsx(2,ns,5),v2_int_gsx(2,ns,5), welf_agg_gsx(2,ns,5)
real(dp):: coh_glb_min,coh_glb_max,coh_grid(nx),v1_int_ds(nd,ns),v2_int_ds(nd,ns),logsum,logsum_base,prob_kid(ns),prob_kid_base(ns),ass_glb_max,ass_glb_min,ass_grid(nx),v1_int_x(5),v2_int_x(5), wght_chk,wght,wght_s,wght_k,wght_ks
integer::inds(2)
real(dp)::vals(2)
integer::xc,pc,yc,ec,ic,kc,sc,dc,gc,scc,icc,sc1,sc2,kc1,kc2,zc,hc,scpar,gcpar,xcpar,ycpar,epsc,jmin,jmax,xc_dec, ind_benef,mc_max,mc,yc1,yc2,epsc1,epsc2
real(dp):: labinctaxrv_base, labinctaxrv_new, cons_base, cons_new, lab_base, lab_new
real(dp):: quint_frac_exper_new(5),quint_frac_exper(5),quint_frac_base_new(5),quint_frac_base(5),tmp,Phi_scpar(ns),Phi_scown(ns),v2_int_phi0,logsum_finvf,val_sc_initvf(ns),val_sc_finvf(ns),wghttmp,welf_agg_phi0, & 
    lsra_tr,lsra_aggr,prob_fin_hs(2),prob_fin(2),val_col,prob_col(2),ev_kid_choice(2),prob_sc(ns),probfe(nk),wght_sng, pdvterm,pdv_term

!integer,intent(in):: xc

!v2tild = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)+tr,vals,inds,.false.,.false.)
!
!fun_lsra = pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) - v2tild

welf_agg = 0.0_dp
v1_int = 0.0_dp
v2_int = 0.0_dp
v1_int_sp = 0.0_dp
v2_int_sp = 0.0_dp
v1_int_gp_sp = 0.0_dp 
v2_int_gp_sp = 0.0_dp
v1_int_gp = 0.0_dp
v2_int_gp = 0.0_dp
v1_int_y = 0.0_dp
v2_int_y = 0.0_dp
v1_int_x = 0.0_dp
v2_int_x = 0.0_dp
        
welf_agg_new = 0.0_dp
v1_int_new = 0.0_dp
v2_int_new = 0.0_dp
v1_int_sp_new = 0.0_dp
v2_int_sp_new = 0.0_dp
v1_int_gp_sp_new = 0.0_dp
v2_int_gp_sp_new = 0.0_dp
v1_int_gp_new = 0.0_dp
v2_int_gp_new = 0.0_dp  
v1_int_y_new = 0.0_dp
v2_int_y_new = 0.0_dp
v1_int_x_new = 0.0_dp
v2_int_x_new = 0.0_dp
        
v1_int_gsx = 0.0_dp
v2_int_gsx = 0.0_dp
        
quint_frac_exper_new = 0.0_dp
quint_frac_exper = 0.0_dp
quint_frac_base_new = 0.0_dp
quint_frac_base = 0.0_dp
        
wght_chk = 0.0_dp
        
    do scpar=1,ns
        Phi_scpar(scpar)= sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,scpar,js,1))
        do gcpar=1,1 !  !,2 !ng
           
            do hc=1,np
                do kc=1,nk
                    do gc=1,2 !ng
                        do ic=1,ni
                            do ec=1,ne
                                do yc=1,ny
                                    do epsc=1,nw
                                        do pc=1,np
                                            do sc=1,ns
                                                do xc=1,nx
                                                    do dc=1,nd !l
                                                        
                                                        wght = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,1) 
                                                        wght_s = wght / Phi_scpar(scpar)
                                                        if (wght==0.0_dp) cycle
                                                                
                                                        ! new CEV
                                                        tmp = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,1), & 
                                                            grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1)+tr,vals,inds,.false.,.false.)
                                                        v1_int_new = v1_int_new  +tmp* wght   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                        v1_int_sp_new(scpar) = v1_int_sp_new(scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,1) * wght_s
                                                        tmp = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc), & 
                                                            grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1),vals,inds,.false.,.false.)
                                                        v2_int = v2_int  + tmp * wght 
                                                        v2_int_sp(scpar) = v2_int_sp(scpar)  + tmp * wght_s 
                                                        ! v2_int_new =  func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc), &
                                                        !     grid%coh_base(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,vals,inds,.false.,.false.)!* grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc)! * agg%educ_matrix(scpar,sc,tc)/ sum(agg%educ_matrix(:,sc,tc) )
                                                        !cev_loc = fun_cev(pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1),pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),afcev1) 
                                                        !  welf_agg = welf_agg + wght * cev_loc
                                                                
                                                              
                                           
                                                    end do
                                                end do
                                            end do
                                        end do
                                    enddo
                                        
                            end do
                        enddo  
                    end do
                enddo
            enddo
        enddo
            enddo
        enddo
        
      !  print*, "wght", wght_chk
        
        wght_chk = 0.0_dp
        
        do scpar=1,ns
            Phi_scpar(scpar)= sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,scpar,js,tc))
            do gcpar=1,1 !ng
           
                do hc=1,np
                    
        
                    do kc=1,nk
                        do gc=1,2 !ng
                            do ic=1,ni
                                do ec=1,ne
                                    do yc=1,ny
                                        do epsc=1,nw
                                        do pc=1,np !1
                                            do sc=1,ns
                                                do xc=1,nx
                                                    do dc=1,nd !l
                                                        
                                                            wght = grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) 
                                                            wght_s = wght / Phi_scpar(scpar)
                                                            if (wght==0.0_dp) cycle
                                                            wght_chk = wght_chk + grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) 
                                                            
                                                            
                                                            ! new CEV
                                                            tmp =func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc),grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc),vals,inds,.false.,.false.)
                                                            v2_int_new = v2_int_new  + tmp * wght   !* grid%Phi_base(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc) !* agg%educ_matrix_base(scpar,sc,tc)/ sum(agg%educ_matrix_base(:,sc,tc) )
                                                            v2_int_sp_new(scpar) = v2_int_sp_new(scpar)  + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc) * wght_s
                                                            ! v2_int_new =  func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc), &
                                                           !     grid%coh_base(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jstud(3),tc) ,vals,inds,.false.,.false.)!* grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,js,tc)! * agg%educ_matrix(scpar,sc,tc)/ sum(agg%educ_matrix(:,sc,tc) )
                                                            !cev_loc = fun_cev(v1_int_new,v2_int_new,afcev1) 
                                                           ! welf_agg_new = welf_agg_new + wght * cev_loc
                                                                
                                                              
                                           
                                                    end do
                                                end do
                                            end do
                                        end do
                                    enddo
                                        
                            end do
                        enddo  
                    end do
                enddo
            enddo
        enddo
            enddo
        enddo
        
        

        fun_lsra = v1_int_new - v2_int_new
        
end function fun_lsra
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
function fun_lsra_ss(tr)

real(dp):: fun_lsra_ss
!real(dp),intent(in):: v1,v2(:),coh2(:)
real(dp),intent(in):: tr
real(dp):: v2tild,vals(2),v1tild
integer:: inds(2)
!integer,intent(in):: xc

!v2tild = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc),grid%coh(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)+tr,vals,inds,.false.,.false.)

v2tild = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc), & 
                                                                    grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1) + tr,vals,inds,.false.,.false.)

v1tild = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,1), & 
                                                                    grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc) + tr,vals,inds,.false.,.false.)


fun_lsra_ss = pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tcc) - v1tild

       
end function fun_lsra_ss
! ------------------------------------------------------------------------------

    
end subroutine sub_welfare
! ------------------------------------------------------------------- 

! ------------------------------------------------------------------- 
function fun_dec(x_in,pvec)
real(dp):: fun_dec
real(dp),intent(in):: x_in, pvec(:)
integer:: np,pc

np = size(pvec,1)

if (x_in<=pvec(1))then
    fun_dec = 1
elseif (x_in >pvec(np) )then
    fun_dec = np+1
endif

do pc=2,np

    if (x_in >pvec(pc-1) .and. x_in <=pvec(pc) )then
        fun_dec = pc
    endif
    
enddo


end function
! ------------------------------------------------------------------- 


! -----------------------------------------------------------------
subroutine sub_sumstats(agg,grid,pol,stat,t0,t1)
! organizes computation of summary statistics
implicit none

type(t_agg),intent(inout)::agg
type(t_grid),intent(inout)::grid
type(t_pol),intent(in)::pol
type(t_stat),intent(inout)::stat
integer,intent(in)::t0,t1

real(dp),allocatable,dimension(:)::distrvec,yvec,distrvec1,distrvec2,yvec1,yvec2
real(dp),allocatable,dimension(:)::lorx,lory
real(dp),parameter::epsi=1.0e-08_dp
real(dp),parameter::min_inc=0.05
real(dp)::cvy0,vary0,mean_y,med_y,toty,frac_med,p10,p90,pvec(9),qvec(4)
integer::tc,tcc,ic
integer:: nh
integer::ntg
real(dp),allocatable:: Phi_child_loc(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), Ass_child_loc(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), Inc_child_loc(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), & 
    NetInc_child_loc(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)!,distrvec_kc(:,:,:)
integer:: scpar,sc,hc,kc,gc,ec,xc,dc,yc,pc,jc,epsc,dc_loc,mc,yc2,kc2,epsc2,sc2
character(1)::i_char1, i_char2, i_char3

print*, ' '
print*, '----------------------------------------'
print*, 'Computation of Summary Statistics'
print*, '----------------------------------------'
print*, ' '

! allocate objects of Lorenz curve and Gini
ntg=1 !(t2g-t1g)/tg_delt+1

if (opt_trref==0 .and. t0==1 .and. opt_vf_init==1)then

    allocate(stat%gini_ass(ntg))
    allocate(stat%varlog_ass(ntg))
    allocate(stat%gini_cons(ntg))
    allocate(stat%gini_labrv(ntg))
    allocate(stat%varlog_cons(ntg))
    allocate(stat%gini_inc(ntg))
    allocate(stat%varlog_inc(ntg)) 

endif

if (opt_sumstats==1 .or. opt_sumstats_loc==1)then

    allocate(Phi_child_loc(2,nd,nx,np,ny,ny,nw,nw,nk,nk,ng,np,ns,ns,ns,nj))
    allocate(Ass_child_loc(2,nd,nx,np,ny,ny,nw,nw,nk,nk,ng,np,ns,ns,ns,nj))
    allocate(Inc_child_loc(2,nd,nx,np,ny,ny,nw,nw,nk,nk,ng,np,ns,ns,ns,nj))
    allocate(NetInc_child_loc(2,nd,nx,np,ny,ny,nw,nw,nk,nk,ng,np,ns,ns,ns,nj))
    
   ! allocate(distrvec_kc(nh,ns,nk) )

    
    do tc=t0,t1 !t1g,t2g,tg_delt       ! compute Gini, etc. only every tg_delt years for years from t1g to t2g
    tcc=tc
    
        ! ********************************
        Phi_child_loc = 0.0_dp
        Ass_child_loc = 0.0_dp
        Inc_child_loc = 0.0_dp
        NetInc_child_loc = 0.0_dp
   
        do jc=js,nj
            
            mc=1
            yc2 =1
            epsc2 = 1
            kc2 = 1
            sc2 = 1
            do scpar = 1,ns
                do sc=1,ns
                    do hc=1,np
                        do pc=1,np
                            do kc=1,nk
                                !do gc=1,ng
                                    do ec=1,ne
                                        do epsc=1,nw
                                            do yc=1,ny
                                                do xc=1,nx
                                                    do dc=1,nd
                                                        dc_loc = dc!  1
                                                        if (jc<jf-1)then
                                                            yc2 =1
                                                            epsc2 = 1
                                                            kc2 = 1
                                                            sc2 = 1
                                                            do gc = 1,ng
                                                                Phi_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) = Phi_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc)
                                                        
                                                                Ass_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) = Ass_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) +grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                Inc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =Inc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + pol%grwage(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                NetInc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =NetInc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + pol%netinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                            enddo
                                                            
                                                        else
                                                            do mc=1,2
                                                               
                                                            if (mc==1)then ! single
                                                                do gc = 1,ng
                                                                    yc2 =1
                                                                    epsc2 = 1
                                                                    kc2 = 1
                                                                    sc2 = 1
                                                                    Phi_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) = Phi_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + grid%Phi(dc,xc,pc,yc,ec,kc,gc,epsc,hc,sc,1,scpar,jc,tc) * probmar_s(1,gc,sc)
                                                                    Ass_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =Ass_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc)  + grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                    Inc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =Inc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc)+ pol%grwage(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                    NetInc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =NetInc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + pol%netinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,jc,tc)
                                                                enddo
                                                                
                                                            else
                                                                gc = 1
                                                                do sc2=1,ns
                                                                    do kc2=1,nk
                                                                        do epsc2=1,nw
                                                                            do yc2=1,ny
                                                                                Phi_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) = Phi_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + grid%Phi_cpl(dc,xc,pc,yc,yc2,ec,kc,kc2,epsc,epsc2,sc,sc2,jc,tc) * probmar_s(2,gc,sc)
                                                                                Ass_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) = Ass_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc)  +grid%ass_cpl(dc,xc,pc,yc,yc2,ec,epsc,epsc2,kc,kc2,sc,sc2,jc,tc)
                                                                                Inc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =Inc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + pol%grwage_cpl(dc,xc,pc,yc,yc2,ec,epsc,epsc2,kc,kc2,sc,sc2,jc,tc)
                                                                                NetInc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) =NetInc_child_loc(mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) + pol%netinc_cpl(dc,xc,pc,yc,yc2,ec,epsc,epsc2,kc,kc2,sc,sc2,jc,tc)
                                                                            enddo
                                                                        enddo
                                                                    enddo
                                                                enddo
                                                                
                                                            endif
                                                            enddo
                                                                
                                                        endif
                                                        
                                                    enddo
                                                enddo
                                            enddo
                                        enddo
                                    enddo
                               ! enddo
                            enddo
                        enddo
                    enddo
        
                enddo
    
            enddo
        enddo
        
            ! grid%Phi(nd,nx,np,ny,ne,nk,ng,nw,np,ns,ni,ns,nj,nt)
   
        !Phi_child_loc(:,:,:,:,:,:,:,:,:,js+1:jr(1)-1,tc) = Phi_child_loc(:,:,:,:,:,:,:,:,:,js+1:jr(1)-1,tc) / (jr(1) - (js+1)  )
    
        !print*, "distr", sum(Phi_child_loc(:,:,:,:,:,:,:,:,:,js+1:jr(1)-5)) !, sum(grid%Phi(:,:,:,y,1,kc,1,epsc,hc,sc,1,1,jc,tc) )
    
        ! asset gini
    
         
        ! WAGE VARS
        ! (mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) 
        call sub_vectorize( Phi_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jr(1)-1) ,Inc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jr(1)-1),distrvec,yvec,nh,.true.)
        
        !print*, "vectorized W" 
        distrvec = distrvec / (jr(1) - 1 -  (js +1) +1 )
        print*, "sum distrvec", sum(distrvec) !, Inc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jf-1)
        yvec=max(epsi,yvec)
        ! compute variance of logs, coefficient of variation, median 
        call sub_varcv(distrvec,log(yvec),stat%varlog_ass(1),cvy0,med_y,pvec,qvec_inc,pvec_inc_frac,qvec_inc_frac,1,toty_inc)
        
        if (opt_trref == 0) qvec_inc_base = qvec_inc
        
        print*, "var of grwageinc", stat%varlog_ass(1),cvy0,pvec,med_y
        
        
        print*, "marg tax rate of median earner", 1.0_dp - (1.0_dp - tau_pr) * lambda_pr * (0.33_dp* med_y)**(-tau_pr)
        
        ! WAGE VARS
        ! (mc,dc_loc,xc,pc,yc,yc2,epsc,epsc2,kc,kc2,gc,hc,sc,sc2,scpar,jc) 
        call sub_vectorize( Phi_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,1:ns-1,:,1,js+1:jr(1)-1) ,Inc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,1:ns-1,:,1,js+1:jr(1)-1),distrvec,yvec,nh,.true.)
        
        !print*, "vectorized W" 
        distrvec = distrvec / (jr(1) - 1 -  (js +1) +1 )
        print*, "sum distrvec", sum(distrvec) !, Inc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jf-1)
        yvec=max(epsi,yvec)
        ! compute variance of logs, coefficient of variation, median 
        call sub_varcv(distrvec,log(yvec),stat%varlog_ass(1),cvy0,med_y,pvec,qvec_inc,pvec_inc_frac,qvec_inc_frac,1,toty_inc)
        
        if (opt_trref == 0) qvec_inc_base = qvec_inc
        
        print*, "var of grwage non col", stat%varlog_ass(1),pvec,med_y
        
        call sub_vectorize( Phi_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,ns:ns,:,1,js+1:jr(1)-1) ,Inc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,ns:ns,:,1,js+1:jr(1)-1),distrvec,yvec,nh,.true.)
        
        !print*, "vectorized W" 
        distrvec = distrvec / (jr(1) - 1 -  (js +1) +1 )
        print*, "sum distrvec", sum(distrvec) !, Inc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jf-1)
        yvec=max(epsi,yvec)
        ! compute variance of logs, coefficient of variation, median 
        call sub_varcv(distrvec,log(yvec),stat%varlog_ass(1),cvy0,med_y,pvec,qvec_inc,pvec_inc_frac,qvec_inc_frac,1,toty_inc)
        
        if (opt_trref == 0) qvec_inc_base = qvec_inc
        
        print*, "var of grwage COL", stat%varlog_ass(1),pvec,med_y
            
        if (tcc==1) then
            allocate(lorx(nh))
            allocate(lory(nh))
        endif
        
        !! compute gini
        !call sub_gini(distrvec,yvec,nh,stat%gini_ass(tcc) ) !,toty ) !,.true.)
        !
        !print*, "gini income all", stat%gini_ass(tcc)
            
        
        if (t0==1)then    
            call sub_vectorize( Phi_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:nj-2),Ass_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:nj-2),distrvec,yvec,nh,.true.)
        
            print*, "vectorized"
            !pause
        
            distrvec = distrvec / (nj-2 -  (js +1) +1 )
            call sub_varcv(distrvec,yvec,stat%varlog_ass(1),cvy0,med_y,pvec_ass,qvec,pvec_ass_frac,qvec_ass_frac,0,toty_ass)

            print*, "var of log ass", stat%varlog_ass(1),pvec_ass,med_y
            
            print*, "qvec", qvec
            
            print*, "qvecfrac", qvec_ass_frac, toty_ass
            
            print*, "pvecfrac", pvec_ass_frac
            
            !! compute gini
            !call sub_gini(distrvec,yvec,nh,stat%gini_ass(tcc) ) !,toty ) !,.true.)
            !
            !print*, "gini all", stat%gini_ass(tcc)
            
        endif
        
        !! GINI
        ! if (tcc==1) then
        !    allocate(lorx(nh))
        !    allocate(lory(nh))
        ! endif
        !
        !! compute gini
        !call sub_gini(distrvec,yvec,nh,stat%gini_ass(tcc) ) !,toty ) !,.true.)
        !
        !print*, "gini all", stat%gini_ass(tcc)
        
        !call sub_vectorize( Phi_child_loc(:,:,:,:,:,:,:,1:2,:,js+5:jr(1)-1),pol%grwageinc(:,:,:,:,1,:,1,:,:,1:2,1,:,js+5:jr(1)-1,tc),distrvec,yvec,nh,.true.)
        !distrvec = distrvec / (jr(1)-1 - js-5+1)
        !distrvec = distrvec /sum(Phi_child_loc(:,:,:,:,:,:,:,1:2,:,js+5:jr(1)-1))
        !
        !call sub_varcv(distrvec,log(yvec),stat%varlog_ass(1),cvy0,med_y,p10,p90)
        !
        !print*, "var of log earnings nonCL", stat%varlog_ass(1),p10,p90,med_y
        !
        !call sub_vectorize( Phi_child_loc(:,:,:,:,:,:,:,3:3,:,js+5:jr(1)-1),pol%grwageinc(:,:,:,:,1,:,1,:,:,3:3,1,:,js+5:jr(1)-1,tc),distrvec,yvec,nh,.true.)
        !distrvec = distrvec / (jr(1)-1 - js-5 + 1)
        !distrvec = distrvec /sum(Phi_child_loc(:,:,:,:,:,:,:,3,:,js+5:jr(1)-1))
        !
        !call sub_varcv(distrvec,log(yvec),stat%varlog_ass(1),cvy0,med_y,p10,p90)
        !
        !print*, "var of log earnings CL", stat%varlog_ass(1),p10,p90,med_y
        !
        !call sub_vectorize( Phi_child_loc(:,:,:,:,:,:,:,:,:,jr(1)-5:jr(1)-5),pol%netinc(:,:,:,:,1,:,1,:,:,:,1,:,jr(1)-5:jr(1)-5,tc),distrvec,yvec,nh,.true.)
        !
        !
        
       ! call sub_vectorize( Phi_child_loc(:,:,:,:,:,:,:,:,:,js+5:js+5),pol%netinc(:,:,:,:,1,:,1,:,:,:,1,:,js+5:js+5,tc),distrvec,yvec,nh,.true.)
        call sub_vectorize( Phi_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jr(1)-1),NetInc_child_loc(:,1:ndl,:,:,:,:,:,:,:,:,:,1,:,:,1,js+1:jr(1)-1),distrvec,yvec,nh,.true.)
        distrvec = distrvec / (jr(1) - 1 -  (js +1) +1 )
        
        ! determine POVERTY line: net income 
        if (opt_trref==0 .and. t0==1)then
            ! compute median
            frac_med = 0.0_dp
            xc=1
            do while (frac_med<0.5_dp)
                frac_med = frac_med + distrvec(xc)
                xc= xc+1
            enddo
            stat%incmed(tclb) = yvec(xc-1)
            print*, "median is ", stat%incmed(tclb)
        endif
        
        ! fraction below poverty line (50% of median, OECD definition)
        frac_med = 0.0_dp
        xc=1
        do while (yvec(xc)<=0.5_dp * stat%incmed(tclb))
            frac_med = frac_med + distrvec(xc)
            xc= xc+1
        enddo
        stat%fracpov(tc) = frac_med
        print*, "frac at poverty line is ", stat%fracpov(tc),  stat%incmed(tclb)*0.5_dp
        
        !call sub_vectorize( Phi_child_loc(:,:,:,:,:,:,:,:,:,jr(1)-5:jr(1)-5),pol%grwageinc(:,:,:,:,1,:,1,:,:,:,1,:,jr(1)-5:jr(1)-5,tc),distrvec,yvec,nh,.true.)
        !distrvec = distrvec / (jr(1)-1 - js - 5 +1)
        !!! compute gini
        !!call sub_gini(distrvec,yvec,nh,stat%gini_inc(tcc) ) !,.true.)
        !
        !! determine POVERTY line: gross income 
        !if (opt_trref==0)then
        !    ! compute median
        !    frac_med = 0.0_dp
        !    xc=1
        !    do while (frac_med<0.5_dp)
        !        frac_med = frac_med + distrvec(xc)
        !        xc= xc+1
        !    enddo
        !    stat%grincmed(tc) = yvec(xc-1)
        !    print*, "gross median is ", stat%grincmed(tclb)
        !endif
        !grincmed = stat%grincmed(tclb)
        !
        !! fraction below poverty line (50% of median, OECD definition)
        !frac_med = 0.0_dp
        !xc=1
        !do while (yvec(xc)<=0.5_dp * stat%grincmed(tclb))
        !    frac_med = frac_med + distrvec(xc)
        !    xc= xc+1
        !enddo
        !!stat%fracpov(tclb) = frac_med
        !print*, "frac at gross  poverty line is ",frac_med,  stat%grincmed(tclb)*0.5_dp
        !
       ! pause
        ! ********************************
        ! gini of labor income tax contributions
     !   Phi_child_loc(:,:,:,:,:,:,:,:,:,js+1:jr(1)-5,tc) = Phi_child_loc(:,:,:,:,:,:,:,:,:,js+1:jr(1)-5,tc) * (jr(1)-5 - js)
        if (opt_suits==1)then 
            ! sort and compute nh, now for consumption
            ! sort distr by income
            call sub_vectorize2( Phi_child_loc(1,:,:,:,:,:1,1,1,1,1,:,1,:,:,:,js+5:js+5),pol%labinctaxrv(:,:,:,:,1,:,1,:,:,:,1,:,js+5:js+5,tc),pol%grwageinc(:,:,:,:,1,:,1,:,:,:,1,:,js+5:js+5,tc),distrvec1,distrvec2,yvec1,yvec2,nh,.true.)
            
            print*, "vectorized"
            
            if (tcc==1) then
                allocate(lorx(nh))
                allocate(lory(nh))
            endif
            ! compute gini
            !call sub_gini(distrvec,yvec,nh,stat%gini_labrv(tcc))
            !call sub_gini(distrvec,yvec,lorx(:),lory(:),stat%gini_labrv(tcc) ) !,.true.)
            call sub_gini2(distrvec1,distrvec2,yvec1,yvec2,lorx,lory,stat%gini_labrv(tcc))
            !stat%gini_cons(tcc) = fun_gini(distrvec,yvec)
        
            print*, "gini lab rv ", stat%gini_labrv(tcc)
            !pause
        endif
        
        
       !! ! compute variance of logs, coefficient of variation, median 
        !call sub_varcv(distrvec,log(yvec),stat%varlog_labrv(tcc),cvy0,med_y)
        
        
        !! ********************************
        !! gross wage inc gini
        !
        !! sort and compute nh, now for gross wage inc
        !call sub_vectorize( Phi_child_loc(:,:,:,:,:,:,:,:,:,jc,tc),pol%grwageinc_child(:,:,1,:,:,1,:,:,:,:,1,:,jc,tc),distrvec,yvec,nh,.true.)
        !
        !! compute gini
        !call sub_gini(distrvec,yvec,lorx(:),lory(:),stat%gini_inc(tcc) ) !,.true.)
        !!stat%gini_cons(tcc) = fun_gini(distrvec,yvec)
        !
        !print*, "gr wage inc gini child", stat%gini_inc(tcc)
        !!pause
        !
        !! correct small entries (rounding error):
        !yvec=max(epsi,yvec)
        !
        !! compute variance of logs, coefficient of variation, median 
        !call sub_varcv(distrvec,log(yvec),stat%varlog_inc(tcc),cvy0,med_y)

    
        tcc=tcc+1
    end do

    !deallocate(stat%gini_ass)
    !deallocate(stat%varlog_ass)
    !deallocate(stat%gini_cons)
    !deallocate(stat%varlog_cons)
    !deallocate(stat%gini_inc)
    !deallocate(stat%varlog_inc)
    !
    deallocate(Phi_child_loc)
   ! deallocate(distrvec_kc)

endif

end subroutine sub_sumstats
! -----------------------------------------------------------------

! -----------------------------------------------------------------
subroutine sub_vectorize(distr,y,distrvec,yvec,nh,opt_srt)

use alexutils, only: linmake
use toolbox, only: sort
USE PERMU_INT
USE UMACH_INT

implicit none

real(dp),dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:),intent(in)::distr,y
real(dp),allocatable,dimension(:),intent(out)::distrvec,yvec
integer,intent(out)::nh
logical,intent(in):: opt_srt

integer,allocatable,dimension(:)::perm
integer::hc

! build objects
nh=size(y)
allocate(distrvec(nh))
allocate(yvec(nh))
allocate(perm(nh))

! build vectors
yvec=reshape(y,(/nh/))
distrvec=reshape(distr,(/nh/))

if (opt_srt==.true.)then
print*, "here1"
    ! sort matrices according to y-dimension
    perm=linmake(nh)
    call dsvrgp(nh,yvec,yvec,perm)
    !call sort(yvec, perm)
    print*, "here2"
    !distrvec = reshape( distr, (/ nh /), order = perm)
    call permu(distrvec,perm,distrvec)

endif


end subroutine sub_vectorize
! -----------------------------------------------------------------

! -----------------------------------------------------------------
subroutine sub_vectorize2(distr,y1,y2,distrvec1,distrvec2,yvec1,yvec2,nh,opt_srt)

use alexutils, only: linmake
implicit none

! sort based on y2, use y1

real(dp),dimension(:,:,:,:,:,:,:,:,:,:),intent(in)::distr,y1,y2
real(dp),allocatable,dimension(:),intent(out)::distrvec1,yvec1,distrvec2,yvec2
integer,intent(out)::nh
logical,intent(in):: opt_srt

integer,allocatable,dimension(:)::perm1,perm2
integer::hc

! build objects
nh=size(y1)
allocate(distrvec1(nh)) 
allocate(yvec1(nh))
allocate(perm1(nh))
allocate(distrvec2(nh)) 
allocate(yvec2(nh))
allocate(perm2(nh))

! build vectors
yvec1=reshape(y1,(/nh/))
yvec2=reshape(y2,(/nh/))
distrvec1=reshape(distr,(/nh/))
distrvec2=reshape(distr,(/nh/))

! sort matrices according to y-dimension
perm1=linmake(nh)
perm2=linmake(nh)

! sort y1 itself
call dsvrgp(nh,yvec1,yvec1,perm1)

! sort y2 itself
call dsvrgp(nh,yvec2,yvec2,perm2)

! sort distr according to y2
!call dpermu(nh,yvec,perm,1,yvec)
call dpermu(nh,distrvec1,perm1,1,distrvec1)
call dpermu(nh,distrvec2,perm2,1,distrvec2)

end subroutine sub_vectorize2
! -----------------------------------------------------------------



! ---------------------------------------------------------------------
subroutine sub_gini2(distr1,distr2,y1,y2,lorx,lory,gini)

! computes the gini coefficient

implicit none

real(dp),dimension(:),intent(in)::distr1,distr2,y1,y2
real(dp),intent(out),dimension(:)::lorx,lory
real(dp),intent(out)::gini
!logical,intent(in):: opt_srt
integer:: nhz

real(dp)::mass1,mass2,toty1,toty2
integer::nh,hc,hcc

nh=size(distr1,1)

toty1=sum(y1(:)*distr1(:))
toty2=sum(y2(:)*distr2(:))

print*, "here1"

!if (opt_srt)then
!$OMP PARALLEL
!$OMP DO private(mass1,mass2)
    ! compute the lorenz curves (only for sorted data)
    do hc=1,nh
        mass1=sum(y1(1:hc)*distr1(1:hc))
        lory(hc)=mass1/toty1
        
        !lorx(hc)=sum(distr(1:hc))
        mass2=sum(y2(1:hc)*distr2(1:hc))
        lorx(hc)=mass2/toty2
    end do
!endif
!$OMP END DO
!$OMP END PARALLEL

 print*, "here2"   
    
gini=0.0_dp
!nhz = count(distr1>0.0_dp)
!$OMP PARALLEL
!$OMP DO  reduction(+:gini)
do hc=2,nh
   ! do hcc=1,nh
        gini=gini+ (lory(hc)+lory(hc-1)) * (lorx(hc)-lorx(hc-1))
   ! end do
end do
!$OMP END DO
!$OMP END PARALLEL

!print*, "here", gini, toty,nhz

!gini=gini/(2.0_dp*toty*nhz**2.0_dp)

end subroutine sub_gini2
! ---------------------------------------------------------------------


! ---------------------------------------------------------------------
subroutine sub_gini(distr,y,nh,gini)

! computes the gini coefficient

implicit none

real(dp),dimension(:),intent(in)::distr,y
integer,intent(in)::nh
real(dp),intent(out)::gini
real(dp):: toty

real(dp)::temp
integer::hc,hcc
real(dp):: start_time,stop_time 

! total y:
toty=sum(y(:)*distr(:))

!start_time= Omp_get_wtime()
gini=0.0_dp
!$OMP PARALLEL
!$OMP DO private(hc,temp), reduction(+:gini)
do hc=1,nh
    temp = sum(distr(:)*abs(y(hc)-y(:)))
    gini = gini + distr(hc) * temp
end do
!$OMP END DO
!$OMP END PARALLEL
!stop_time= Omp_get_wtime()
!if (opt_print==1) print*, 'time elapsed for Gini computation: ', stop_time-start_time

gini=gini/(2.0_dp*toty)

end subroutine sub_gini
! ---------------------------------------------------------------------


! ---------------------------------------------------------------------
subroutine sub_theil(distr,y,theil)

implicit none
real(dp),dimension(:),intent(in)::distr,y
real(dp),intent(out)::theil

real(dp)::temp,toty 
integer::nh,hc,hcc

nh=size(distr,1)

toty=sum(y(:)*distr(:))

temp = 0.0_dp
!$OMP PARALLEL
!$OMP DO private(hc), reduction(+:temp)
do hc = 1,nh
    temp = temp + y(hc)/toty * log(y(hc)/toty)
enddo
!$OMP END DO
!$OMP END PARALLEL

theil = temp/nh 

end subroutine sub_theil
! ---------------------------------------------------------------------


! ---------------------------------------------------------------------
subroutine sub_varcv(distr,y,vary,cvy,medy,pvec,qvec,pvec_frac,qvec_frac,opt_log,toty)

! computes variance, coefficient of variation and median of y

implicit none

real(dp),dimension(:),intent(in)::distr,y
real(dp),intent(out)::vary,cvy,medy,pvec(:),qvec(:),pvec_frac(:),qvec_frac(:),toty
integer,intent(in):: opt_log

real(dp)::meany,meany2,sumdistr,afac,vec_pc(9),vec_qc(4)
real(dp),allocatable::adistr(:)
integer::nh,pc
integer:: hc,pc_loc,qc_loc

do pc = 1,9
    vec_pc(pc) = 0.1_dp * pc 
enddo

do pc = 1,4
    vec_qc(pc) = 0.2_dp * pc 
enddo
    

nh=size(distr,1)
allocate(adistr(nh))

! make sure that distr sums to one:
afac=sum(distr(:))
adistr=distr/afac

meany=sum(y(:)*adistr(:))
meany2=sum(y(:)**2.0*adistr(:))

vary=meany2-meany**2
cvy=sqrt(vary)/meany

! determine median:
hc=0
sumdistr=0.0_dp
do while ( sumdistr<=0.5 )
    ! only look at positive entries in distribution:
    hc=hc+1
    sumdistr=sumdistr+adistr(hc)
end do
medy=y(hc)

! compute total
hc=0
toty=0.0_dp
do hc = 1,nh
    ! only look at positive entries in distribution:
    
    toty = toty + adistr(hc) * y(hc)
end do


!! determine p(10):
!hc=0
!sumdistr=0.0_dp
!do while ( sumdistr<=0.1 )
!    ! only look at positive entries in distribution:
!    hc=hc+1
!    sumdistr=sumdistr+adistr(hc)
!end do
!p10=y(hc)

! determine p(90):
pvec_frac = 0.0_dp
qvec_frac = 0.0_dp
do pc = 1,9
    hc=0
    sumdistr=0.0_dp
    do while ( sumdistr<=vec_pc(pc) )
        ! only look at positive entries in distribution:
        hc=hc+1
        sumdistr=sumdistr+adistr(hc)
        
    end do
    pvec(pc)=y(hc)
    
enddo




do pc = 1,4
    hc=0
    sumdistr=0.0_dp
    do while ( sumdistr<=vec_qc(pc) )
        ! only look at positive entries in distribution:
        hc=hc+1
        sumdistr=sumdistr+adistr(hc)
       
    end do
    qvec(pc)=y(hc)
    
   
enddo

do hc = 1,nh
        
    pc_loc = fun_dec(y(hc),pvec)
    qc_loc = fun_dec(y(hc),qvec)
        
    ! only look at positive entries in distribution:
        
    pvec_frac(pc_loc)=pvec_frac(pc_loc) + adistr(hc) * y(hc)
    qvec_frac(qc_loc)=qvec_frac(qc_loc) + adistr(hc) * y(hc)
    !toty = toty + adistr(hc) * y(hc)
        
enddo

qvec_frac = qvec_frac / toty
pvec_frac = pvec_frac / toty

if (opt_log==1)then
    pvec = exp(pvec)
    qvec = exp(qvec)

    !p10 = exp(p10)
    !p90 = exp(p90)
    medy = exp(medy)
endif
!print*, "here", p10,p90,hc,y(hc-5:hc+2)
!pause

end subroutine sub_varcv
! ---------------------------------------------------------------------


! ------------------------------------------------------------------- 
subroutine sub_saving(agg,demo,grid,pol,lc,stat)  
! saving of output
use esplot
implicit none
type(t_agg),intent(inout)::agg 
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
type(t_lc),intent(inout)::lc
type(t_stat),intent(inout)::stat
real(dp)::gr_rho_p(nt),cev_loc,afcev,v1_int,welf_aggdist_phi,coh,prodret,ass,v2_int
real(dp):: coh_glb_min,coh_glb_max,coh_grid(nx),v1_int_ds(nd,ns),v2_int_ds(nd,ns),logsum,logsum_base,prob_kid(ns),prob_kid_base(ns),welf_agg,ass_glb_max,ass_glb_min,ass_grid(nx)
integer::inds(2)
real(dp)::vals(2)
integer::xc,pc,yc,ec,ic,jc,kc,sc,tc,tcc,tccc,dc,gc,scc,icc,jcc,jc_temp,tc_temp,sc1,sc2,yc2,kc1,kc2,zc,gckid,ickid,hc,jc_kid
character(1)::i_char
real(dp),parameter::epsi=1.0e-06


!print*, ' '
!print*, '----------------------------------------'
!print*, 'Saving output'
!print*, '----------------------------------------'
!print*, ' ' 

tc = 1

if (opt_trref==0 .and. tc==1)then
    write(i_char , '(i1)' )  0
else
    write(i_char , '(i1)' )  1
endif
        

! save average ivt transfer and rho
if (opt_trref==0 .and. tc==1)then
    open(unit=33,file='output/intergT.txt')
else    
    open(unit=33,file='output/intergT.txt',action='write',position='append')
endif
open(unit=34,file='output/interg'//i_char//'.txt')

write(33,'(7f50.16)') agg%tinv(tc), agg%moninv(tc), agg%ivt_pkid(tc),agg%h_j(jt,tc), agg%h_j(jt+1,tc), agg%rhoedu0(tc), agg%rhoearn0(tc)
write(34,'(7f50.16)') agg%tinv(tc), agg%moninv(tc), agg%ivt_pkid(tc),agg%h_j(jt,tc), agg%h_j(jt+1,tc), agg%rhoedu0(tc), agg%rhoearn0(tc)
        
close (33)
close (34)


! save LC profiles investments
if (opt_trref==0 .and. tc==1)then
    open(unit=83,file='output/sharesT.txt')
else    
    open(unit=83,file='output/sharesT.txt',action='write',position='append')
endif
open(unit=84,file='output/inv_lc'//i_char//'.txt')

write(83,'(3f50.16)') agg%lhsfrac(tc),  1.0_dp - agg%clfrac(tc) - agg%lhsfrac(tc), agg%clfrac(tc)
write(84,'(3f50.16)') agg%lhsfrac(tc),  1.0_dp - agg%clfrac(tc) - agg%lhsfrac(tc), agg%clfrac(tc)
        
close (83)
close (84)
        
! save LC profiles investments
if (opt_trref==0 .and. tc==1 )then
    open(unit=23,file='output/inv_lcT.txt')
else
    open(unit=23,file='output/inv_lcT.txt',action='write',position='append')
endif
open(unit=24,file='output/inv_lc'//i_char//'.txt')
       
do jc_kid = jf,jt
    write(23,'(3f50.16)') agg%m_j(jc_kid,tc) , agg%t_j(jc_kid,tc), agg%h_j(jc_kid,tc) 
    write(24,'(3f50.16)') agg%m_j(jc_kid,tc) , agg%t_j(jc_kid,tc), agg%h_j(jc_kid,tc)    
enddo
        
close (23)
close (24)
        
! save human capital profiles, by skills and marital status
if (opt_trref==0 .and. tc==1)then
    open(unit=13,file='output/hc_jT.txt')
else
    open(unit=13,file='output/hc_jT.txt',action='write',position='append')
endif
open(unit=14,file='output/hc_j'//i_char//'.txt') 
       
do jc_kid = jf,jt+1
    write(13,'(6f50.16)') agg%h_sgj(1,1,jc_kid,tc) ,agg%h_sgj(2,1,jc_kid,tc) ,agg%h_sgj(3,1,jc_kid,tc) ,agg%h_sgj(1,2,jc_kid,tc) ,agg%h_sgj(2,2,jc_kid,tc) ,agg%h_sgj(3,2,jc_kid,tc) 
    write(14,'(6f50.16)') agg%h_sgj(1,1,jc_kid,tc) ,agg%h_sgj(2,1,jc_kid,tc) ,agg%h_sgj(3,1,jc_kid,tc) ,agg%h_sgj(1,2,jc_kid,tc) ,agg%h_sgj(2,2,jc_kid,tc) ,agg%h_sgj(3,2,jc_kid,tc)     
enddo
        
close (13) 
close (14) 
        
! save money profiles, by skills and marital status
if (opt_trref==0 .and. tc==1)then
    open(unit=11,file='output/moninv_sgjT.txt')
else
    open(unit=11,file='output/moninv_sgjT.txt',action='write',position='append')
endif
open(unit=12,file='output/moninv_sgj'//i_char//'.txt')
       
do jc_kid = jf,jt
    write(11,'(6f50.16)') agg%m_sgj(1,1,jc_kid,tc) ,agg%m_sgj(2,1,jc_kid,tc) ,agg%m_sgj(3,1,jc_kid,tc) ,agg%m_sgj(1,2,jc_kid,tc) ,agg%m_sgj(2,2,jc_kid,tc) ,agg%m_sgj(3,2,jc_kid,tc) 
    write(12,'(6f50.16)') agg%m_sgj(1,1,jc_kid,tc) ,agg%m_sgj(2,1,jc_kid,tc) ,agg%m_sgj(3,1,jc_kid,tc) ,agg%m_sgj(1,2,jc_kid,tc) ,agg%m_sgj(2,2,jc_kid,tc) ,agg%m_sgj(3,2,jc_kid,tc)     
enddo
        
close (11) 
close (12) 
        
! save time profiles, by skills and marital status
if (opt_trref==0 .and. tc==1)then
    open(unit=9,file='output/tinv_sgjT.txt')
else
    open(unit=9,file='output/tinv_sgjT.txt',action='write',position='append')
endif
open(unit=10,file='output/tinv_sgj'//i_char//'.txt')
       
do jc_kid = jf,jt
    write(9,'(6f50.16)') agg%t_sgj(1,1,jc_kid,tc) ,agg%t_sgj(2,1,jc_kid,tc) ,agg%t_sgj(3,1,jc_kid,tc) ,agg%t_sgj(1,2,jc_kid,tc) ,agg%t_sgj(2,2,jc_kid,tc) ,agg%t_sgj(3,2,jc_kid,tc) 
    write(10,'(6f50.16)') agg%t_sgj(1,1,jc_kid,tc) ,agg%t_sgj(2,1,jc_kid,tc) ,agg%t_sgj(3,1,jc_kid,tc) ,agg%t_sgj(1,2,jc_kid,tc) ,agg%t_sgj(2,2,jc_kid,tc) ,agg%t_sgj(3,2,jc_kid,tc)     
enddo
        
close (9) 
close (10)

! save time profiles, by prod stat
if (opt_trref==0 .and. tc==1)then
    open(unit=559,file='output/tinv_yT.txt')
else
    open(unit=559,file='output/tinv_yT.txt',action='write',position='append')
endif
open(unit=510,file='output/tinv_y'//i_char//'.txt')
       
do jc_kid = jf,jt
    write(559,'(2f50.16)') agg%t_yj(1,jc_kid,tc) , agg%t_yj(2,jc_kid,tc) 
    write(510,'(2f50.16)') agg%t_yj(1,jc_kid,tc) , agg%t_yj(2,jc_kid,tc)
enddo
close (559) 
close (510)

! save time profiles, by prod stat
if (opt_trref==0 .and. tc==1)then
    open(unit=569,file='output/moninv_yT.txt')
else
    open(unit=569,file='output/moninv_yT.txt',action='write',position='append')
endif
open(unit=560,file='output/moninv_y'//i_char//'.txt')
       
do jc_kid = jf,jt
    write(569,'(2f50.16)') agg%m_yj(1,jc_kid,tc) , agg%m_yj(2,jc_kid,tc) 
    write(560,'(2f50.16)') agg%m_yj(1,jc_kid,tc) , agg%m_yj(2,jc_kid,tc)
enddo
close (569) 
close (560)

! save ivt, by skills and marital status
if (opt_trref==0 .and. tc==1)then
    open(unit=39,file='output/ivt_sgjT.txt')
else
    open(unit=39,file='output/ivt_sgjT.txt',action='write',position='append')
endif
open(unit=109,file='output/ivt_sgj'//i_char//'.txt')
       
write(39,'(6f50.16)') agg%ivt_sg(1,1,tc) ,agg%ivt_sg(2,1,tc) ,agg%ivt_sg(3,1,tc) ,agg%ivt_sg(1,2,tc) ,agg%ivt_sg(2,2,tc) ,agg%ivt_sg(3,2,tc) 
write(109,'(6f50.16)') agg%ivt_sg(1,1,tc) ,agg%ivt_sg(2,1,tc) ,agg%ivt_sg(3,1,tc) ,agg%ivt_sg(1,2,tc) ,agg%ivt_sg(2,2,tc) ,agg%ivt_sg(3,2,tc)     
        
close (39) 
close (109)
        
! save money inv, by skills
if (opt_trref==0 .and. tc==1)then
    open(unit=61,file='output/moninv_sT.txt')
else
    open(unit=61,file='output/moninv_sT.txt',action='write',position='append')
endif
open(unit=62,file='output/moninv_s'//i_char//'.txt') 
       
write(61,'(3f50.16)') agg%m_s(1,tc) , agg%m_s(2,tc), agg%m_s(3,tc)
write(62,'(3f50.16)') agg%m_s(1,tc) , agg%m_s(2,tc), agg%m_s(3,tc)
        
close (61) 
close (62) 
        
! save time inv, by skills
if (opt_trref==0 .and. tc==1)then
    open(unit=66,file='output/tinv_sT.txt')
else
    open(unit=66,file='output/tinv_sT.txt',action='write',position='append')
endif
open(unit=67,file='output/tinv_s'//i_char//'.txt')
       
write(66,'(3f50.16)') agg%t_s(1,tc) , agg%t_s(2,tc), agg%t_s(3,tc)
write(67,'(3f50.16)') agg%t_s(1,tc) , agg%t_s(2,tc), agg%t_s(3,tc)
        
close (66) 
close (67)
        
! save transfers, by skills
if (opt_trref==0 .and. tc==1)then
    open(unit=46,file='output/ivt_sT.txt')
else
    open(unit=46,file='output/ivt_sT.txt',action='write',position='append')
endif
open(unit=47,file='output/ivt_s'//i_char//'.txt')
       
write(46,'(3f50.16)') agg%ivt_s(1,tc) , agg%ivt_s(2,tc), agg%ivt_s(3,tc)
write(47,'(3f50.16)') agg%ivt_s(1,tc) , agg%ivt_s(2,tc), agg%ivt_s(3,tc)
        
close (46) 
close (47) 
    

! save HK age 18, by skills
if (opt_trref==0 .and. tc==1)then
    open(unit=116,file='output/h16_sT.txt')
else
    open(unit=116,file='output/h16_sT.txt',action='write',position='append')
endif
open(unit=117,file='output/h16_s'//i_char//'.txt')
       
write(116,'(3f50.16)') agg%h_sj(1,jt,tc) ,agg%h_sj(2,jt,tc), agg%h_sj(3,jt,tc)
write(117,'(3f50.16)') agg%h_sj(1,jt,tc) ,agg%h_sj(2,jt,tc), agg%h_sj(3,jt,tc)
        
close (116) 
close (117) 

! save educ matrices
if (opt_trref==0 .and. tc==1)then
    open(unit=24,file='output/educmT.txt')
else
    open(unit=24,file='output/educmT.txt',action='write',position='append')
endif
open(unit=25,file='output/educm'//i_char//'.txt') 
       
do sc= 1,ns
    write(24,'(3f50.16)') sum(grid%Phi_guess(:,:,:,:,:,:,:,1,:,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,sc,:,:,js,tc) ) , sum(grid%Phi_guess(:,:,:,:,:,:,:,2,:,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,sc,:,:,js,tc) )  , sum(grid%Phi_guess(:,:,:,:,:,:,:,3,:,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,sc,:,:,js,tc) )    
    write(25,'(3f50.16)') sum(grid%Phi_guess(:,:,:,:,:,:,:,1,:,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,sc,:,:,js,tc) ) , sum(grid%Phi_guess(:,:,:,:,:,:,:,2,:,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,sc,:,:,js,tc) )  , sum(grid%Phi_guess(:,:,:,:,:,:,:,3,:,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,sc,:,:,js,tc) )    
enddo
        
close (24)
close (25)
        
! save educ matrices by marital status 
if (opt_trref==0 .and. tc==1)then
    open(unit=15,file='output/educ_gT.txt')
else
    open(unit=15,file='output/educ_gT.txt',action='write',position='append')
endif
open(unit=16,file='output/educ_g'//i_char//'.txt')
       
do sc= 1,ns
    write(15,'(6f50.16)') sum(grid%Phi_guess(:,:,:,:,:,:,:,1,1,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,1,sc,:,:,js,tc) ) , sum(grid%Phi_guess(:,:,:,:,:,:,:,2,1,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,1,sc,:,:,js,tc) )  , sum(grid%Phi_guess(:,:,:,:,:,:,:,3,1,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,1,sc,:,:,js,tc) )    , &
        sum(grid%Phi_guess(:,:,:,:,:,:,:,1,2,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,2,sc,:,:,js,tc) ) , sum(grid%Phi_guess(:,:,:,:,:,:,:,2,2,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,2,sc,:,:,js,tc) )  , sum(grid%Phi_guess(:,:,:,:,:,:,:,3,2,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,2,sc,:,:,js,tc) )    
    write(16,'(6f50.16)') sum(grid%Phi_guess(:,:,:,:,:,:,:,1,1,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,1,sc,:,:,js,tc) ) , sum(grid%Phi_guess(:,:,:,:,:,:,:,2,1,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,1,sc,:,:,js,tc) )  , sum(grid%Phi_guess(:,:,:,:,:,:,:,3,1,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,1,sc,:,:,js,tc) )    , &
        sum(grid%Phi_guess(:,:,:,:,:,:,:,1,2,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,2,sc,:,:,js,tc) ) , sum(grid%Phi_guess(:,:,:,:,:,:,:,2,2,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,2,sc,:,:,js,tc) )  , sum(grid%Phi_guess(:,:,:,:,:,:,:,3,2,sc,:,:,js,tc) ) /sum(grid%Phi_guess(:,:,:,:,:,:,:,:,2,sc,:,:,js,tc) )    
   
enddo

        
close (15)
close (16)


! fraction of "horse riders"
if (opt_trref==0 .and. tc==1)then
    open(unit=76,file='output/hr_fracT.txt')
    open(unit=79,file='output/hr_frac_sgT.txt')
else
    open(unit=76,file='output/hr_fracT.txt',action='write',position='append')
    open(unit=79,file='output/hr_frac_sgT.txt',action='write',position='append')
endif
open(unit=77,file='output/hr_frac'//i_char//'.txt')
       
write(76,'(4f50.16)') agg%hr_sh(tc), agg%hr_sh_sp(1,tc), agg%hr_sh_sp(2,tc), agg%hr_sh_sp(3,tc)
write(77,'(4f50.16)') agg%hr_sh(tc), agg%hr_sh_sp(1,tc), agg%hr_sh_sp(2,tc), agg%hr_sh_sp(3,tc)
write(79,'(6f50.16)') agg%hr_sh_gp_sp(1,1,tc), agg%hr_sh_gp_sp(1,2,tc), agg%hr_sh_gp_sp(1,3,tc) ,agg%hr_sh_gp_sp(2,1,tc), agg%hr_sh_gp_sp(2,2,tc), agg%hr_sh_gp_sp(2,3,tc)
        
close (76) 
close (77) 
close (79) 


if (opt_trref==0 .and. tc==1)then
    open(unit=918,file='output/gini.txt')
else
    open(unit=918,file='output/gini.txt',action='write',position='append')
endif
write(918,'(2f50.16)') stat%gini_inc(tc), stat%varlog_inc(tc)
close(918)

end subroutine sub_saving
! ------------------------------------------------------------------- 




! -------------------------------------------------------------------------------
subroutine sub_param_jincr_logist(alpha0,alpha1,param_j)

implicit none

real(dp),intent(in):: alpha0,alpha1
real(dp),intent(out):: param_j(:)
integer:: jc,jmax,jc_temp
real(dp):: temp

jmax = size(param_j,1)

do jc=1,jmax
     
    if (jc==1)then
        jc_temp = 4
    else
        jc_temp = (jc+1) * 2
    endif
    
    temp = 1.0_dp + exp(-alpha0 *(jc_temp + alpha1)) 
    param_j(jc) = 1.0_dp / temp
   
enddo
    
end subroutine sub_param_jincr_logist
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_param_jincr_concave(alpha0,alpha1,param_j)
! function returning age dependent parameter vector param_j, based on exponnential function
implicit none

real(dp),intent(in):: alpha0,alpha1
real(dp),intent(out):: param_j(:)
integer:: jc,jmax,jc_temp
real(dp):: temp
real(dp),parameter::epsi=1.0e-03_dp

jmax = size(param_j,1)

do jc=1,jmax
    
    if (jc==1)then
        jc_temp = 4
    else
        jc_temp = (jc+1) * 2
    endif
    
    temp =alpha0*  exp( - alpha1 * jc_temp)
    param_j(jc) =max(epsi, 1.0_dp - temp ) 
   
enddo
    
end subroutine sub_param_jincr_concave
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_param_jincr_convex(alpha0,alpha1,param_j)
! function returning age dependent parameter vector param_j, based on exponnential function
implicit none

real(dp),intent(in):: alpha0,alpha1
real(dp),intent(out):: param_j(:)
integer:: jc,jmax,jc_temp
real(dp):: temp

jmax = size(param_j,1)

do jc=1,jmax
    
    if (jc==1)then
        jc_temp = 4
    else
        jc_temp = (jc+1) * 2
    endif
        
    temp =alpha0*  exp(  alpha1 * jc_temp)
    param_j(jc) =  temp
   
enddo
    
end subroutine sub_param_jincr_convex
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_getalpha_sigma(sigma1,sigma2,alpha0,alpha1)
! function returning age dependent parameter vector param_j, based on exponnential function
implicit none

real(dp),intent(in):: sigma1,sigma2
real(dp),intent(out):: alpha0,alpha1
integer:: jc,jmax
real(dp):: temp,temp1,temp2

temp1 = log(sigma1/sigma2)
temp2 = 10.5_dp - 2.5_dp
temp2 = 1.0_dp / temp2
alpha1 = - temp2 * temp1

temp = exp(2.5_dp * alpha1)
alpha0 = sigma1 / temp
    
end subroutine sub_getalpha_sigma
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_getalpha_kappa(kappa1,kappa2,alpha0,alpha1)
! function returning age dependent parameter vector param_j, based on exponnential function
implicit none

real(dp),intent(in):: kappa1,kappa2
real(dp),intent(out):: alpha0,alpha1
integer:: jc,jmax
real(dp):: temp,temp1,temp2

temp1 = (1.0_dp - kappa1) / (1.0_dp - kappa2)
temp1 = log(temp1)
temp2 =-1.0_dp / ( 2.5_dp - 10.5_dp  )
alpha1 = temp1 * temp2

temp = exp( - 2.5_dp * alpha1)
alpha0 = (1.0_dp - kappa1) / temp
    
end subroutine sub_getalpha_kappa
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_param_linlog(alpha0,alpha1,param_j,jckid)
! function returning age dependent parameter vector param_j, based on specification linear in logs
implicit none

real(dp),intent(in):: alpha0,alpha1
real(dp),intent(out):: param_j
integer,intent(in):: jckid
integer:: jc,jmax,jc_temp
real(dp):: temp

    
temp =alpha0 +   alpha1 * (jckid - 1)
    
param_j =  exp(temp)
   
    
end subroutine sub_param_linlog
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
subroutine sub_getinterc_linlog(midval,jmid,alpha0,alpha1)
! function returning age dependent parameter vector param_j, based on specification linear in logs
implicit none

real(dp),intent(in):: alpha1,midval
integer,intent(in):: jmid
real(dp),intent(out):: alpha0
integer:: jc,jmax,jc_temp
real(dp):: temp

alpha0 = log(midval) - alpha1 * (jmid - 1)
    
end subroutine sub_getinterc_linlog
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
function fun_sw_point(val)

implicit none
integer:: fun_sw_point
real(dp), intent(in):: val(:,:) ! 1dim is grid, 2dim is decision
integer:: nnx,nns,xc
real(dp):: high_sh(nx),logsum,prob_s(size(val,2))

nnx = size(val,1)
nns = size(val,2)

do xc=nx-1,1,-1
    
    call sub_logsumprob(val(xc,:),logsum,prob_s,ns,sigma_emp)
    high_sh(xc) = prob_s(nns)
    
    if (high_sh(xc) > high_sh(xc+1))then
        fun_sw_point = xc
    endif
        
    
enddo

end function fun_sw_point
! -------------------------------------------------------------------------------


! ---------------------------------------------------------------------------------------------
function f_probfe(h,sc,gammah)
! computes update of consumption tax rate
implicit none

real(dp)::f_probfe
real(dp),intent(in)::h,gammah! agg consumption, G (exo),E,labor,pension, capital inc tax revenues
integer,intent(in):: sc
real(dp):: allrv,netexp,gammahloc,help
gammahloc=1.5_dp

if (sc==1)then
    f_probfe =max(0.0_dp,min(1.0_dp, h/gammahloc )) !min(1.0_dp, exp(h)/5.0_dp ) !1.0_dp - 0.5_dp * exp(-h)
elseif (sc==2)then
    f_probfe =max(0.0_dp,min(1.0_dp, h/gammahloc )) !min(1.0_dp, exp(h)/5.0_dp ) !1.0_dp - 0.5_dp * exp(-h)
else
    f_probfe =max(0.0_dp,min(1.0_dp, h/gammahloc )) !min(1.0_dp, exp(h)/5.0_dp ) !1.0_dp - 0.5_dp * exp(-h)
endif

if (sc<ns)then

   f_probfe = 1.0_dp - exp(-h )
else
   f_probfe = 1.0_dp - exp(-h) 
endif

!help = exp(4.9_dp * (h - 1.0_dp )  )

!f_probfe = help/(1.0_dp + help) 
 
end function f_probfe
! ---------------------------------------------------------------------------------------------
 
! ---------------------------------------------------------------------------------------------
function f_dprobfe(h,sc,gammah)
! computes update of consumption tax rate
implicit none

real(dp)::f_dprobfe
real(dp),intent(in)::h,gammah! agg consumption, G (exo),E,labor,pension, capital inc tax revenues
integer,intent(in):: sc
real(dp):: allrv,netexp,gammahloc
gammahloc=1.38_dp 
f_dprobfe =( 1/gammahloc )**(1.0_dp) * 1.0_dp * h**(1.0_dp-1.0_dp) !min(1.0_dp, exp(h)/5.0_dp ) !1.0_dp - 0.5_dp * exp(-h)

end function f_dprobfe
! ---------------------------------------------------------------------------------------------
 
! ---------------------------------------------------------------------------------------------
function f_probfin(h)
! computes update of consumption tax rate
implicit none

real(dp)::f_probfin
real(dp),intent(in)::h! agg consumption, G (exo),E,labor,pension, capital inc tax revenues
real(dp):: allrv,netexp

f_probfin =1.0_dp -  exp(-h * omega_fin)

!f_probfin = 0.5_dp

end function f_probfin
! ---------------------------------------------------------------------------------------------

! ---------------------------------------------------------------------------------------------
function f_probfin_hs(h)
! computes update of consumption tax rate
implicit none

real(dp)::f_probfin_hs
real(dp),intent(in)::h! agg consumption, G (exo),E,labor,pension, capital inc tax revenues
real(dp):: allrv,netexp

f_probfin_hs =1.0_dp  -  exp(-h * omega_fin_hs)

!f_probfin = 0.5_dp

end function f_probfin_hs
! ---------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------
function scpmax(jc,sc)

implicit none
integer:: scpmax
integer,intent(in):: jc,sc

if (jc==js )then
    
    scpmax = ns
     
else
    
    scpmax = 1
endif

end function scpmax
!----------------------------------------------------------------------------


end module inst_mod
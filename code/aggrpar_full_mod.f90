module aggrpar_full_mod
    
 ! this module organizes aggregation
    use nrtype
    use params_mod
    use types_mod
    use solvehhdlabpar_mod,only:ec1,f_coh,f_coh_cpl,f_grid_ass,f_coh_tp1
    use inst_mod
    use dc_egm_mod
    use ESPlot 
   
	implicit none
    
    contains 
   ! (igov_ref_upd,aggpdvcol,aggpdvig,agg,demo,grid,pol,stat,lc,t0,t1)
    ! ---------------------------------------------------------------------------    
    subroutine sub_eval_ig(igov_str,pdvcol,pdvig,pdv_fc,agg,demo,grid,pol,stat,lc,t0,t1)
    ! function called from solver for solution of olg model
    use toolbox,only: fzero
    implicit none    
    type(t_agg),intent(inout)::agg
    type(t_demo),intent(inout)::demo
    type(t_grid),intent(inout)::grid
    type(t_pol),intent(inout)::pol
    type(t_stat),intent(inout)::stat
    type(t_lc),intent(inout)::lc
    integer,intent(in):: t0,t1
    real(dp),intent(inout):: igov_str,pdvcol,pdvig,pdv_fc
    real(dp),parameter::tolf=1.0e-09_dp
    logical:: check_return
    real(dp):: totpdv,dist
    
    if (col_subs_param==1.0_dp)then
        igov_str = igov_ref
    else
        
        call fzero(igov_str,func_ig_equil,check_return)
    endif
    
    dist = func_ig_equil(igov_str)
    
    print*, "igov_str is", igov_str,pdv_fc,totpdv,pdvcol,pdvig,dist
    print*, "omega is", pdvig / totpdv
     
    
    contains
    
    ! ---------------------------------------------------------------------------    
    function func_ig_equil(igov_str)

    implicit none
    real(dp):: func_ig_equil
    real(dp),intent(in):: igov_str
    real(dp):: igov_temp, col_temp, df
    integer:: tc
    
    ! compute igov pdv
    pdvig = 0.0_dp
    pdvcol = 0.0_dp
    do tc = 2,nt
        
        df = 1.0_dp / (1.0_dp + agg%ret(1))**(tc - 1.0_dp)
        
        igov_temp = agg%igov_kg(tc) + agg%igov_hs_base(tc) * igov_str
        igov_temp = igov_temp - agg%igov(1)
        pdvig = pdvig + df * igov_temp
    ! igov_kg(tc) + igov_hs_base(tc) * igov_str
        col_temp = agg%clsubs(tc)
        col_temp = col_temp - agg%clsubs(1)
        pdvcol = pdvcol + df * col_temp
    enddo
    
    totpdv = pdvig + pdvcol
    
    if (col_subs_param==1.0_dp)then
        pdv_fc = totpdv
    endif
    
    
    func_ig_equil = pdv_fc - totpdv
      
    
    end function
    ! --------------------------------------------------------------------------- 
    
    ! --------------------------------------------------------------------------- 
    function f_debt(psilambda)
    real(dp):: f_debt
    real(dp),intent(in):: psilambda
    integer:: tc
    real(dp):: labinctaxrv_new,def_new,dist_inc
    ! permanent tax rate change
    agg%psi_lambda(2:nt) = psilambda

    ! using gvt intertemporal budget constraint, iterate backward
    do tc = nt-1,1,-1
        if (tc>1)then
            ! from labinctaxrvyr recover labinctaxrv
            if (tc==nt-1) agg%debt(nt) = agg%debtss(nt)
            agg%debt(tc) = (agg%debt(tc+1)*(1.0_dp+demo%lamt(tc)) *(1.0_dp + demo%popgrt(tc)) + agg%gcons(tc)) / (1.0_dp + agg%ret(tc) )
         
        else
            f_debt = (agg%debt(tc+1)*(1.0_dp+demo%lamt(tc)) *(1.0_dp + demo%popgrt(tc)) + agg%gcons(tc)) / (1.0_dp + agg%ret(tc) ) !(agg%debt(tc+1) + agg%gcons(tc)) / (1.0_dp + agg%ret(tc) )
            
            !agg%gcons(tc) / (agg%ret(tc) - popgr - demo%lamt(tc) - popgr * demo%lamt(tc))
        endif
        
    enddo
     
    agg%debtss(1) = f_debt
    
    print*, "debt time path", agg%debt(:)
    !print*, "dist in f_byr", f_debt, agg%debt(2), agg%debt(1) 
    
    end function f_debt
! --------------------------------------------------------------------------- 
 


        
    end subroutine sub_eval_ig
    ! ---------------------------------------------------------------------------    
        
    
    
    ! ---------------------------------------------------------------------------    
    subroutine sub_eval_debt(psistar,debt,agg,demo,grid,pol,stat,lc,t0,t1)
    ! function called from solver for solution of olg model
    use toolbox,only: fzero
    implicit none    
    type(t_agg),intent(inout)::agg
    type(t_demo),intent(inout)::demo
    type(t_grid),intent(inout)::grid
    type(t_pol),intent(inout)::pol
    type(t_stat),intent(inout)::stat
    type(t_lc),intent(inout)::lc
    integer,intent(in):: t0,t1
    real(dp),intent(inout):: psistar
    real(dp),intent(out):: debt
    real(dp),parameter::tolf=1.0e-09_dp
    logical:: check_return
    real(dp):: byr0
    
    
    psistar = agg%psi_lambda(2) !zbrent(func_debt_equil,0.1_dp,2.0_dp,tolf)
    call fzero(psistar,func_debt_equil,check_return)
    print*, "psistar is", psistar
     
    byr0 = f_debt(psistar)
    
    debt = byr0 
    
    agg%psi_lambda(2:nt) = psistar
    
    contains
    
    ! ---------------------------------------------------------------------------    
    function func_debt_equil(lambda_str)

    implicit none
    real(dp):: func_debt_equil
    real(dp),intent(in):: lambda_str
    real(dp):: labinctaxrv
    integer:: tc
    
    do tc=2,nt
        labinctaxrv = agg%hsvtmp1(tc) - lambda_str *agg%hsvtmp2(tc)
        agg%gcons(tc) =fun_gcons(labinctaxrv,agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc)+agg%clsubs(tc) + agg%gdp(1) * gyr_str) 
        if (tc==nt) agg%debtss(tc) = agg%gcons(tc) / (agg%ret(tc) - demo%popgrt(tc) - demo%lamt(tc) - demo%popgrt(tc) * demo%lamt(tc))
    enddo
     
    byr0 = f_debt(psistar)
    
    debt = byr0 
    
    func_debt_equil = agg%debt(1) - agg%debtss(1)

    end function
    ! --------------------------------------------------------------------------- 
    
    ! --------------------------------------------------------------------------- 
    function f_debt(psilambda)
    real(dp):: f_debt
    real(dp),intent(in):: psilambda
    integer:: tc
    real(dp):: labinctaxrv_new,def_new,dist_inc
    ! permanent tax rate change
    agg%psi_lambda(2:nt) = psilambda

    ! using gvt intertemporal budget constraint, iterate backward
    do tc = nt-1,1,-1
        if (tc>1)then
            ! from labinctaxrvyr recover labinctaxrv
            if (tc==nt-1) agg%debt(nt) = agg%debtss(nt)
            agg%debt(tc) = (agg%debt(tc+1)*(1.0_dp+demo%lamt(tc)) *(1.0_dp + demo%popgrt(tc)) + agg%gcons(tc)) / (1.0_dp + agg%ret(tc) )
         
        else
            f_debt = (agg%debt(tc+1)*(1.0_dp+demo%lamt(tc)) *(1.0_dp + demo%popgrt(tc)) + agg%gcons(tc)) / (1.0_dp + agg%ret(tc) ) !(agg%debt(tc+1) + agg%gcons(tc)) / (1.0_dp + agg%ret(tc) )
            
            !agg%gcons(tc) / (agg%ret(tc) - popgr - demo%lamt(tc) - popgr * demo%lamt(tc))
        endif
        
    enddo
     
    agg%debtss(1) = f_debt
    
    print*, "debt time path", agg%debt(:)
    !print*, "dist in f_byr", f_debt, agg%debt(2), agg%debt(1) 
    
    end function f_debt
! --------------------------------------------------------------------------- 
 
!! ---------------------------------------------------------------------------    
!    subroutine sub_eval_debt(psistar,debt,agg,demo,grid,pol,stat,lc,t0,t1)
!    ! function called from solver for solution of olg model
!    use toolbox,only: fzero
!    implicit none  
!    type(t_agg),intent(inout)::agg
!    type(t_demo),intent(inout)::demo
!    type(t_grid),intent(inout)::grid
!    type(t_pol),intent(inout)::pol
!    type(t_stat),intent(inout)::stat
!    type(t_lc),intent(inout)::lc
!    integer,intent(in):: t0,t1
!    real(dp),intent(inout):: psistar
!    real(dp),intent(out):: debt
!    real(dp),parameter::tolf=1.0e-09_dp
!    logical:: check_return
!    real(dp):: byr0
!    
!    byr0 = f_byr(psistar)
!    
!    debt = byr0 * agg%gdp(2)
!    !psistar = zbrent(f_byr,0.1_dp,2.0_dp,tolf)
!    !call fzero(psistar,f_byr,check_return)
!    
!    contains
!    
!    ! --------------------------------------------------------------------------- 
!    function f_byr(psilambda)
!    real(dp):: f_byr
!    real(dp),intent(in):: psilambda
!    integer:: tc
!    real(dp):: labinctaxrv_new,def_new,dist_inc
!    ! permanent tax rate change
!    agg%psi_lambda(2:nt) = psilambda
!
!    ! using gvt intertemporal budget constraint, iterate backward
!    do tc = nt-1,2,-1
!        ! from labinctaxrvyr recover labinctaxrv
!        labinctaxrv_new = agg%labinctaxrvyr(tc) * psilambda
!        dist_inc =labinctaxrv_new - agg%labinctaxrv(tc)
!
!        ! dist_b = dist_inc, therefore recover b
!        def_new = agg%def(tc) -dist_inc
!        agg%dyr(tc) = def_new / agg%gdp(tc)
!        ! from deficit to debt
!        agg%byr(tc) = 1.0_dp/(1.0_dp + f_aftertaxRnet(1.0_dp+agg%ret(tc-1),tau_k)) * (  agg%gdp(tc+1)/agg%gdp(tc) * agg%byr(tc+1) - agg%dyr(tc) )
!        
!    enddo
!    
!    f_byr = abs(agg%byr(2) - agg%byr(1) )
!    print*, "dist in f_byr", f_byr, agg%byr(2), agg%byr(1)
!    
!    end function f_byr
!! --------------------------------------------------------------------------- 

        
    end subroutine sub_eval_debt
    ! ---------------------------------------------------------------------------    
        

! ---------------------------------------------------------------------------     
subroutine sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,opt_intp)
! computation of cross-sectional distributions 
! calls embedded subroutine for aggregation
use funcs_mod,only:func_intp
implicit none

type(t_stat),intent(inout)::stat
type(t_agg),intent(inout)::agg 
type(t_demo)::demo
type(t_grid),intent(inout)::grid
type(t_lc),intent(inout)::lc 
type(t_pol),intent(inout)::pol  
integer,intent(in)::t0,t1,opt_intp !  jmax is max age in forward aggregation; opt_upd=1: update outer loop variables

integer::tc,sc,kc,jc,ic,ec,yc,pc,xc,dc,vc,vcm1
integer:: sc1,sc2,kc1,kc2,yc1,yc2,yc1m1,yc2m1,kc_mig
integer::tcm1,tcp1,wc,jcm1,dcm1
integer::icm1,ecm1,ycm1
real(dp)::coh,gr_wage,net_wage,ben_p,gr_ben_p,leis,lab,min_pens,R,dist_ret,frac_emigr,dist
real(dp):: gr_wage1,gr_wage2,net_wage1,net_wage2,ben_p1,ben_p2,gr_ben_p1,gr_ben_p2,leis1,leis2,lab1,lab2
real(dp)::vals(2)
integer::inds(2)
real(dp),allocatable::prob(:),v_temp(:),ic_vec(:)
integer:: indsd(nd,2)
real(dp):: valsd(nd,2), prob_dc(nd),v_dc(nd), leis_dc(nd), lab_dc(nd)
real(dp):: leis1_dc(nd),leis2_dc(nd),lab1_dc(nd),lab2_dc(nd)
real(dp):: frac_d
real(dp),parameter::epsi=1.0e-04
real(dp)::sum_frac,dist_frac,migr_frac,frac
real(dp)::frac_mig,sh_mig
logical,parameter::opt_check=.false.
integer,allocatable,dimension(:) :: dc_vec
integer:: dc_temp, yc_max, dcl, nd_glob
real(dp):: v
real(dp):: pini(ny),pini_ret(ny),prob_tr_ret(ny,ny) !,prob_tr_mig(ny,ny),pini_mig(ny)
integer:: gc,mc,sc_p,kc_p,yc_p,xc_p,ycm1_p,gc_p,dcm1_p,ic_temp,srv_ind,kc_mig1,kc_mig2,gctemp,ndlloc,qc_max(nd)
real(dp):: coh_own,coh_p,frac_p,sh_mig_p,frac_mig_p,frac_lv,frac_sng
real(dp):: lab_distr(nd),lab_distrx(nx),prob_srv(3,ni,nj)

integer:: inds2dim(nd,2,2),indsh(nd,2)    
integer:: it_meas, jmax, it_meas_cpl, it_meas_outer,nn, opt_upd,nh,jcc,qc
integer,parameter:: maxit_meas = 4, maxit_meas_cpl = 5, maxit_meas_outer = 10 



do tc=t0,t1

    if (tc==1) agg%pop_j_temp(:,tc) = demo%frac_jt(:,tc)
    if (tc==nt) agg%pop_j_temp(:,tc) = demo%frac_jt(:,tc)
    
    grid%pc_edu_long = 0.0_dp
    grid%pc_earn_long = 0.0_dp

    
    ! initial h0 distn
    !call sub_h0distr(agg%fh0_no(tclb),agg%fh0_cl(tclb),agg%h0norm(tclb),demo%h0prop,agg%h0distr)
    
    opt_ige = .false.
    
    print*, "aggregating PARENTS"

    print*, "all vars", agg%ret(tc), agg%wage_s(:,tc), agg%avwage(tc), agg%avearn(tc), agg%rho_p(tc), agg%beta(tc), agg%psi_lambda(tc)
    
    ! aggregation for parents (that's all we need so far, child initial distr-n is also computed there)
   ! if (opt_calib==1)then
        opt_upd=1
   ! else
   !     opt_upd=0
   ! endif
        
    if (opt_init_ss==1 .or. opt_fin_ss==1)then
        
        opt_cpl_aggr = 1
    
        it_meas = 0
        
        dist = 9000.0_dp
    
        do while (it_meas < maxit_meas)
    
            opt_upd = 0
            
                            
        
            call sub_meas_aggr_full(stat,agg,demo,grid,lc,pol,t0,t1,js,nj,opt_upd,0,0,opt_ige,it_meas,tc,1)
            
            if (it_meas==0)then
                grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
            endif
            
            
            !do hc = 1,np
            !    if (sum( grid%Phi_guess(:,:,:,:,:,:,:,:,hc,:,:,:,js,tc) ) > 0.0_dp)then
            !        agg%probcl(hc) =sum( grid%Phi_guess(:,:,:,:,:,:,:,:,hc,ns-1:ns,:,:,js,tc) ) / sum( grid%Phi_guess(:,:,:,:,:,:,:,:,hc,:,:,:,js,tc) )
            !    else
            !        agg%probcl(hc) =0.0_dp
            !    endif
            !    
            !        
            !enddo !(nd,nx,np,ny,ne,nk,ng,nw,np,ns,ni,ns,nj,nt))
            !
            !print*, agg%probcl
            !pause
            !agg%cons_sj(:,jt,tc) = agg%cons_sj(:,jt,tc) /agg%poppar_skidj(:,jt,tc) 
            agg%ivt_skid(:,tc) = agg%ivt_skid(:,tc) /(earn_si_lo * agg%poppar_skidj(:,jt,tc) ) 
            print*, "cons jt", agg%cons_sj(:,jt,tc)
            print*, "ivt kidedu jt", agg%ivt_skid(:,tc)
            print*, "cons", agg%cons_j(:,tc)
            print*, "indbc excact", agg%indbcexact_j(:,tc)
            print*, "indbc near", agg%indbc_j(:,tc)
            print*, "sav", agg%ass_j(:,tc)/(earn_si_lo * agg%pop_j(:,tc)) 
            print*, "sav1", agg%ass_sj(:,1,tc) 
            print*, "sav2", agg%ass_sj(:,2,tc)
            print*, "sav3", agg%ass_sj(:,3,tc)
            print*, "lab", agg%lab_j(:,tc)
            print*, "nettr", agg%net_trr_j(:,tc)
            print*, "meanwage", agg%meanwage_s(1,tc), agg%meanwage_s(2,tc), agg%meanwage_s(3,tc), agg%meanwage_s(4,tc)
            print*, "slopewage", agg%slopewage_s(:,tc) * agg%h_sj(:,jt,tc)/agg%h_j(jt,tc)  !/agg%meanwage_s(:,tc)
            
            print*, "phi hc", agg%phi_hc
            print*, "wage no, hc", agg%wage_hc(1,:) / agg%phi_shc(1,:)
            print*, "wage co, hc", agg%wage_hc(3,:) / agg%phi_shc(3,:)
        
            if (it_meas>1) dist = maxval(abs( grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)  - grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) ))
            print*, "cur dist", dist, it_meas
            if (dist<epsi) exit
            it_meas = it_meas + 1
            grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
        
        enddo
        
        
     
        opt_upd = 1
        
        opt_ige = .true. 
        opt_cpl_aggr = 1
        
        call sub_meas_aggr_full(stat,agg,demo,grid,lc,pol,t0,t1,js,nj,opt_upd,0,0,opt_ige,it_meas,tc,1)
    
        if (opt_ige_compute>0)then
        
            agg%rhoedu0(tc)=func_rho(grid%phi_edu_long,grid%pc_edu_long,nu_long,nn,.false.)
            if (nn==nu_long) then
                print*, ' '
                print*, 'increase nu_long'
                print*, ' '
            endif
        
            print*, "rho",agg%rhoedu0(tc)
        endif
        
        if (opt_ige_compute==2)then
            
            ! normalize the distr-n
            grid%phi_earn_long = grid%phi_earn_long / (sum(grid%phi_earn_long))
            
            
                
            agg%rhoearn0(tc)=func_rho(grid%phi_earn_long,grid%pc_earn_long,nv_long,nn,.false.)
            if (nn==nv_long) then
                print*, ' '
                print*, 'increase nv_long'
                print*, ' '
            endif
        
            print*, "rhoearn",agg%rhoearn0(tc)
            
        endif
        
        
        print*, "done with PARENTS"
    
      !  call sub_sumstats(grid,pol,stat)
        
    else
        
        opt_ige = .true. 
        
        if (opt_curver==1)then
            if (tc==2)then
                opt_trref = 1
            else
            
                opt_trref = 0
            endif
        endif
        
        
        
        !opt_trref=0
        it_meas = 2
        
        ! transition: no ffpp loop
        opt_upd = 1
        
        opt_cpl_aggr = 1
        
        tcm1=max(1,tc-1)
        !if (tc>2) grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jf-1,tcm1) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jf-1,1) ! max(1,tc-1))
       
        
        !call sub_meas_aggr_full(stat,agg,demo,grid,lc,pol,t0,t1,js,nj,opt_upd,0,0,opt_ige,it_meas,tc,1)
        call sub_meas_aggr_full(stat,agg,demo,grid,lc,pol,t0,t1,js+1,nj,opt_upd,0,0,opt_ige,it_meas,tc,1)
         
        print*, "cons", agg%cons_j(:,tc)
        print*, "sav", agg%ass_j(:,tc)
        print*, "lab", agg%lab_j(:,tc)
        print*, "nettr", agg%net_trr_j(:,tc)
        
        print*, "meanwage", agg%meanwage_s(1,tc), agg%meanwage_s(2,tc), agg%meanwage_s(3,tc)
        
        print*, "done with PARENTS trans per", tc
    
       ! call sub_sumstats(grid,pol,stat)
        
        !opt_trref=1
    endif
    

    
enddo

!opt_trref = 1

if (t0>1)then
   ! opt_trref = 1
    call sub_welfare(agg,demo,grid,pol,stat,lc,t0,t1)
   
else
    
endif
 
call sub_sumstats(agg,grid,pol,stat,t0,t1)  

meas_init_flg =0

end subroutine sub_meas_call
! ---------------------------------------------------------------------------     

    
! ---------------------------------------------------------------------------    
subroutine sub_meas_aggr_full(stat,agg,demo,grid,lc,pol,t0,t1,jmin,jmax,opt_upd,opt_rfg,opt_intp,opt_ige,it_meas,tc,gen_id)
! computation of cross-sectional distributions 
! calls embedded subroutine for aggregation
use funcs_mod,only:func_intp
implicit none

type(t_stat),intent(inout)::stat
type(t_agg),intent(inout)::agg
type(t_demo)::demo
type(t_grid),intent(inout)::grid
type(t_lc),intent(inout)::lc
type(t_pol),intent(inout)::pol
integer,intent(in)::t0,t1,jmin,jmax,opt_upd,opt_rfg,opt_intp,it_meas,tc,gen_id !  jmax is max age in forward aggregation; opt_upd=1: update outer loop variables
logical,intent(in):: opt_ige

integer::sc,kc,jc,ic,ec,yc,pc,xc,dc,jcc,epscm1,epsc1m1,epsc2m1,epsc2
integer:: sc1,sc2,kc1,kc2,yc1,yc2,yc1m1,yc2m1,kc_mig,jc_kid,hquant_ind,pc_cur_min,pc_cur_max,pc_cur
integer::tcm1,tcp1,wc,jcm1,dcm1
integer::icm1,ecm1,ycm1
real(dp)::coh,gr_wage,net_wage,wage_totax,ben_p,gr_ben_p,leis,lab,min_pens,R,dist_ret,Phi_s_kid(ns),wght_kid,wght_s,debtss
real(dp):: gr_wage1,gr_wage2,net_wage1,net_wage2,ben_p1,ben_p2,gr_ben_p1,gr_ben_p2,leis1,leis2,lab1,lab2,agglogh(ns),Phi_s_h(ns,np), agg_hk_s(ns,np),sort_param_tmp, tmp_distr
real(dp)::vals(2)
integer::inds(2)
real(dp),allocatable::prob(:),v_temp(:),ic_vec(:)
integer:: indsd(nd,2)
real(dp):: valsd(nd,2), prob_dc(nd),v_dc(nd), leis_dc(nd), lab_dc(nd), fr_av,savg,savp !, gconst
real(dp):: leis1_dc(nd),leis2_dc(nd),lab1_dc(nd),lab2_dc(nd)
real(dp):: frac_d
real(dp),parameter::epsi=1.0e-08
real(dp)::sum_frac,dist_frac,migr_frac,frac
real(dp)::frac_mig,sh_mig
logical,parameter::opt_check=.false.
integer,allocatable,dimension(:) :: dc_vec
integer:: dc_temp, yc_max, dcl, nd_glob,nd_loc
real(dp):: v
real(dp):: pini(ny),pini_ret(ny),prob_tr_ret(ny,ny),avnetwageLT !,prob_tr_mig(ny,ny),pini_mig(ny)
integer:: gc,mc,sc_p,kc_p,yc_p,xc_p,ycm1_p,gc_p,dcm1_p,ic_temp,srv_ind,kc_mig1,kc_mig2,gctemp,ndlloc,qc_max(nd),xc_sw,gcpar,epsc,epsc1,espc2,gc_min,gc_max,xc1,xc2,dc2m1,dc1m1_max,dc2m1_max,xc2_min,xc2_max
real(dp):: coh_own,coh_p,frac_p,sh_mig_p,frac_mig_p,frac_lv,frac_sng,wght_hs_s,wght_hs_gs,afpdv,perfsort_flg
real(dp):: lab_distr(nd),lab_distrx(nx),prob_srv(3,ni,nj)
real(dp):: cit_trans(ni,ni),pop_j_cpl(nj),pop_ij_cpl(ni,nj),pop_ij_sng(ni,nj),& 
    agg_vec(34),agg_str_s(5,ns),agg_str_gj(5,2,nj),agg_str_g(5,2), & 
    agg_str_sj_intp(2,ns,nj),agg_vec_intp(2),agg_str_s_intp(2,ns), &
    dist_distr,gcpss,ass_grid(nx),agg_str(8,1,2,nj,ns),Rafter
real(dp), allocatable:: Phi(:,:,:,:,:,:,:,:,:,:,:,:),Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid_cpl(:,:,:,:,:,:,:,:,:,:,:,:),Phi_child(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid_old(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid_old_cpl(:,:,:,:,:,:,:,:,:,:,:,:)
integer:: jc_migr,jcm1_migr_max,jcm1_migr,ind_sw1,ind_sw2,indm1_sw1,indm1_sw2,indm1_sw1_max,indm1_sw2_max,xc_loc,ickid,hckid_max,ickid_min,gckid_min,ickid_max,gckid_max,ickidm1,ic_min,ic_max,hc,hcm1,xc_dec,cutoff1,cutoff2
integer:: sc_kid,kc_kid,ec_kid,yc_kid,hckid,dckid,opt_kid,uc,nc,vc,scpar,hcc,hq,jcloc,hcc_min,hcc_max,xcpar,ycpar,hcc_temp,kcp1,kcp1_min,kcp1_max
logical:: nan_flg(nd)
real(dp):: ass_loc,av_prod,bnet,ass,logsum,val_sc(ns),logsum_kid_educ,prob_kid_educ(ns),prob_ni(np),tinv_slope_j(nj),moninv_slope_j(nj),cl_sh(nx),hp1_hs,wght_hs,Phi_hk(np),hk_cum,hk_cum1,hk_cum2,Phi_h(np),avnetwage !,hc_foc_error_j(nj)
real(dp)::vals2dim(nd,2,2),valsh(nd,2),vals_h(2),hk_kid,frac_in,ass_cum,ret_old,afHS,afLHS,totres,totres_gsx,totresgross,totresgross_gsx,probfe(nk),prob_sc_kid(ns),cons
integer:: inds2dim(nd,2,2),indsh(nd,2),inds_h(2),hcc_loc,nh,jc_child,epsc_kid,gc_kid,indsd_s(ns,nd,nk,ny,nw,2),nsloc,sc2_max,kc2_max,yc2m1_max,epsc2m1_max,hcm1_max
character(1)::i_char
character(2)::tc_char
real(dp),allocatable,dimension(:)::distrvec,yvec
real(dp),dimension(2):: gr_wage_cpl,net_wage_cpl,wage_totax_cpl,gr_ben_p_cpl,ben_p_cpl
integer:: xc3(2), kc3(2), sc3(2), yc3m1(2), epsc3m1(2)
real(dp):: gr_wage_kid,net_wage_kid,gr_ben_p_kid,ben_p_kid,leis_dc_s(ns,ndl),lab_dc_s(ns,ndl),valsd_s(ns,nd,nk,ny,nw,2),wage_totax_kid,prob_sc(ns),prob_dc_s(ns,nk,ny,nw,nd),v_dc_s(ns,nd)
 
!probmar(1) = 1.0_dp !prob_mar_s(:,1)
!probmar(2) = 0.0_dp

pini(:)=demo%pini
pini_ret = 0.0_dp
pini_ret(1) =1.0_dp 
prob_tr_ret = 0.0_dp
prob_tr_ret(1,1)=1.0_dp
if (ny==2)then
    prob_tr_ret(2,2)=1.0_dp
endif

allocate( Phi(nd,nx,np,ny,ne,nk,ng,nw,np,ns,1,ns))
allocate( Phi_cpl(nd,nx,np,ny,ny,ne,nk,nk,nw,nw,ns,ns))
allocate(Phi_kid(nd,nx,np,ny,ne,nk,ng,nw,np,ns,1,ns))
allocate(Phi_kid_old(nd,nx,np,ny,ne,nk,ng,nw,np,ns,1,ns))
allocate(Phi_kid_cpl(nd,nx,np,ny,ne,nk,ng,nw,np,ns,1,ns))
allocate(Phi_kid_old_cpl(nd,nx,np,ny,ne,nk,ng,nw,np,ns,1,ns))
allocate(Phi_child(nd,nx,np,ny,ne,nk,ng,nw,np,ns,1,ns))



!print*, "ass jf", grid%ass(1,1,:,:,1,:,:,1,1,:,1,1,jf,tc)
!pause

print*, "start agg"
!pause


!do tc=t0,t1
if (t0==t1) then
    tcm1=tc
    tcp1=tc
       
else
   
    tcm1=max(1,tc-1)
    tcp1=min(nt,tc+1)   ! careful: when computing transition, have to start in period 1, so that capital stock in period 2 is computed
    
endif
    
ret_old = agg%ret(tc)
! gross interest rate in tc
R=1.0_dp+agg%ret(tc)

agg_vec = 0.0_dp
agg_str = 0.0_dp
    
agg_str_s= 0.0_dp
agg_str_gj= 0.0_dp 
agg_str_g= 0.0_dp
agg%totnetres_j(:,tc) = 0.0_dp

agg%popfull(tc) = 0.0_dp

agg%phi_hc = 0.0_dp
agg%pop_j(:,tc) = 0.0_dp
agg%pop_sj(:,:,tc) = 0.0_dp
agg%pop_s(:,tc) = 0.0_dp

agg%phi_shc = 0.0_dp
agg%wage_hc =0.0_dp

agg%ass(tc) = 0.0_dp
agg%sav(tc) = 0.0_dp
agg%regretcl(tc) = 0.0_dp
agg%valuedrop(tc) = 0.0_dp

agg%meanwage_s(:,tc) = 0.0_dp
agg%slopewage_s(:,tc) = 0.0_dp

agg%emprate(tc) = 0.0_dp

agg%cev_s(:,tc) = 0.0_dp
agg%hkja_s(:,tc) = 0.0_dp
agg%hkjacol_s(:,tc) = 0.0_dp
agg%popja_s(:,tc) = 0.0_dp
agg%popjacol_s(:,tc) = 0.0_dp

agg%avtax(tc) = 0.0_dp
agg%avtax_av(tc) = 0.0_dp
agg%avtax_j(:,tc) = 0.0_dp
agg%avtax_av_j(:,tc) = 0.0_dp
agg%avtaxbot(tc) = 0.0_dp
agg%margtaxbot(tc) = 0.0_dp
agg%avtaxbot_frac(tc) = 0.0_dp
agg%avtaxrat(tc) = 0.0_dp

agg%moninv_tot(tc) = 0.0_dp
agg%tinv_tot(tc) = 0.0_dp
agg%hk_tot(tc) = 0.0_dp
agg%clshr_tot(tc) = 0.0_dp
agg%ivt_tot(tc) = 0.0_dp
agg%pop_tot(tc) = 0.0_dp

agg%moninvratio(tc) = 0.0_dp

agg%frac_gs(:,:) = 0.0_dp

agg%clshr_jasp(:,:,tc ) = 0.0_dp
agg%hk_jasp(:,:,tc ) = 0.0_dp
agg%pop_jasp(:,:,:,tc ) = 0.0_dp
    ! for aggregation purposes: initialization
    
    agg_vec = 0.0_dp
    agg_str = 0.0_dp
    
    agg_str_s= 0.0_dp
    agg_str_gj= 0.0_dp 
    agg_str_g= 0.0_dp
    
    agg%wage_inc_s(:,tc)  =0.0_dp
    agg%fe_s(:,tc) = 0.0_dp
    agg%pop_s(:,tc) = 0.0_dp
    
    agg%earnHS_j(:,tc) = 0.0_dp

    agg%earnHS_j_LEV(:,tc) = 0.0_dp
       
    agg%tot_inc_j(:,tc) = 0.0_dp
    agg%labinctaxrv_j(:,tc) = 0.0_dp
    agg%capinctaxrv_j(:,tc) = 0.0_dp
    agg%constaxrv_j(:,tc) = 0.0_dp
    agg%pccontr_j(:,tc) = 0.0_dp
    
    agg%lab_s(:,tc)  = 0.0_dp

    agg%indbc_j(:,tc) = 0.0_dp
    agg%indbcexact_j(:,tc) = 0.0_dp
  
    agg%net_trr_j(:,tc) = 0.0_dp
    
    agg%poppar(tc) = 0.0_dp
    agg%poppar_ivt(tc) = 0.0_dp
    agg%poppar_inv(tc) = 0.0_dp
    agg%moninv(tc) = 0.0_dp
    agg%tinv(tc) = 0.0_dp
  !  agg%moninv_x(:,tc) = 0.0_dp
  !  agg%tinv_x(:,tc) = 0.0_dp
    agg%ivt(tc) = 0.0_dp
    agg%ivt_pkid(tc) = 0.0_dp
    agg%ivt_s(:,tc) = 0.0_dp
    agg%ivt_skid(:,tc) = 0.0_dp
    agg%ivt_sg(:,:,tc) = 0.0_dp
    agg%ivt_sg_cpl(:,:,tc) = 0.0_dp
    agg%t_s(:,tc) = 0.0_dp
    agg%m_s(:,tc) = 0.0_dp
    agg%poppar_s(:,tc) = 0.0_dp
    agg%poppar_sj(:,:,tc) = 0.0_dp
    agg%pop_sj(:,:,tc) = 0.0_dp
    agg%poppar_skidj(:,:,tc) = 0.0_dp
    agg%t_sj(:,:,tc) = 0.0_dp
    agg%m_sj(:,:,tc) = 0.0_dp
    agg%h_sj(:,:,tc) = 0.0_dp
    agg%poppar_sgj(:,:,:,tc) = 0.0_dp
    agg%poppar_sgj_cpl(:,:,:,tc) = 0.0_dp
    agg%poppar_ssgj_cpl(:,:,:,:,tc) = 0.0_dp
    agg%t_sgj(:,:,:,tc) = 0.0_dp
    agg%m_sgj(:,:,:,tc) = 0.0_dp
    agg%h_sgj(:,:,:,tc) = 0.0_dp
    agg%t_sgj_cpl(:,:,:,tc) = 0.0_dp
    agg%m_sgj_cpl(:,:,:,tc) = 0.0_dp
    agg%h_sgj_cpl(:,:,:,tc) = 0.0_dp
    agg%t_ssgj_cpl(:,:,:,:,tc) = 0.0_dp
    agg%m_ssgj_cpl(:,:,:,:,tc) = 0.0_dp
    agg%h_ssgj_cpl(:,:,:,:,tc) = 0.0_dp
   
    agg%pop_kid_s(:,tc) = 0.0_dp
    
    agg%grwage(tc) = 0.0_dp
        
    agg%m_j(:,tc) = 0.0_dp
    agg%t_j(:,tc) = 0.0_dp
    agg%h_j(:,tc) = 0.0_dp
    agg%m_yj(:,:,tc) = 0.0_dp
    agg%t_yj(:,:,tc) = 0.0_dp
    agg%h_yj(:,:,tc) = 0.0_dp
    agg%inv_j(:,tc) = 0.0_dp
    
    !agg%m_xj(:,:,tc) = 0.0_dp
    !agg%t_xj(:,:,tc) = 0.0_dp
    
    agg%poppar_j(:,tc) = 0.0_dp
        
    agg%cons_j(:,tc) = 0.0_dp
    agg%ass_j(:,tc) = 0.0_dp
    agg%lab_j(:,tc) = 0.0_dp
    agg%ky_j(:,tc) = 0.0_dp
    
    agg%lab_sj(:,:,tc) = 0.0_dp
    agg%cons_sj(:,:,tc) = 0.0_dp
    agg%ass_sj(:,:,tc) = 0.0_dp
    agg%frac_bc_sj(:,:,tc) = 0.0_dp
    agg%frac_nettr_sj(:,:,tc) = 0.0_dp
    agg%labinc_sj(:,:,tc) = 0.0_dp
    agg%totinc_sj(:,:,tc) = 0.0_dp
        
    agg%asswp(tc) = 0.0_dp
    agg%asswp_j(:,tc) = 0.0_dp
    agg%net_wage_inc_j(:,tc) = 0.0_dp
    agg%wage_inc_j(:,tc) = 0.0_dp
    
    agg%igov(tc) = 0.0_dp
    agg%igov_kg(tc) = 0.0_dp
    agg%igov_hs(tc) = 0.0_dp
    agg%igov_hs_base(tc) = 0.0_dp
    agg%clsubs(tc) = 0.0_dp
    agg%clcost(tc) = 0.0_dp
    
    agg%avprem(:,tc) = 0.0_dp
    agg%avprem_pop(:,tc) = 0.0_dp
    agg%marprem(:,tc) = 0.0_dp
    agg%marprem_pop(:,tc) = 0.0_dp
    
    agg%educ_matrix(:,:,tc) = 0.0_dp
    agg%educ_matrix_g(:,:,:,tc) = 0.0_dp
    agg%educ_matrix_mg(:,:,:,:,tc) = 0.0_dp
    
    agg%hc_foc_error_j = 0.0_dp
    
    agg%hsvtmp1(tc) = 0.0_dp
    agg%labinctaxrv(tc) = 0.0_dp
    agg%hsvtmp2(tc) = 0.0_dp
    
    agg%v_delta(:,tc) = 0.0_dp
    agg%v_kid(:,tc) = 0.0_dp
    
    print*, "start agg3"
   
    !if (opt_vf_init==1 .and. it_meas==0)then
    !    grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)=0.0_dp
    !else
    !    grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jmin+1:nj,tc)=0.0_dp
    !endif
        
    !    print*, "start agg4"
        
    !grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)=0.0_dp
   ! if (opt_vf_init==1 .and. it_meas==0) grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,:,:)=0.0_dp
    
   ! grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,:,:)=0.0_dp
    
    !print*, "start agg5"


print*, "starting"
    
do jc=jmin,jmax !jf-2,jmax    ! age today
  
    opt_kid = 0
               
    !print*, "age in aggr", jc 
    !pause
        
    Phi = 0.0_dp
        
    jcm1=max(jc-1,1)    ! age yesterday
                
    ! initial measures
    
    ndlloc=ndl
        
    nc = 0
    !grid%phi_inv_long = 0.0_dp
    !grid%p_inv_long = 0.0_dp
        
    if (gen_id==1 .and. jc==js)then
        
        if (opt_vf_init==1 .and. it_meas ==0 .and. opt_init_ss==1)then
            
            print*, "Initializing parental measure jf"
            
            
                do ickid=1,1 !np

                    !!$OMP PARALLEL private(gc,sc,kc,ic,yc,ec,frac,dc_vec,dcl,coh,nd_glob,prob,v_temp,prob_dc,gr_wage,net_wage,ben_p,leis_dc,lab_dc,valsd,indsd,min_pens,dc,frac_d)
                    !!$OMP DO collapse(6) , reduction(+:Phi,agg_vec,agg_str)
                    do gc=1,ng
                        !do hc = np,np! acquired human capital
                            do ic=1,1 !  innate ability
                 
                                do sc=1,ns  
                                
                                    call basefun(grid%hk_grid,np,1.12_dp,vals,inds)
                                    prob_ni = 0.0_dp
                                    prob_ni(inds(1) ) = vals(1)
                                    prob_ni(inds(2) ) = vals(2)
                                    do hc=inds(1),inds(2)! !np-2,np-2
                                
                                    do pc=1,1 !inds(1),inds(2)!np
                                    
                                        do kc=1,nk
                                            do yc=1,ny
                                                do epsc=1,nw
                                                do ec=1,1  ! emplyoment state ne is retirement
                                                    
                                                    do xc_dec = 1,5
                           
                                                        ! first: solution for  stock
                                                        ! look at fraction in population of gender gc, education sc, wealth quintile / quartile xc_dec, child ability ickid
                                                        frac=  ns**(-1.0_dp)* 5**(-1.0_dp) * prob_ni(hc) * nk**(-1.0_dp) * ng**(-1.0_dp)
                                
                                    
                                                        ! if (opt_cpl ) frac = frac * demo%prob_mrg(gc,ic,sc,2,tc)
                                    
                                                        if (frac==0.0_dp) cycle
                                    
                                                        ! dc probabilities
                                                        allocate(dc_vec(ndl))
                                                        do dcl=1,ndl
                                                            dc_vec(dcl)=dcl
                                                        enddo
                                  
                                                        ! out of this fraction, look at the fraction with specific productivity and income shock:
                                                        frac=frac*demo%pini(yc) * demo%prob_epsi(epsc)
                                    
                                                        ! distribute this fraction according to cash-on-hand
                                                        ! wealth component of cash on hand, income component added below
                                                        !coh=demo%ass_input(gc,sc,xc_dec) ! !+ agg%tr_isj(ic,sc,jc,tc)  *f_aftertaxR(R,agg%tau_k(tc))
                                 
                                                        ass= 0.0_dp !demo%ass_input(gc,sc,xc_dec)  
                                                    
                                                        nd_glob = size(dc_vec)
                                
                                                        allocate(prob(nd_glob))
                                                        allocate(v_temp(nd_glob))
                                                        if (opt_intp==0)then
                                                            call sub_prob_dc_new_out(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,ec,kc,hc,yc,epsc,ic,sc,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,logsum,prob,prob_dc,v_dc, &
                                                                min_pens,nd_glob,ndl,pc,ickid,1,gen_id)
                                                        else
                                                            !call sub_prob_dc_new_intp(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,ec,kc,hc,yc,ic,sc,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,prob,prob_dc, &
                                                            !    min_pens,nd_glob,ndl,pc,ickid,gckid,gen_id)
                                                        endif
                                        
                                                        ! tbc put assert statement here
                                                        if (abs(sum(prob_dc)-1.0_dp)>epsi)then
                                                            print*, "dc prob should sum up to one sng",prob_dc,jc
                                                            pause
                                                        endif
                                   
                                                        do dc=1,nd
                                    
                                                            if (prob_dc(dc)==0.0) cycle
                                        
                                                            vals = valsd(dc,:)
                                                            inds = indsd(dc,:)
                                                           
                                                            lab = lab_dc(dc)
                                          
                                                            frac_d = frac  *  prob_dc(dc)                                             
                                            
                                                            if (frac_d>0.0_dp .and. isnan(v_dc(dc)))then
                                                                print*, "NaN value with positive weight?",jc,ec,dc
                                                                !pause
                                                            endif
                                           
                                                                call sub_compudistr_out(Phi,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,inds,vals, &
                                                                    gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,leis,lab,ec,dc,yc,epsc,kc,hc,sc,ic,gc,jc,tc,tcp1,jmax,ndl,pc,ickid,1,opt_kid,uc,vc,1,1,1,1,nc)
                                                           
                                       
                                                        enddo
                            
                                                        deallocate(prob,v_temp,dc_vec)        
                                                        
                                                    enddo
                                                    
                                    
                                                end do  ! end do ec
                                                enddo
                                            enddo
                                        end do  ! end co ic
                                enddo
                      
                            enddo
                        enddo
                    enddo
                enddo
             
            enddo ! enddo gc
            if (opt_intp==0)then
                grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = Phi
            else
                grid%Phi_int(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = Phi
            endif
            
            print*, "done with parental measure initialization"
            !pause
            
        else
            
            Phi = 0.0_dp
            Phi_kid = 0.0_dp
            Phi_kid_old = 0.0_dp
            jcm1 = jt
        
            if (gen_id ==1)then
            
            
                ndlloc = ndl
            
                do mc = 1,2
                
                    if (mc==1)then
                        gc_max = ng
                        gc_min = ng
                        sc2_max = 1
                        kc2_max = 1
                        yc2m1_max = 1
                        epsc2m1_max = 1
                        hcm1_max = 1
                    
                    else
                        gc_max = 1
                        gc_min = 1
                        sc2_max = ns
                        kc2_max = nk
                        yc2m1_max = ny
                        epsc2m1_max = nw
                        hcm1_max = 1
                    endif
                
                    do gc = gc_min,gc_max
                      do sc=1,ns
                        do sc2=1,sc2_max
            
                    do gc_kid = 1,ng
                        do xc_dec = 1,1 
                
                            do scpar = 1,1
                                
                                    do ickidm1=1,1 !ni !ickid_max: CURRENTLY this dimension is irrelevant, because for all ages operate on hk_grid, thus, pc dimension
                    
                                        if (gen_id==1 .and. jc>=jf .and. jc<=jt)then
                                            ickid= ickidm1
                                        else
                                            ickid= 1
                                        endif
                                        
                                        do hcm1=1,hcm1_max ! own hc yesterd
                        
                                            hc=1 !hcm1 ! own hc today, this is aggregation for parents only so far, thus, no loop over own HK needed
                   
                                           ! do gc=gc_min,gc_max !ng        
                                                do icm1=1,ni  ! parental h0 yesterday    
                                            !        do sc=1,ns  ! skill level
                                            !            do sc2=1,sc2_max
                                 
                                                            do kc=1,nk  ! fixed effect 
                                                                do kc2 = 1,kc2_max
                                        
                                                                    do ecm1=1,ne !ec1(jcm1,tcm1),ec2(jcm1,tcm1)  ! employment state yesterday 
                                                                        do dcm1 =1,nd ! ec1(jcm1,tcm1),ec2(jcm1,tcm1)
                                                
                                                                            do ycm1=1,ny  ! income states yesterday 
                                                                                do yc2m1 = 1,yc2m1_max
                                                                                    do epscm1=1,nw
                                                                                        do epsc2m1 = 1,epsc2m1_max
                                
                                                                                            ! transition of home population:
                                                                                            do pc=1,np ! child acquired hc yesterday
                                                        
                                                                                                kcp1_min = 1
                                                                                                kcp1_max = nk
                                                                                                if (nk>1)then
                                                                                                    probfe(nk) = f_probfe(grid%hk_grid(pc),1,agg%gammah_s(1,tc))
                                                                                                    probfe(1) = 1.0_dp -probfe(nk) 
                                                                                                else
                                                                                                    probfe(1) = 1.0_dp
                                                                                                endif
                                       
                                                      
                                                                                                do pc_cur = 1,1
                                                        
                                                                                                    do xc=1,nx  ! cash on hand yesterday
                                                                               
                                                                                                        ! look at fraction of population at this position yesterday:
                                                                                                        if (mc==1)then
                                                                                                            frac=grid%Phi(dcm1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1) /0.5_dp
                                                                                                        else
                                                                                                            frac=grid%Phi_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,kc,kc2,epscm1,epsc2m1,sc,sc2,jcm1,tcm1) !*0.5_dp
                                                                                                        endif                                      
                                                                                                                          
                                                                                                        if ( frac==0.0 ) cycle   
                                                                                        
                                                                                                        do ic=icm1,icm1 
                                                              
                                                                                                            ec = 1                                      
                                                    
                                                                                                            ! for each ec, determine set of discrete choices available
                                                                   
                                                                       
                                                                                                            allocate(dc_vec(ndl))
                                                                                                            do dcl=1,ndl
                                                                                                                dc_vec(dcl)=dcl
                                                                                                            enddo
                                                                                                            nd_glob = size(dc_vec)
                                                                                                            allocate(v_temp(nd_glob),prob(nd_glob) )
                                                                                    
                                                                                                            ! when entering loop, always reset fraction
                                                                    
                                                                                                            if (mc==1)then
                                                                                                                frac=grid%Phi(dcm1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)/0.5_dp
                                                                                                            else
                                                                                                                frac=grid%Phi_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,kc,kc2,epscm1,epsc2m1,sc,sc2,jcm1,tcm1)
                                                                                                            endif  
                                                                     
                                                                                                            prob_sc_kid = 0.0_dp
                                                                    
                                                                                                            ! dc probabilities
                                                                                                            do dcl=1,ndl
                                                                                                                dc_vec(dcl)=dcl
                                                                                                            enddo
                                                                                                            ! distribute this fraction according to cash-on-hand
                                                                                                            ! wealth component of cash on hand, income component added below
                                                                                                            Rafter = f_aftertaxR(agg%ret(tc) + 1.0_dp,agg%tau_k(tc))
                                                                                                            if (mc==1)then
                                                                                        
                                                                                                                bnet =pol%b(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,sc,hcm1,scpar,tcm1)
                                                                                                                cons = pol%cons(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,1,scpar,jt,tcm1) !(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
                                                                                                            else
                                                                                                                bnet =pol%b_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,tcm1)
                                                                                                                cons = pol%cons_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,jt,tcm1)
                                                                                                            endif
                                                                                    
                                                                                                            ass = bnet/Rafter   !+ agg%tr_isj(ic,sc,jc,tc)  *f_aftertaxR(R,agg%tau_k(tc))
           
                                                                                                            if (ass<grid%amin_age(gc_kid,3,2,js) ) then
               
                                                                                                                nsloc = ns-1
                                                                                                            else
                                                                                                                nsloc = ns 
                                                                                                            endif
                                                                
                                                                                                            if (mc==1)then
                                                                        
                                                                                                                call sub_prob_dc_new_kid(agg,demo,grid,pol,pol%b(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,sc,hcm1,scpar,tcm1),gr_wage_kid,net_wage_kid,wage_totax_kid, & 
                                                                                                                    gr_ben_p_kid,ben_p_kid,ec, &
                                                                                                                    pc,1,gc_kid,js,tc,js,tcm1,dc_vec,lab_dc_s,valsd_s,indsd_s,v_temp,logsum,prob,prob_dc_s,prob_sc,v_dc_s,min_pens,ndl,ndl,nsloc,sc,Rafter)
                                                                                                            else
                                                                                                                call sub_prob_dc_new_kid(agg,demo,grid,pol,pol%b_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,tcm1),gr_wage_kid,net_wage_kid,wage_totax_kid, & 
                                                                                                                    gr_ben_p_kid,ben_p_kid,ec, &
                                                                                                                    pc,1,gc_kid,js,tc,js,tcm1,dc_vec,lab_dc_s,valsd_s,indsd_s,v_temp,logsum,prob,prob_dc_s,prob_sc,v_dc_s,min_pens,ndl,ndl,nsloc,max(sc,sc2),Rafter)
                   
                                                                                                            endif
                                                                                    
                                                                   
                                                                        
                                                        
                                                                                                            prob_ni = 0.0_dp
                                                            
                                                                                                            prob_ni(1) = 1.0_dp
                                                            
                                                            
                                                                                                            do sc_kid = 1,nsloc
                                                                                                                
                                                                                                                if (nk>1)then
                                                                                                                    probfe(nk) = f_probfe(grid%hk_grid(pc),1,agg%gammah_s(1,tc))
                                                                                                                    probfe(1) = 1.0_dp -probfe(nk) 
                                                                                                                else
                                                                                                                    probfe(1) = 1.0_dp
                                                                                                                endif
               
                                                                                                                do kc_kid = 1,nk
                
                                                                                                                    do yc_kid = 1,ny
                                                                                                                        do epsc_kid = 1,nw
                                                                                                                            
                                                                                                                            
                                   
                                                                                                                            do dckid=1,nd
                                                                                                                                
                                                                                                                                ! THEY ALL WORK AT NO WAGES
                                                                                                                                if (sc_kid<ns-1)then
                                                                                                                                    call sub_wage(gr_wage_kid,net_wage_kid,wage_totax_kid,agg%wage_s(sc_kid,tc),demo%ageprod(gc_kid,js,sc_kid),demo%grid_y(yc_kid),grid%hk_grid(pc), & 
                                                                                                                                        agg%tau_p(tc),sc_kid,kc_kid,dckid,1,js,1,agg%gammah_s(sc_kid,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                                                                                                                                    
                                                                                                                                else
                                                                                                                                    call sub_wage(gr_wage_kid,net_wage_kid,wage_totax_kid,agg%wage_s(1,tc),demo%ageprod(gc_kid,js,1),demo%grid_y(yc_kid),grid%hk_grid(pc), & 
                                                                                                                                        agg%tau_p(tc),1,kc_kid,dckid,1,js,1,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                                                                                                                                endif
                                                                                                                                
                                                                                                                                !gr_wage = gammafe(sc_kid,kc_kid)
                                    
                                                                                                                                if (prob_dc_s(sc_kid,kc_kid,yc_kid,epsc_kid,dckid)==0.0) cycle
                                        
                                                                                                                                vals = valsd_s(sc_kid,dckid,kc_kid,yc_kid,epsc_kid,:)
                                                                                                                                inds = indsd_s(sc_kid,dckid,kc_kid,yc_kid,epsc_kid,:)
                                                                                                                                leis = leis_dc_s(sc_kid,dckid)
                                                                                                                                lab = lab_dc_s(sc_kid,dckid)
                                          
                                                                                                                                frac_d = frac  *  prob_dc_s(sc_kid,kc_kid,yc_kid,epsc_kid,dckid)  * prob_sc(sc_kid) *probfe(kc_kid)  * demo%pini(yc_kid) * demo%prob_epsi(epsc_kid) * ng**(-1.0_dp) !* probmar_s(mc,gcpar,scpar)
                                                                                                                                if (mc==1)then
                                                                                                                                    frac_d = frac_d * probmar_s(mc,gc,sc)
                                                                                                                                else
                                                                                                                                    frac_d = frac_d * ( probmar_s(mc,1,sc) + probmar_s(mc,2,sc2)) * 0.5_dp
                                                                                                                                endif
                                                                                                                                agg%frac_gs(gc_kid,sc_kid) = agg%frac_gs(gc_kid,sc_kid) + frac_d 
                                                                                                                                
                                                                                                                                    
                                                                                                                                prob_sc_kid(sc_kid) = prob_sc_kid(sc_kid) + frac_d                    
                                                               
                                                                                                                                if (frac_d>0.0_dp .and. isnan(v_dc_s(sc_kid,dckid)))then
                                                                                                                                    print*, "NaN value with positive weight?",js,ec_kid,dckid
                                                                                                                                    !pause
                                                                                                                                endif
                                                                                                                                
                                                                                                                                !agg%cons_sj(sc_kid,jt,tc) = agg%cons_sj(sc_kid,jt,tc) + frac_d *  cons
                                                                                                                                agg%ivt_skid(sc_kid,tc) = agg%ivt_skid(sc_kid,tc)  + frac_d *  bnet
                                                                                                                                agg%poppar_skidj(sc_kid,jt,tc) = agg%poppar_skidj(sc_kid,jt,tc) + frac_d 
                                                                                                                                if (mc==1)then
                                                                                                                                    agg%v_delta(jt,tc) =  agg%v_delta(jt,tc) + (pol%v(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,1,scpar,jt,tcm1) - pol%v_nokid(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,1,scpar,jt,tcm1) )*  frac_d
                                                                                                                                else
                                                                                                                                     agg%v_delta(jt,tc) =  agg%v_delta(jt,tc) + (pol%v_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,jt,tcm1) -pol%v_nokid_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,jt,tcm1) ) *  frac_d
                                                                                                                                endif
                                                                                                                                
                  
                                                                                                                                if (mc==1)then
                                                                                                                                    call sub_compudistr_out_kid(Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,inds,vals, &
                                                                                                                                        gr_wage_kid,net_wage_kid,wage_totax_kid,gr_ben_p_kid,ben_p_kid,leis,lab,ec,dckid,yc_kid,epsc_kid,kc_kid,pc,sc_kid,ickid,gc_kid,js,tc,tcp1,js,ndl,1,1,1,1,uc,vc,sc,sc,mc,gc,xc_dec,ycm1,epscm1,kc,nc)
                                                                                                                                else
                                                                                                                                    call sub_compudistr_out_kid(Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,inds,vals, &
                                                                                                                                        gr_wage_kid,net_wage_kid,wage_totax_kid,gr_ben_p_kid,ben_p_kid,leis,lab,ec,dckid,yc_kid,epsc_kid,kc_kid,pc,sc_kid,ickid,gc_kid,js,tc,tcp1,js,ndl,1,1,1,1,uc,vc,sc,sc2,mc,1,xc_dec,ycm1,epscm1,kc,nc)
                                                                                                                                endif
                                                                                                        
                                       
                                                                                                                            enddo
                            
                                                                                                                        enddo
                                                                                                                    enddo
                
                                                                            
                                                                                                                enddo
                                                                                                            enddo
                                                                                                            deallocate(dc_vec,v_temp,prob)
                                                                                                        end do  ! end do yc
                                                
                                                                                                    end do ! end do ic
                                                   
                                                                                                end do  ! end do xc
                                                        
                                                                                            enddo
                                                                                        end do  ! end do pc
                                                                                    enddo

                                                                                end do  ! end do ycm1
                                                
                                                                            enddo
                                                                        end do  ! end do ecm1
                                                                    enddo
                                                                end do  ! end do icm1
                                                            enddo
                                                        enddo
                
                                                    enddo ! enddo gc
                                                enddo
                                            enddo
                                        enddo
                                    enddo
                                
                            enddo
                        enddo
                    enddo
                    
                    if (mc==1)then
                        grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,mc,tc) = Phi_kid
                        print*, "sumheremc1", sum(Phi_kid)
                    else
                        grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,mc,tc) = Phi_kid - grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,1,tc)
                        print*, "sumheremc2",sum(grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,mc,tc)), sum(Phi_kid)
                    endif
                    
                    
                enddo
            
           
        
           
                !!$OMP END DO 
                !!$OMP END PARALLEL
           
            !if (probmar(2)>0.0_dp .and. opt_cpl_aggr==1)then
            !    grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = Phi_kid +   Phi_kid_cpl
            !
            !    grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)=  Phi_kid +   Phi_kid_cpl
            !else
                !grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = Phi_kid !/probmar(1)
                grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)=Phi_kid !/probmar(1)
            
                grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
            !endif
        
                
            
            if (abs(sum(grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)) - 1.0_dp) > epsi)then
                print*, "beep kid update",sum(grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)) ,tc,sum(Phi_kid),sum(Phi_kid_cpl) 
                pause
            else ! correct round off            
                grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)/sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc))
                
                print*,"kids educ shares", tc, sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,3,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,2,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                print*, " gr wage", tc, agg%grwage(tc)
                !  pause
                
             !   grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) / sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc))
            
                agg%lhsfrac(tc) = agg%pop_kid_s(1,tc) / sum(agg%pop_kid_s(:,tc)) !sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                agg%clfrac(tc) =agg%pop_kid_s(ns,tc) / sum(agg%pop_kid_s(:,tc)) ! sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,3,:,:,js,tc))
                agg%dropfrac(tc) =agg%pop_kid_s(ns-1,tc) / sum(agg%pop_kid_s(:,tc)) 
            
            
            
                !print*, "now", agg%hsfrac_str(tc) 
            
                endif
       
            print*, "frac regret enrol", agg%regretcl(tc) / agg%pop_j(js,tc)
            print*, "frac value drop", agg%valuedrop(tc) / agg%pop_j(js,tc) 

            print*, "edu genderSC1", agg%frac_gs(:,1) / sum(agg%frac_gs(:,1))
            print*, "edu genderSC2", agg%frac_gs(:,2) / sum(agg%frac_gs(:,2))
            print*, "edu genderGC1", agg%frac_gs(1,:) / sum(agg%frac_gs(1,:))
            print*, "edu genderGC2", agg%frac_gs(2,:) / sum(agg%frac_gs(2,:))
            
            
  
            
        !if (tc>2 .and. tc<nt)then ! transition
        !    grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
        !elseif (tc==2)then
        !    grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,1)
        !endif
        
            print*, "avearn3040", avearn3040,agg%h_j(jt+1,tc)
            !pause

            !print*, "constrr frac", agg%indbc_j(:,tc)
            !pause
            open(unit=116,file='output/fracnegass.txt')
            do jcloc=jf,nj
               write(116,'(4f50.16)') agg%indbc_j(jcloc,tc) 
            enddo
            !pause
            
  !  endif
    
        
        open(unit=16,file='output/polfun.txt')
        
        do jcloc=jf,jt-1
            do sc=1,ns
                do gc=1,ng
                    do pc=1,np
                        do xc=1,nx
                            write(16,'(4f50.16)') grid%coh(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),pol%m(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),pol%t1(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),grid%hk_grid(pc)    
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        close (16)
        
        
        open(unit=236,file='output/polfunb.txt')
        
        do jcloc=jt,jt
            do sc=1,ns
                do gc=1,ng
                    do pc=1,np
                        do xc=1,nx
                            write(236,'(2f50.16)') grid%coh(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),pol%b(1,xc,pc,1,1,1,gc,1,sc,1,1,tc)    
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        close (236)
        
        
        
        
        open(unit=17,file='output/vchild.txt')
        
        do jcloc=js,js
            do sc=1,ns
                do gc=1,ng
                    do pc=1,np
                        do xc=1,nx
                            write(17,'(2f50.16)') grid%coh_child(1,xc,1,1,1,1,gc,1,pc,sc,1,1,jcloc,tc),pol%v_child(1,xc,1,1,1,1,gc,1,pc,sc,1,1,jcloc,tc) 
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        close (17)

        
        !do hc=1,np
        !print*, "hk d",hc, sum(grid%Phi_guess(:,:,:,:,:,:,:,:,hc,:,:,:,js+1,tc) ), grid%hk_grid(hc)
        !enddo
        
        !pause
   
  
        endif
            
            
            
        endif
        
            
           
    else    ! if jc>js(sc)    
       
        if (probmar(2)>0.0_dp .and. opt_cpl_aggr==1 .and. jc>=jf-1)then
            
            Phi_cpl = 0.0_dp
            
            ndlloc = ndl*ndl
            
            if (jc==jf-1)then
              
                xc2_max = nx
                dc2m1_max = nd
                dc1m1_max = nd
                nd_loc = nd
            else
              
                xc2_max = 1
                dc2m1_max = 1
                dc1m1_max = nd_cpl
                nd_loc=nd_cpl
            endif
            
            
            
                do sc2 = 1,ns
                    do sc1 = 1,ns
                        do epsc1m1 = 1,nw
                            do epsc2m1 = 1,nw 
                                do kc2=1,nk
                        
                                    do kc1 = 1,nk      
                                        do yc2m1 = 1,ny   ! parental h0 yesterday    
                                            do yc1m1 = 1,ny
                                                do ecm1=1,ne !ec1(jcm1,tcm1),ec2(jcm1,tcm1)  ! employment state yesterday 
                                                    do dcm1 =1,dc1m1_max !nd ! ec1(jcm1,tcm1),ec2(jcm1,tcm1)
                                                       do dc2m1=1,dc2m1_max
                                                
                                                
                                                    ! transition of home population:
                                                    do pc=1,np ! child acquired hc yesterday
                                                        
                                                        if (jc==jf)then
                                                            if (opt_corr==1)then
                                                                call basefun(grid%hk_grid,np,agg%h0distr(2,sc2),vals,inds)
                                                            else 
                                                                call basefun(grid%hk_grid,np,agg%h0distr(1,sc1),vals,inds)
                                                            endif
                                                            prob_ni = 0.0_dp
                                                            prob_ni(inds(1) ) = vals(1)
                                                            prob_ni(inds(2) ) = vals(2)
                                                            pc_cur_min = inds(1)
                                                            pc_cur_max = inds(2)
                                                        else
                                                            prob_ni = 0.0_dp
                                                            
                                                            pc_cur_min = 1
                                                            pc_cur_max = 1
                                                            prob_ni(1) = 1.0_dp
                                                        endif
                                                        
                                                        do pc_cur = pc_cur_min,pc_cur_max
                                                        
                                                            do xc1=1,nx  ! cash on hand yesterday
                                                                xc = xc1
                                                                do xc2 = 1,xc2_max
                                                                    
                                                                    xc3(1) = xc1
                                                                    xc3(2) = xc2
                                                                    yc3m1(1) = yc1m1
                                                                    yc3m1(2) = yc2m1
                                                                    kc3(1)= kc1
                                                                    kc3(2) = kc2
                                                                    epsc3m1(1) = epsc1m1
                                                                    epsc3m1(2) = epsc2m1
                                                                    sc3(1) = sc1
                                                                    sc3(2) = sc2
                                                                    
                                                                    ! look at fraction of population at this position yesterday:
                                                        
                                                                    if (jc==jf-1)then
                                                                        frac=grid%Phi(dcm1,xc1,pc,yc1m1,ecm1,kc1,1,epsc1m1,1,sc1,1,1,jcm1,tcm1)* grid%Phi(dc2m1,xc2,pc,yc2m1,ecm1,kc2,2,epsc2m1,1,sc2,1,1,jcm1,tcm1)
                                                                        frac = frac * 4.0_dp                
                                                                    else
                                                                        frac=grid%Phi_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,kc1,kc2,epsc1m1,epsc2m1,sc1,sc2,jcm1,tcm1)
                                                                    endif                                                           
 
                                                                                                         
                                                                    
                                                                    
                                                                    if ( frac==0.0 ) cycle   
                                                                 
                                                                    do yc1=1,ny ! today's income state
                                                                        do yc2=1,ny
                                                                            do epsc1=1,nw
                                                                                do epsc2=1,nw
                                               
                                                                                    if (ecm1==1)then ! in worker state yesterd
                                                        
                                                                                        if (jcm1>=jr(tcm1)-1 )then ! today >=jr
                                                                                            ec =2 ! => ret state today                                                            
                                                                                        else ! today <jr                                                           
                                                                                            if (dcm1<nd_loc)then ! didn't choose to retire yester
                                                                                                ec=1 ! => stay in worker state today
                                                                                            else ! chose to retire yester
                                                                                                ec=2 ! => move to retired state today
                                                                        
                                                                                            endif                                                            
                                                                                        endif    
                                                            
                                                                                    else ! in retired state yester => in ret state today (absorbing) 
                                                                                        ec=2
                                                                                    endif                                                    
                                                    
                                                                                    ! for each ec, determine set of discrete choices available
                                                                                    if (ec==1)then ! worker => discrete choices
                                                                                        if (opt_test_noer==1)then ! no end reet
                                                                                            allocate(dc_vec(ndl*ndl))
                                                                                            do dcl=1,ndl*ndl
                                                                                                dc_vec(dcl)=dcl
                                                                                            enddo
                                                            
                                                                                        elseif ( opt_test_noer==0)then !yes end ret
                                                                
                                                                                            if (jc>=jer .and. jc<jr(tc))then  !ret window
                                                                
                                                                                                allocate(dc_vec(ndl*ndl+1))
                                                                                                do dcl=1,ndl*ndl
                                                                                                    dc_vec(dcl)=dcl
                                                                                                enddo
                                                                                                dc_vec(ndl*ndl+1)=nd_cpl ! ret
                                                                    
                                                                                            else ! out of it
                                                                    
                                                                                                allocate(dc_vec(ndl*ndl))
                                                                                                do dcl=1,ndl*ndl
                                                                                                    dc_vec(dcl)=dcl
                                                                                                enddo
                                                                    
                                                                                            endif                                                             
                                                       
                                                                                        endif     
                                                                                    else ! ec=2
                                                                                        allocate(dc_vec(1))
                                                        
                                                                                        if (opt_test_noer)then
                                                            
                                                                                            dc_vec(1)=nd_cpl
                                                            
                                                                                        else
                                                                
                                                                                            if (jcm1>=jr(tcm1)-1 )then 
                                                                                                if (jcm1==jr(tcm1)-1)then
                                                                                                    if (ecm1==1)then ! retired after max age possible => late  reward
                                                                                                        dc_vec(1)=nd_cpl-2
                                                                                                    else
                                                                                                        dc_vec(1)=dcm1
                                                                                                    endif
                                                                                                else
                                                                                                    dc_vec(1)=dcm1
                                                                                                endif
                                                                        
                                                                                            else
                                                                                                if (jcm1>=jer .and. jcm1<jrr)then ! early ret window
                                                                                                    dc_vec(1)=nd_cpl-1
                                                                                                elseif (jcm1>=jrr)then ! reg
                                                                                                    if (ecm1==1)then
                                                                                                        dc_vec(1)=nd_cpl
                                                                                                    else
                                                                                                        dc_vec(1)=dcm1
                                                                                                    endif                                                                            
                                                                    
                                                                                                endif                                                                                                                       
                                                            
                                                                                            endif
                                                            
                                                                                        endif
                                                                                    endif                                                                
                                                            
                                                                                    ! when entering loop, always reset fraction
                                                                   
                                                                                    if (jc==jf-1)then
                                                                                        if (sc1==sc2)then
                                                                                            perfsort_flg=1.0_dp
                                                                                        else
                                                                                            perfsort_flg=0.0_dp
                                                                                        endif
                                                                                        
                                                                                        !frac = grid%Phi2guess_s(dcm1,xc1,yc1m1,kc1,1,epsc1m1,sc1,tcm1) * grid%Phi2guess_s(dc2m1,xc2,yc2m1,kc2,2,epsc2m1,sc2,tcm1)* grid%Phi_s(sc1,1,tcm1) * grid%Phi_s(sc2,2,tcm1) !  & 
                                                                                        !   ! *( (1.0_dp -sort_param) * grid%Phi_s(sc1,1,tcm1) * grid%Phi_s(sc2,2,tcm1) + sort_param * perfsort_flg ) 
                                                                                        !!frac=grid%Phi(dcm1,xc1,pc,yc1m1,ecm1,kc1,1,epsc1m1,1,sc1,1,1,jcm1,tcm1)* grid%Phi(dc2m1,xc2,pc,yc2m1,ecm1,kc2,2,epsc2m1,1,sc2,1,1,jcm1,tcm1)
                                                                                        !!frac = frac * ns * ns
                                                                                        !    
                                                                                        !frac =  grid%Phi2guess_s(dcm1,xc1,yc1m1,kc1,1,epsc1m1,sc1,tcm1) * grid%Phi2guess_s(dc2m1,xc2,yc2m1,kc2,2,epsc2m1,sc2,tcm1)* grid%Phi_s(sc1,1,tc) * &
                                                                                        !( (1.0_dp - sort_param) * grid%Phi_s(sc2,2,tc)    + sort_param * perfsort_flg )  
                                                                                        !    
                                                                                        !!frac = frac * 0.5_dp
                                       
                                                                                        tmp_distr = (grid%Phi_s(sc2,2,tc)- sort_param * grid%Phi_s(sc2,1,tcm1)  ) /  (sum(grid%Phi_s(:,2,tcm1))- sort_param * sum(grid%Phi_s(:,1,tcm1))  )
                                                                                        sort_param_tmp = sort_param
                                                                                        if (tmp_distr<=0.0_dp) sort_param_tmp = 1.0_dp
                                                                                        frac =grid%Phi(dcm1,xc1,pc,yc1m1,ecm1,kc1,1,epsc1m1,1,sc1,1,1,jcm1,tcm1)* grid%Phi(dc2m1,xc2,pc,yc2m1,ecm1,kc2,2,epsc2m1,1,sc2,1,1,jcm1,tcm1)/grid%Phi_s(sc2,2,tcm1) * ( (1.0_dp - sort_param) * tmp_distr  + sort_param * perfsort_flg )
                                                                                        frac = frac * 4.0_dp
                                                                                    else
                                                                                        frac=grid%Phi_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,kc1,kc2,epsc1m1,epsc2m1,sc1,sc2,jcm1,tcm1)
                                                                                    endif 
                                                                     
                                                                                    nd_glob = size(dc_vec)
                                                                                    allocate(v_temp(nd_glob),prob(nd_glob) )
                                                                 
                                                                
                                                                                    if (jc>jf .and. jc<=jt)then
                                                                    
                                                                                        call sub_prob_dc_new_out_2dim_cpl(agg,demo,grid,pol,grid%sav_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1),gr_wage_cpl,net_wage_cpl,wage_totax_cpl,gr_ben_p_cpl,ben_p_cpl, &
                                                                                            dcm1,ecm1,yc1m1,yc2m1,epsc1m1,epsc2m1,ec, &
                                                                                            kc1,kc2,yc1,yc2,epsc1,epsc2,sc1,sc2,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,vals2dim,inds2dim,valsh,indsh,v_temp,prob,prob_dc,v_dc,min_pens,nd_glob,ndlloc, & 
                                                                                            xc,pc)
                                                                          
                                                                                    else 
                                                                                        if (jc==jf-1)then
                                                                                            !if (gc==1)then
                                                                                                savg = grid%sav(dcm1,xc1,pc,yc1m1,ecm1,epsc1m1,1,kc1,1,sc1,1,1,jcm1,tcm1)
                                                                                                savp = grid%sav(dc2m1,xc2,pc,yc2m1,ecm1,epsc2m1,2,kc2,1,sc2,1,1,jcm1,tcm1)
                                                                                            !else
                                                                                            !    savg = grid%sav(dcm1,xc2,pc,yc2m1,ecm1,epsc2m1,gc,kc2,1,sc2,1,1,jcm1,tcm1)
                                                                                            !    savp = grid%sav(dcm1,xc1,pc,yc1m1,ecm1,epsc1m1,1,kc1,1,sc1,1,1,jcm1,tcm1)
                                                                                            !endif
                                                                                            
                                                                                            call sub_prob_dc_new_out_cpl(agg,demo,grid,pol,savg + savp,gr_wage_cpl,net_wage_cpl,wage_totax_cpl,gr_ben_p_cpl,ben_p_cpl,ec, & 
                                                                                                kc1,kc2,yc1,yc2,epsc1,epsc2,sc1,sc2,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,logsum,prob,prob_dc,v_dc,min_pens,nd_glob,ndl*ndl,pc_cur)
                                                                                        else
                                                                                            call sub_prob_dc_new_out_cpl(agg,demo,grid,pol,grid%sav_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1),gr_wage_cpl,net_wage_cpl,wage_totax_cpl,gr_ben_p_cpl,ben_p_cpl,ec, & 
                                                                                                kc1,kc2,yc1,yc2,epsc1,epsc2,sc1,sc2,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,logsum,prob,prob_dc,v_dc,min_pens,nd_glob,ndl*ndl,pc_cur)
                                                                                        endif
                                                                                        
                                                                    
                                                                                    endif  
                                                                
                                                                    
                                                                        
                                                                                    if (abs(sum(prob_dc) - 1.0_dp ) >epsi)then
                                                                                        print*, "dc prob should sum up to one sng2",sum(prob_dc)
                                                                                        pause
                                                                                    endif
                                                            
                                                                                    do dc=1,nd_cpl
                                                                                        if (prob_dc(dc)==0.0_dp) cycle
                                                                
                                                                                        frac_d = frac  *  prob_dc(dc) 
                                                                            
                                                                                        if (frac_d>0.0_dp .and. isnan(v_dc(dc)))then
                                                                                            print*, "NaN value with positive weight?",jc,ec,dc
                                                                                        !    pause
                                                                                        endif
                                                                        
                                                                                        vals = valsd(dc,:)
                                                                                        inds = indsd(dc,:)
                                                                                        
                                                                                        if (abs(sum(vals) - 1.0_dp)  > epsi)then
                                                                                            print*, "bad intp weights",vals,dc,sc1,sc2,epsc1m1,epsc2m1,kc1,kc2,yc2m1,yc1m1,ecm1,dcm1,pc,savg,savp
                                                                                            pause
                                                                                        endif
                                                                                        
                                                                        
                                                                                        lab = lab_dc(dc)
                                                                                        if (jc==jf-1)then
                                                                           
                                                                                                coh=((grid%sav(dcm1,xc1,pc,yc1m1,ecm1,epsc1m1,1,kc1,1,sc1,1,1,jcm1,tcm1) + &
                                                                                                    grid%sav(dc2m1,xc2,pc,yc2m1,ecm1,epsc2m1,2,kc2,1,sc2,1,1,jcm1,tcm1))/(1.0_dp+demo%lamt(tcm1)) &
                                                                                                    +agg%tr_sj(sc1,jc,tc)+agg%tr_sj(sc2,jc,tc))*f_aftertaxR(R,agg%tau_k(tc))
                                                                           
                                                                            
                                                                                        else
                                                                                            coh=(grid%sav_cpl(dcm1,xc1,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1)/(1.0_dp+demo%lamt(tcm1)) &
                                                                                                +agg%tr_sj(sc1,jc,tc)+agg%tr_sj(sc2,jc,tc))*f_aftertaxR(R,agg%tau_k(tc))
                                                                                        endif
                                                                            
                                                                                
                                                                                        ! out of those, look at those with specific income shock:
                                                                                        if (ec<ne)then
                                                                                            frac_d=frac_d * demo%prob_epsi(epsc1)* demo%prob_epsi(epsc2)
                                                                                            if (jc==jf-1)then
                                                                                                frac_d = frac_d *demo%pini(yc1)*demo%pini(yc2)
                                                                                            else
                                                                                                frac_d = frac_d *demo%prob_y(yc1m1,yc1)*demo%prob_y(yc2m1,yc2)
                                                                                            endif
                                                                            
                                                                                        else
                                                                                            frac_d = frac_d*prob_tr_ret(yc1m1,yc1)*prob_tr_ret(yc2m1,yc2)* demo%prob_epsi(epsc1)* demo%prob_epsi(epsc2)
                                                                                        endif  
                                                                        
                                                                                        if (jc==jf) frac_d = frac_d * prob_ni(pc_cur)
                                                                    
                                                                                        if (jc>jf .and. jc<=jt)then
                                                                        
                                                                                            !if (jc==jf) frac_d = frac_d * (ni)**(-1.0_dp) !* (2)**(-1.0_dp)
                                                                        
                                                                        
                                                                                                call sub_compudistr_out_2dim_cpl(Phi_cpl,Phi_kid_cpl,Phi_kid_old_cpl,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,(probmar_s(2,1,sc1)+ probmar_s(2,2,sc2) )*0.5_dp, & 
                                                                                                    inds2dim(dc,:,:),vals2dim(dc,:,:),indsh(dc,:),valsh(dc,:),gr_wage_cpl,net_wage_cpl,wage_totax_cpl,gr_ben_p_cpl,ben_p_cpl,leis,lab,ec,dc,yc1,yc2,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc,tcp1,tcm1,jmax,ndl,pc_cur,uc,vc,nc)
                                                                                                
                                                                        
                                                                        
                                                                                        else
                                                                      
                                                                                            call sub_compudistr_out_cpl(Phi_cpl,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,(probmar_s(2,1,sc1)+ probmar_s(2,2,sc2) )*0.5_dp, & 
                                                                                                inds,vals,gr_wage_cpl,net_wage_cpl,wage_totax_cpl,gr_ben_p_cpl,ben_p_cpl,leis,lab,ec,dc,yc1,yc2,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc,tcp1,jmax,ndl,pc_cur,opt_kid,uc,vc,nc)
                                                                                            !print*, sum(Phi_cpl)
                                                                                        endif
                                                                    
                                                                            
                                                                    
                                                                                    enddo
                                                          
                                                                                    deallocate(prob,v_temp,dc_vec)
                                                                    
                                                                                enddo
                                    
                                                                            end do  ! end do yc
                                                
                                                                        end do ! end do ic
                                                   
                                                                    end do  ! end do xc
                                                        
                                                                enddo
                                                            end do  ! end do pc
                                                        enddo

                                                    end do  ! end do ycm1
                                                enddo
                                               enddo
                                            enddo
                                          
                                        enddo
                                    end do  ! end do ecm1
                                enddo
                            end do  ! end do icm1
                        enddo
                    enddo
                
                enddo ! enddo gc
            enddo
       
            !!$OMP END DO 
            !!$OMP END PARALLEL
            
        grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = Phi_cpl
        if (abs(sum(Phi_cpl) - 1.0_dp ) > epsi)then
            print*, "bad Phi cpl", sum(Phi_cpl), jc,tc
            pause
            grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)/sum(Phi_cpl)
       
        else
            grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)/sum(Phi_cpl)
        endif
        
        !print*, "sum cpl", sum(Phi_cpl),jc, sum(grid%Phi(:,:,:,:,:,:,1,:,:,:,:,:,jcm1,tcm1) ), sum(grid%Phi(:,:,:,:,:,:,2,:,:,:,:,:,jcm1,tcm1) )
        !pause    
    endif
        
        
         
        Phi = 0.0_dp
        
        if (gen_id ==1)then
            
            
            
            !if (jc==jt)then
            !    Phi_kid = 0.0_dp
            !    Phi_kid_old = 0.0_dp
            !endif
            
            if (jc==jt)then 
                uc=0 
                grid%phi_edu_long = 0.0_dp
                vc=0 
                grid%phi_earn_long = 0.0_dp
              
            endif
        
        
            ndlloc = ndl
            
            do xc_dec = 1,1
                
            do scpar = 1,ns
           
                do ickidm1=1,1 !ni !ickid_max: CURRENTLY this dimension is irrelevant, because for all ages operate on hk_grid, thus, pc dimension
                    
                    if (gen_id==1 .and. jc>=jf .and. jc<=jt)then
                        ickid= ickidm1
                    else
                        ickid= 1
                    endif
                                        
                    do hcm1=1,np ! own hc yesterd
                        
                        hc=1 !hcm1 ! own hc today, this is aggregation for parents only so far, thus, no loop over own HK needed
                   
                        do gc=1,ng        
                            do icm1=1,ni  ! parental h0 yesterday    
                                do sc=1,ns  ! skill level
                                    
                                    
                                
                                    do kc=1,nk  ! fixed effect 
                                        if (sc==ns .and. jcm1==js)then
                                            kcp1_min = 1
                                            kcp1_max = nk
                                            if (nk>1)then
                                                probfe(nk) = f_probfe(grid%hk_grid(hcm1),sc,agg%gammah_s(sc,tc))
                                                probfe(1) = 1.0_dp -probfe(nk) 
                                            else
                                                probfe(1) = 1.0_dp
                                            endif
                                        else
                                            kcp1_min = kc
                                            kcp1_max = kc
                                            probfe = 0.0_dp
                                            probfe(kc) = 1.0_dp
                                        endif
                                        
                                        
                                        do kcp1=kcp1_min,kcp1_max
                                        
                                        
                                        do ecm1=1,ne !ec1(jcm1,tcm1),ec2(jcm1,tcm1)  ! employment state yesterday 
                                            do dcm1 =1,nd ! ec1(jcm1,tcm1),ec2(jcm1,tcm1)
                                                
                                                do ycm1=1,ny  ! income states yesterday 
                                                    do epscm1=1,nw
                                
                                                    ! transition of home population:
                                                    do pc=1,np ! child acquired hc yesterday
                                                        
                                                        if (gc==2 .and. jc==jf)then
                                                            if (opt_corr==1)then 
                                                                call basefun(grid%hk_grid,np,agg%h0distr(1,sc),vals,inds)
                                                            else
                                                                call basefun(grid%hk_grid,np,agg%h0distr(gc,sc),vals,inds)
                                                            endif
                                                            prob_ni = 0.0_dp
                                                            prob_ni(inds(1) ) = vals(1)
                                                            prob_ni(inds(2) ) = vals(2)
                                                            pc_cur_min = inds(1)
                                                            pc_cur_max = inds(2)
                                                        else
                                                            prob_ni = 0.0_dp
                                                            
                                                            pc_cur_min = 1
                                                            pc_cur_max = 1
                                                            prob_ni(1) = 1.0_dp
                                                        endif
                                                        
                                                        do pc_cur = pc_cur_min,pc_cur_max
                                                        
                                                        do xc=1,nx  ! cash on hand yesterday
                                                            ! look at fraction of population at this position yesterday:
                                                        
                                                            if (opt_intp==0)then
                                                                frac=grid%Phi(dcm1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)
                                                            else
                                                                frac=grid%Phi_int(dcm1,xc,pc,ycm1,ecm1,icm1,gc,kc,hcm1,sc,ickidm1,1,jcm1,tcm1)
                                                            endif                                                            
                                                                                                                          
                                                            if ( frac==0.0 ) cycle   
                                                            
                                                            !if (jc==js+1)then
                                                            !    if (pc > hcm1 .or. pc < hcm1)then
                                                            !        print*, "hcm1 and pc", hcm1,pc
                                                            !        pause
                                                            !    endif
                                                            !endif
                                                            
                                                                                        
                                                            do ic=icm1,icm1 
                                                                   
                                                                do yc=1,ny ! today's income state
                                                                    do epsc=1,nw
                                               
                                                                    if (ecm1==1)then ! in worker state yesterd
                                                        
                                                                        if (jcm1>=jr(tcm1)-1 )then ! today >=jr
                                                                            ec =2 ! => ret state today                                                            
                                                                        else ! today <jr                                                           
                                                                            if (dcm1<nd)then ! didn't choose to retire yester
                                                                                ec=1 ! => stay in worker state today
                                                                            else ! chose to retire yester
                                                                                ec=2 ! => move to retired state today
                                                                        
                                                                            endif                                                            
                                                                        endif    
                                                            
                                                                    else ! in retired state yester => in ret state today (absorbing) 
                                                                        ec=2
                                                                    endif                                                    
                                                    
                                                                    ! for each ec, determine set of discrete choices available
                                                                    if (ec==1)then ! worker => discrete choices
                                                                        if (opt_test_noer==1)then ! no end reet
                                                                            allocate(dc_vec(ndl))
                                                                            do dcl=1,ndl
                                                                                dc_vec(dcl)=dcl
                                                                            enddo
                                                            
                                                                        elseif ( opt_test_noer==0)then !yes end ret
                                                                
                                                                            if (jc>=jer .and. jc<jr(tc))then  !ret window
                                                                
                                                                                allocate(dc_vec(ndl+1))
                                                                                do dcl=1,ndl
                                                                                    dc_vec(dcl)=dcl
                                                                                enddo
                                                                                dc_vec(ndl+1)=nd ! ret
                                                                    
                                                                            else ! out of it
                                                                    
                                                                                allocate(dc_vec(ndl))
                                                                                do dcl=1,ndl
                                                                                    dc_vec(dcl)=dcl
                                                                                enddo
                                                                    
                                                                            endif                                                             
                                                       
                                                                        endif     
                                                                    else ! ec=2
                                                                        allocate(dc_vec(1))
                                                        
                                                                        if (opt_test_noer)then
                                                            
                                                                            dc_vec(1)=nd
                                                            
                                                                        else
                                                                
                                                                            if (jcm1>=jr(tcm1)-1 )then 
                                                                                if (jcm1==jr(tcm1)-1)then
                                                                                    if (ecm1==1)then ! retired after max age possible => late  reward
                                                                                        dc_vec(1)=nd-2
                                                                                    else
                                                                                        dc_vec(1)=dcm1
                                                                                    endif
                                                                                else
                                                                                    dc_vec(1)=dcm1
                                                                                endif
                                                                        
                                                                            else
                                                                                if (jcm1>=jer .and. jcm1<jrr)then ! early ret window
                                                                                    dc_vec(1)=nd-1
                                                                                elseif (jcm1>=jrr)then ! reg
                                                                                    if (ecm1==1)then
                                                                                        dc_vec(1)=nd
                                                                                    else
                                                                                        dc_vec(1)=dcm1
                                                                                    endif                                                                            
                                                                    
                                                                                endif                                                                                                                       
                                                            
                                                                            endif
                                                            
                                                                        endif
                                                                    endif                                                                
                                                            
                                                                    ! when entering loop, always reset fraction
                                                                  
                                                                    frac=grid%Phi(dcm1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)
                                                                    
                                                                     
                                                                    nd_glob = size(dc_vec)
                                                                    allocate(v_temp(nd_glob),prob(nd_glob) )
                                                                
                                                                
                                                                    if (gc==2 .and. jc>jf .and. jc<=jt)then
                                                                    
                                                                        call sub_prob_dc_new_out_2dim(agg,demo,grid,pol,grid%sav(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,ickid,scpar,jcm1,tcm1),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p, &
                                                                            dcm1,ecm1,ycm1,epscm1,icm1,ec, &
                                                                            kcp1,hc,yc,epsc,ic,sc,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,vals2dim,inds2dim,valsh,indsh,v_temp,prob,prob_dc,v_dc,min_pens,nd_glob,ndlloc, & 
                                                                            xc,pc,ickid,1,ickidm1,1)
                                                                          
                                                                    else 
                                                                    
                                                                        call sub_prob_dc_new_out(agg,demo,grid,pol,grid%sav(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,ickid,scpar,jcm1,tcm1),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,ec, & 
                                                                            kcp1,hc,yc,epsc,ic,sc,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,logsum,prob,prob_dc,v_dc,min_pens,nd_glob,ndl,pc_cur,ickid,1,gen_id)
                                                                       
                                                                    
                                                                    endif  
                                                                
                                                                    
                                                                        
                                                                    if (abs(sum(prob_dc) - 1.0_dp ) >epsi)then
                                                                        print*, "dc prob should sum up to one sng2",sum(prob_dc)
                                                                        pause
                                                                    endif
                                                            
                                                                    do dc=1,nd
                                                                        if (prob_dc(dc)==0.0_dp) cycle
                                                                
                                                                        frac_d = frac  *  prob_dc(dc) * probfe(kcp1)
                                                                            
                                                                        if (frac_d>0.0_dp .and. isnan(v_dc(dc)))then
                                                                            print*, "NaN value with positive weight?",jc,ec,dc
                                                                        !    pause
                                                                        endif
                                                                        
                                                                        vals = valsd(dc,:)
                                                                        inds = indsd(dc,:)
                                                                        
                                                                        lab = lab_dc(dc)
                                                                        
                                                                            coh=(grid%sav(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,ickid,scpar,jcm1,tcm1)/(1.0_dp+demo%lamt(tcm1)) &
                                                                                +agg%tr_sj(sc,jc,tc))*f_aftertaxR(R,agg%tau_k(tc))
                                                                      
                                                                            
                                                                                
                                                                        ! out of those, look at those with specific income shock:
                                                                        if (ec<ne)then
                                                                            frac_d=frac_d*demo%prob_y(ycm1,yc) * demo%prob_epsi(epsc)
                                                                        else
                                                                            frac_d = frac_d*prob_tr_ret(ycm1,yc)* demo%prob_epsi(epsc)
                                                                        endif  
                                                                        
                                                                        if (gc==2 .and. jc==jf) frac_d = frac_d * prob_ni(pc_cur)
                                                                    
                                                                        if (gc==2 .and. jc>jf .and. jc<=jt)then
                                                                        
                                                                            !if (jc==jf) frac_d = frac_d * (ni)**(-1.0_dp) !* (2)**(-1.0_dp)
                                                                        
                                                                        
                                                                            
                                                                            call sub_compudistr_out_2dim(Phi,Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d, & 
                                                                                inds2dim(dc,:,:),vals2dim(dc,:,:),indsh(dc,:),valsh(dc,:),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,leis,lab,ec,dc,yc,epsc,kc,hc,sc,ic,gc,jc,tc,tcp1,tcm1,jmax,ndl,pc_cur,ickid,1,uc,vc,nc,xc_dec)
                                                                            
                                                                           
                                                                        
                                                                        
                                                                        else
                                                                     
                                                                           
                                                                            call sub_compudistr_out(Phi,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d, & 
                                                                                inds,vals,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,leis,lab,ec,dc,yc,epsc,kcp1,hc,sc,ic,gc,jc,tc,tcp1,jmax,ndl,pc_cur,ickid,1,opt_kid,uc,vc,1,1,1,1,nc)
                                                                           
                                                                        endif
                                                                    
                                                                            
                                                                    
                                                                    enddo
                                                          
                                                                    deallocate(prob,v_temp,dc_vec)
                                                                    
                                                                    enddo
                                    
                                                                end do  ! end do yc
                                                
                                                            end do ! end do ic
                                                   
                                                        end do  ! end do xc
                                                        
                                                        enddo
                                                    end do  ! end do pc
                                                    enddo

                                                end do  ! end do ycm1
                                                
                                    
                                            enddo
                                        end do  ! end do ecm1
                                        enddo
                                    end do  ! end do icm1
                                enddo
                            enddo
                
                        enddo ! enddo gc
                    enddo
                enddo
           
            enddo
            enddo
            !!$OMP END DO 
            !!$OMP END PARALLEL
            if (opt_intp==0)then 
                grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = Phi
            else
                grid%Phi_int(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = Phi
            endif
            if (jc==jf-2)then
                do sc=1,ns
                    do gc=1,2
                        grid%Phi2guess_s(:,:,:,:,gc,:,sc,tc) = grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,tc) / sum(grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,tc))
                        grid%Phi_s(sc,gc,tc) = sum(grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,tc)) / sum(grid%Phi(:,:,1,:,1,:,gc,:,1,:,1,1,jf-2,tc)) !* 0.5_dp
                        ! tbc: grid%Phi_s(sc,gc,tc) = grid%Phi_s(sc,gc,tc) * probmar_s(sc,gc)
                    enddo
                enddo
                ! tbc: grid%Phi_s(:,:,tc) =grid%Phi_s(:,:,tc) / sum(grid%Phi_s(:,:,tc)) 
            endif
            
        endif
            
        
    endif
        
        
    
    
    !print*, "soo",sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)) ,tc,jc
        
    ! add ex post correction (numerical correction)
        
    if (jc>js .and. abs(sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)) - 1.0_dp) > epsi)then
        print*, "beep",sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)) ,tc,jc,jt, sum(Phi), sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jcm1,tcm1))
        pause
    else            
        ! grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) / sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc))
    endif
    
    
    if (t1>t0 .and. jc==jt)then
        
       ! tcm1_old = tcm1
        tcm1 = tc
            
            Phi = 0.0_dp
            Phi_kid = 0.0_dp
            Phi_kid_old = 0.0_dp
            jcm1 = jt
        
            
                ndlloc = ndl
            
                do mc = 1,2
                
                    if (mc==1)then
                        gc_max = ng
                        gc_min = ng
                        sc2_max = 1
                        kc2_max = 1
                        yc2m1_max = 1
                        epsc2m1_max = 1
                        hcm1_max = 1
                        !nd2 = ndl
                    
                    else
                        gc_max = 1
                        gc_min = 1
                        sc2_max = ns
                        kc2_max = nk
                        yc2m1_max = ny
                        epsc2m1_max = nw
                        hcm1_max = 1
                        !nd2 = ndl*ndl
                    endif
                
                    
            
                    do gc_kid = 1,ng
                        do xc_dec = 1,1
                
                            do scpar = 1,1
                                
                                    do ickidm1=1,1 !ni !ickid_max: CURRENTLY this dimension is irrelevant, because for all ages operate on hk_grid, thus, pc dimension
                    
                                        if (gen_id==1 .and. jc>=jf .and. jc<=jt)then
                                            ickid= ickidm1
                                        else
                                            ickid= 1
                                        endif
                                        
                                        do hcm1=1,hcm1_max ! own hc yesterd
                        
                                            hc=1 !hcm1 ! own hc today, this is aggregation for parents only so far, thus, no loop over own HK needed
                   
                                            do gc=gc_min,gc_max !ng        
                                                do icm1=1,ni  ! parental h0 yesterday    
                                                    do sc=1,ns  ! skill level
                                                        do sc2=1,sc2_max
                                 
                                                            do kc=1,nk  ! fixed effect 
                                                                do kc2 = 1,kc2_max
                                        
                                                                    do ecm1=1,ne !ec1(jcm1,tcm1),ec2(jcm1,tcm1)  ! employment state yesterday 
                                                                        do dcm1 =1,nd ! ec1(jcm1,tcm1),ec2(jcm1,tcm1)
                                                
                                                                            do ycm1=1,ny  ! income states yesterday 
                                                                                do yc2m1 = 1,yc2m1_max
                                                                                    do epscm1=1,nw
                                                                                        do epsc2m1 = 1,epsc2m1_max
                                
                                                                                            ! transition of home population:
                                                                                            do pc=1,np ! child acquired hc yesterday
                                                        
                                                                                                kcp1_min = 1
                                                                                                kcp1_max = nk
                                                                                                if (nk>1)then
                                                                                                    probfe(nk) = f_probfe(grid%hk_grid(pc),1,agg%gammah_s(1,tc))
                                                                                                    probfe(1) = 1.0_dp -probfe(nk) 
                                                                                                else
                                                                                                    probfe(1) = 1.0_dp
                                                                                                endif
                                       
                                                      
                                                                                                do pc_cur = 1,1
                                                        
                                                                                                    do xc=1,nx  ! cash on hand yesterday
                                                                               
                                                                                                        ! look at fraction of population at this position yesterday:
                                                                                                        if (mc==1)then
                                                                                                            frac=grid%Phi(dcm1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)/0.5_dp
                                                                                                        else
                                                                                                            frac=grid%Phi_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,kc,kc2,epscm1,epsc2m1,sc,sc2,jcm1,tcm1)
                                                                                                        endif                                      
                                                                                                                          
                                                                                                        if ( frac==0.0 ) cycle   
                                                                                        
                                                                                                        do ic=icm1,icm1 
                                                              
                                                                                                            ec = 1                                      
                                                    
                                                                                                            ! for each ec, determine set of discrete choices available
                                                                   
                                                                       
                                                                                                            allocate(dc_vec(ndl))
                                                                                                            do dcl=1,ndl
                                                                                                                dc_vec(dcl)=dcl
                                                                                                            enddo
                                                                                                            nd_glob = size(dc_vec)
                                                                                                            allocate(v_temp(nd_glob),prob(nd_glob) )
                                                                                    
                                                                                                            ! when entering loop, always reset fraction
                                                                   
                                                                                                            if (mc==1)then
                                                                                                                frac=grid%Phi(dcm1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)/0.5_dp
                                                                                                            else
                                                                                                                frac=grid%Phi_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,kc,kc2,epscm1,epsc2m1,sc,sc2,jcm1,tcm1)
                                                                                                            endif  
                                                                     
                                                                                                            prob_sc_kid = 0.0_dp
                                                                    
                                                                                                            ! dc probabilities
                                                                                                            do dcl=1,ndl
                                                                                                                dc_vec(dcl)=dcl
                                                                                                            enddo
                                                                                                            ! distribute this fraction according to cash-on-hand
                                                                                                            ! wealth component of cash on hand, income component added below
                                                                                                            Rafter = f_aftertaxR(agg%ret(tc) + 1.0_dp,agg%tau_k(tc))
                                                                                                            if (mc==1)then
                                                                                        
                                                                                                                bnet =pol%b(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,sc,hcm1,scpar,tcm1) ! (nd,nx,np,ny,ne,nw,ng,nk,ns,np,ns,nt))
                                                                                                                cons = pol%cons(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hcm1,sc,1,scpar,jt,tcm1) ! (nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
                                                                                                            else
                                                                                                                bnet =pol%b_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,tcm1)
                                                                                                                cons = pol%cons_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,jt,tcm1)
                                                                                                            endif
                                                                                    
                                                                                                            ass = bnet/Rafter   !+ agg%tr_isj(ic,sc,jc,tc)  *f_aftertaxR(R,agg%tau_k(tc))
           
                                                                                                            if (ass<grid%amin_age(gc_kid,3,2,js) ) then
               
                                                                                                                nsloc = ns-1
                                                                                                            else
                                                                                                                nsloc = ns 
                                                                                                            endif
                                                                
                                                                                                            if (mc==1)then
                                                                        
                                                                                                                call sub_prob_dc_new_kid(agg,demo,grid,pol,pol%b(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,sc,hcm1,scpar,tcm1),gr_wage_kid,net_wage_kid,wage_totax_kid, & 
                                                                                                                    gr_ben_p_kid,ben_p_kid,ec, &
                                                                                                                    pc,1,gc_kid,js,tc,js,tcm1,dc_vec,lab_dc_s,valsd_s,indsd_s,v_temp,logsum,prob,prob_dc_s,prob_sc,v_dc_s,min_pens,ndl,ndl,nsloc,sc,Rafter)
                                                                                                            else
                                                                                                                call sub_prob_dc_new_kid(agg,demo,grid,pol,pol%b_cpl(dcm1,xc,pc,ycm1,yc2m1,ecm1,epscm1,epsc2m1,kc,kc2,sc,sc2,tcm1),gr_wage_kid,net_wage_kid,wage_totax_kid, & 
                                                                                                                    gr_ben_p_kid,ben_p_kid,ec, &
                                                                                                                    pc,1,gc_kid,js,tc,js,tcm1,dc_vec,lab_dc_s,valsd_s,indsd_s,v_temp,logsum,prob,prob_dc_s,prob_sc,v_dc_s,min_pens,ndl,ndl,nsloc,max(sc,sc2),Rafter)
                   
                                                                                                            endif
                                                                                    
                                                                   
                                                                        
                                                        
                                                                                                            prob_ni = 0.0_dp
                                                            
                                                                                                            prob_ni(1) = 1.0_dp
                                                            
                                                            
                                                                                                            do sc_kid = 1,nsloc
                                                                                                                
                                                                                                                if (nk>1)then
                                                                                                    probfe(nk) = f_probfe(grid%hk_grid(pc),1,agg%gammah_s(1,tc))
                                                                                                    probfe(1) = 1.0_dp -probfe(nk) 
                                                                                                else
                                                                                                    probfe(1) = 1.0_dp
                                                                                                endif
               ! print*, "prob fe",pc, probfe(nk),sc_kid
                                                                                                                do kc_kid = 1,nk
                
                                                                                                                    do yc_kid = 1,ny
                                                                                                                        do epsc_kid = 1,nw
                                                                                                                            
                                                                                                                            
                                   
                                                                                                                            do dckid=1,nd
                                                                                                                                
                                                                                                                                ! THEY ALL WORK AT NO WAGES
                                                                                                                                if (sc_kid<3)then
                                                                                                                                    call sub_wage(gr_wage_kid,net_wage_kid,wage_totax_kid,agg%wage_s(sc_kid,tc),demo%ageprod(gc_kid,js,sc_kid),demo%grid_y(yc_kid),grid%hk_grid(pc), & 
                                                                                                                                        agg%tau_p(tc),sc_kid,kc_kid,dckid,1,js,1,agg%gammah_s(sc_kid,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                                                                                                                                    
                                                                                                                                else
                                                                                                                                    call sub_wage(gr_wage_kid,net_wage_kid,wage_totax_kid,agg%wage_s(1,tc),demo%ageprod(gc_kid,js,1),demo%grid_y(yc_kid),grid%hk_grid(pc), & 
                                                                                                                                        agg%tau_p(tc),1,kc_kid,dckid,1,js,1,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                                                                                                                                endif
                                                                                                                                
                                                                                                                                !gr_wage = gammafe(sc_kid,kc_kid)
                                    
                                                                                                                                if (prob_dc_s(sc_kid,kc_kid,yc_kid,epsc_kid,dckid)==0.0) cycle
                                        
                                                                                                                                vals = valsd_s(sc_kid,dckid,kc_kid,yc_kid,epsc_kid,:)
                                                                                                                                inds = indsd_s(sc_kid,dckid,kc_kid,yc_kid,epsc_kid,:)
                                                                                                                                leis = leis_dc_s(sc_kid,dckid)
                                                                                                                                lab = lab_dc_s(sc_kid,dckid)
                                          
                                                                                                                                frac_d = frac  *  prob_dc_s(sc_kid,kc_kid,yc_kid,epsc_kid,dckid)  * prob_sc(sc_kid) *probfe(kc_kid)  * demo%pini(yc_kid) * demo%prob_epsi(epsc_kid) * ng**(-1.0_dp) !* probmar_s(mc,gcpar,scpar)
                                                                                                                                if (mc==1)then
                                                                                                                                    frac_d = frac_d * probmar_s(mc,gc,sc)
                                                                                                                                else
                                                                                                                                    frac_d = frac_d * ( probmar_s(mc,1,sc) + probmar_s(mc,2,sc2)) * 0.5_dp
                                                                                                                                endif
                                                                                                                                
                                                                                                                                    
                                                                                                                                prob_sc_kid(sc_kid) = prob_sc_kid(sc_kid) + frac_d                    
                                                               
                                                                                                                                if (frac_d>0.0_dp .and. isnan(v_dc_s(sc_kid,dckid)))then
                                                                                                                                    print*, "NaN value with positive weight?",js,ec_kid,dckid
                                                                                                                                    !pause
                                                                                                                                endif
                                                                                                                                
                                                                                                                                !agg%cons_sj(sc_kid,jt,tc) = agg%cons_sj(sc_kid,jt,tc) + frac_d * cons
                                                                                                                                agg%ivt_skid(sc_kid,tc) = agg%ivt_skid(sc_kid,tc)  + frac_d *  bnet
                                                                                                                                agg%poppar_skidj(sc_kid,jt,tc) = agg%poppar_skidj(sc_kid,jt,tc) + frac_d
                                                                                                                                
                  
                                                                                                                                if (mc==1)then
                                                                                                                                    call sub_compudistr_out_kid(Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,inds,vals, &
                                                                                                                                        gr_wage_kid,net_wage_kid,wage_totax_kid,gr_ben_p_kid,ben_p_kid,leis,lab,ec,dckid,yc_kid,epsc_kid,kc_kid,pc,sc_kid,ickid,gc_kid,js,tc,tcp1,js,ndl,1,1,1,1,uc,vc,sc,sc,mc,gc,xc_dec,ycm1,epscm1,kc,nc)
                                                                                                                                else
                                                                                                                                    call sub_compudistr_out_kid(Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac_d,inds,vals, &
                                                                                                                                        gr_wage_kid,net_wage_kid,wage_totax_kid,gr_ben_p_kid,ben_p_kid,leis,lab,ec,dckid,yc_kid,epsc_kid,kc_kid,pc,sc_kid,ickid,gc_kid,js,tc,tcp1,js,ndl,1,1,1,1,uc,vc,sc,sc2,mc,2,xc_dec,ycm1,epscm1,kc,nc)
                                                                                                                                endif
                                                                                                        
                                       
                                                                                                                            enddo
                            
                                                                                                                        enddo
                                                                                                                    enddo
                
                                                                            
                                                                                                                enddo
                                                                                                            enddo
                                                                                                            deallocate(dc_vec,v_temp,prob)
                                                                                                        end do  ! end do yc
                                                
                                                                                                    end do ! end do ic
                                                   
                                                                                                end do  ! end do xc
                                                        
                                                                                            enddo
                                                                                        end do  ! end do pc
                                                                                    enddo

                                                                                end do  ! end do ycm1
                                                
                                                                            enddo
                                                                        end do  ! end do ecm1
                                                                    enddo
                                                                end do  ! end do icm1
                                                            enddo
                                                        enddo
                
                                                    enddo ! enddo gc
                                                enddo
                                            enddo
                                        enddo
                                    enddo
                                
                            enddo
                        enddo
                    enddo

                    if (mc==1)then
                        grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,mc,tc) = Phi_kid
                        print*, "sumheremc1", sum(Phi_kid)
                    else
                        grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,mc,tc) = Phi_kid - grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,1,tc)
                        print*, "sumheremc2",sum(grid%Phi_childja(:,:,:,:,:,:,:,:,:,:,:,:,mc,tc)), sum(Phi_kid)
                    endif


                enddo
            
           
        
           
                !!$OMP END DO 
                !!$OMP END PARALLEL
           
            !if (probmar(2)>0.0_dp .and. opt_cpl_aggr==1)then
            !    grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = Phi_kid +   Phi_kid_cpl
            !
            !    grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)=  Phi_kid +   Phi_kid_cpl
            !else
                !grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = Phi_kid !/probmar(1)
                grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)=Phi_kid !/probmar(1)
            
                grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
            !endif
        
            
            if (abs(sum(grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)) - 1.0_dp) > epsi)then
                print*, "beep kid update",sum(grid%Phi_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)) ,tc,sum(Phi_kid),sum(Phi_kid_cpl) 
                pause
            else ! correct round off            
                grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)/sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc))
                
                print*,"kids educ shares", tc, sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,3,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,2,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                print*, " gr wage", tc, agg%grwage(tc)
                !  pause
                
             !   grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) / sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc))
            
                agg%lhsfrac(tc) = agg%pop_kid_s(1,tc) / sum(agg%pop_kid_s(:,tc)) !sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                agg%clfrac(tc) =agg%pop_kid_s(ns,tc) / sum(agg%pop_kid_s(:,tc)) ! sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,3,:,:,js,tc))
                agg%dropfrac(tc) =agg%pop_kid_s(ns-1,tc) / sum(agg%pop_kid_s(:,tc)) 
            
            
            
                !print*, "now", agg%hsfrac_str(tc) 
            
                endif
       
        !if (tc>2 .and. tc<nt)then ! transition
        !    grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi_guess(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
        !elseif (tc==2)then
        !    grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,js,1)
        !endif
        
            print*, "avearn3040", avearn3040,agg%h_j(jt+1,tc)
            !pause

            !print*, "constrr frac", agg%indbc_j(:,tc)
            !pause
            open(unit=116,file='output/fracnegass.txt')
            do jcloc=jf,nj
               write(116,'(4f50.16)') agg%indbc_j(jcloc,tc) 
            enddo
            !pause
            
  !  endif
    
        
        open(unit=16,file='output/polfun.txt')
        
        do jcloc=jf,jt-1
            do sc=1,ns
                do gc=1,ng
                    do pc=1,np
                        do xc=1,nx
                            write(16,'(4f50.16)') grid%coh(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),pol%m(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),pol%t1(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),grid%hk_grid(pc)    
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        close (16)
        
        
        open(unit=236,file='output/polfunb.txt')
        
        do jcloc=jt,jt
            do sc=1,ns
                do gc=1,ng
                    do pc=1,np
                        do xc=1,nx
                            write(236,'(2f50.16)') grid%coh(1,xc,pc,1,1,1,gc,1,1,sc,1,1,jcloc,tc),pol%b(1,xc,pc,1,1,1,gc,1,sc,1,1,tc)    
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        close (236)
        
        
        
        
        open(unit=17,file='output/vchild.txt')
        
        do jcloc=js,js
            do sc=1,ns
                do gc=1,ng
                    do pc=1,np
                        do xc=1,nx
                            write(17,'(2f50.16)') grid%coh_child(1,xc,1,1,1,1,gc,1,pc,sc,1,1,jcloc,tc),pol%v_child(1,xc,1,1,1,1,gc,1,pc,sc,1,1,jcloc,tc) 
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        close (17)    
        
       ! tcm1 = tcm1_old
        
        
    endif
    
    
    
            
    enddo ! enddo jc
    
    if (jmax==nj)then
    
   
        agg%cap(tcp1) = agg_vec(1)
        
        !print*, "capital", agg%cap(tc), agg_vec(1)
        !pause
        
        agg%bq(tcp1) =agg_vec(2)
        agg%hrs(tc) = agg_vec(3)
        agg%cons(tc) = agg_vec(4)
        agg%grsav(tc) = agg_vec(5)
        agg%wage_inc(tc) = agg_vec(6)
        agg%net_wage_inc(tc) = agg_vec(7)
        agg%tot_inc(tc) =  agg_vec(8)
        agg%disp_inc(tc) = agg_vec(9)
       ! agg%labinctaxrv(tc) = agg_vec(10) 
        agg%capinctaxrv(tc) = agg_vec(11)
        agg%constaxrv(tc) = agg_vec(12)
        agg%negass(tc) = agg_vec(13)
        agg%wage_inc_hh(tc) = agg_vec(14)
        agg%tot_inc_hh(tc) = agg_vec(15)
        agg%ben_u(tc) = agg_vec(16)
        agg%ben_p(tc) = agg_vec(17)
        agg%pens(tc) = agg_vec(18)
        agg%hrs1(tc)= agg_vec(19)
        agg%hrs2(tc)= agg_vec(20)
        agg%earn(tc)=agg_vec(21) ! total labor earnings
        agg%penstaxrv(tc) = agg_vec(24)
        
        agg%emppop(tc) = agg_vec(29)
        agg%allpopstock(tc) = agg_vec(30)
        
        
        agg%earn_ft(tc) = agg_vec(25)  
        agg%hrs_ft(tc)= agg_vec(26)
        agg%hrs1_calib(tc) = agg_vec(27)
        agg%hrs2_calib(tc) = agg_vec(28) 
       
        agg%frishela(:,tc) = agg_str_g(3,:)
        agg%pop_frisch(:,tc) = agg_str_g(4,:)
       
        
        agg%tot_inc(tc) =  agg_vec(8)
        agg%labinctaxrv(tc) = agg_vec(10) 
        agg%capinctaxrv(tc) = agg_vec(11)
        agg%constaxrv(tc) = agg_vec(12) 
        agg%penstaxrv(tc) = agg_vec(24)
        
        ! total revenues
        agg%totrv(tc) = agg%labinctaxrv(tc) + agg%capinctaxrv(tc) + agg%constaxrv(tc) +  agg%penstaxrv(tc)
        
      !  if (opt_cpl) call sub_meas_aggr_cpl(stat,agg,demo,grid,lc,pol,tc,tcp1,tcm1,jmax,opt_upd,opt_rfg)
        
        ! check distribution
        do jc=jmin,nj
            
            sum_frac=sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc))
     
            !if (opt_cpl) sum_frac = sum_frac + sum(grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc))
        
            !print*, "sumfrac", jc,sum_frac
            !pause
        
            dist_frac=abs(sum_frac-1.0_dp)
            if (dist_frac>epsi) then
            !if (dist_frac>0.01_dp)then
                print*, 'error in updating measures at age ', jc, ' time ', tc, sum_frac,sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc))
               
            endif
       
        end do
        
        
        do sc=1,ns
            agg%meanwage_s(sc,tc) =agg%meanwage_s(sc,tc)/ sum(grid%Phi(:,:,:,:,:,:,:,:,:,sc,:,:,jt,tc))
            agg%slopewage_s(sc,tc) =agg%slopewage_s(sc,tc)/ sum(grid%Phi(:,:,:,:,:,:,:,:,:,sc,:,:,jt,tc))
        enddo
        !   pause
      
        ! compute average annual per child investments, as a fraction of annual earnings
        ! money
        
        agg%m_j(jf:jt,tc) = agg%m_j(jf:jt,tc) / (earn_si_lo*agg%poppar_j(jf:jt+1,tc))
        agg%t_j(jf:jt,tc) = agg%t_j(jf:jt,tc) / (ft_lab*agg%poppar_j(jf:jt+1,tc))
        agg%h_j(jf:jt+1,tc) = agg%h_j(jf:jt+1,tc) / (agg%poppar_j(jf:jt+1,tc))
        
        agg%moninv(tc) =  agg%moninv(tc)  / (earn_si_lo   *agg%poppar_inv(tc) )
        agg%tinv(tc) =  agg%tinv(tc)  / (ft_lab   *agg%poppar_inv(tc) ) 
        agg%moninvratio(tc) = agg%moninvratio(tc) / agg%poppar_inv(tc)
        
       ! agg%mon
        
        if (opt_mean_update) agg%inv_mean(tc) = sum(agg%inv_j(jf:jt-1,tc))/(jt-jf)
        
        print*, "inv mean", agg%inv_mean(tc)
        
        agg%emprate(tc) = agg%emprate(tc)/(jr(1)-1 - js)
        print*, "emp rate", agg%emprate(tc)
        
        agg%hrs(tc) = agg%hrs(tc)/(jr(1)-1 - js)
        print*, "hours", agg%hrs(tc),agg%hrs1(tc)/(jr(1)-1 - js),agg%hrs2(tc)/(jr(1)-1 - js)
        
        print*, "wageinc", sum(agg%wage_inc_j(js+1:jr(1)-1,tc))/(jr(1)-1 - (js+1) + 1)
        
        print*, "value delta", agg%v_delta(jt,tc), agg%v_delta(jt,tc) / nu_param, agg%v_delta(jt+1,tc), agg%v_delta(jt+1,tc) / nu_param 
        print*, "value kid", agg%v_kid(js,tc)

        
        ! average HK stock of ADULT population
        if (opt_init_ss==1 .or. opt_fin_ss==1) then
            agg%hk_tot(tc) =  agg%h_j(jt,tc)
            agg%hk_tot_j(:,tc) = 0.0_dp
            do jc = js,nj
                agg%hk_tot_j(jc,tc) =  agg%hk_tot(tc) !* agg%pop_j(jc,tc)
                do sc = 1,ns
                    agg%phi_s_tot_j(sc,jc,tc) = sum(grid%Phi(:,:,:,:,:,:,:,:,:,sc,:,:,jc,tc)) * agg%pop_j(jc,tc)
                enddo
                
            enddo
            do sc = 1,ns
                agg%phi_s_tot(sc,tc) =agg%pop_s(sc,tc) / sum(agg%pop_s(:,tc))  ! sum(agg%phi_s_tot_j(sc,js:nj,tc)) /  sum(agg%pop_j(js:nj,tc))
            enddo
            
        else
            agg%hk_tot_j(:,tc) = 0.0_dp
            do jc = js,nj
                if (jc==js)then
                    agg%hk_tot_j(jc,tc) = agg%h_j(jt,tc) !* agg%pop_j(jc,tc)
                else
                    agg%hk_tot_j(jc,tc) =  agg%hk_tot_j(jc-1,tcm1) !/agg%pop_j(jc-1,tcm1) * agg%pop_j(jc,tc)
                endif
                
                do sc = 1,ns
                    agg%phi_s_tot_j(sc,jc,tc) = sum(grid%Phi(:,:,:,:,:,:,:,:,:,sc,:,:,jc,tc)) * agg%pop_j(jc,tc)
                enddo
            enddo
            do sc = 1,ns
                agg%phi_s_tot(sc,tc) =agg%pop_s(sc,tc) / sum(agg%pop_s(:,tc))  !   sum(agg%phi_s_tot_j(sc,js:nj,tc)) /  sum(agg%pop_j(js:nj,tc))
            enddo
            !agg%phi_s_tot(:,tc) = sum(agg%phi_s_tot(:,tc))
            agg%hk_tot(tc) =0.0_dp
            do jc = js,nj
                 agg%hk_tot(tc) = agg%hk_tot(tc) +agg%hk_tot_j(jc,tc)*agg%pop_j(jc,tc)/ sum(agg%pop_j(js:nj,tc))  !sum(agg%hk_tot_j(js:nj,tc)) / sum(agg%pop_j(js:nj,tc))
            enddo
            !agg%hk_tot(tc) = (agg%hk_tot(tcm1) * (agg%pop_tot(tc) - 1.0_dp) + agg%h_j(jt,tc-1)) / agg%pop_tot(tc)
        endif
        
        ! average CL share of ADULT population
        agg%clshr_tot(tc) = agg%clshr_tot(tc) / agg%pop_tot(tc)
        
        agg%hk_jasp(:,:,tc) = agg%hk_jasp(:,:,tc) / agg%clshr_jasp(:,:,tc)
        
        agg%wage_inc_s(:,tc) = agg%wage_inc_s(:,tc) / agg%pop_s(:,tc)
        agg%fe_s(:,tc) = agg%fe_s(:,tc) / agg%pop_s(:,tc)
  
        ! compute acquired human capital, at age jt 
        agg%hk(tc) = agg%h_j(jt,tc) / agg%poppar_j(jt,tc)
        
        print*, "ratio neg sav", agg%grsav(tc) / agg_vec(1)
        
        do sc=1,ns
            agg%t_sj(sc,jf:jt-1,tc) = agg%t_sj(sc,jf:jt-1,tc)  / (ft_lab*agg%poppar_sj(sc,jf:jt-1,tc))
            agg%m_sj(sc,:,tc) = agg%m_sj(sc,:,tc)  / (earn_si_lo*agg%poppar_sj(sc,:,tc))
            
            agg%h_sj(sc,:,tc) = agg%h_sj(sc,:,tc)  / agg%poppar_sj(sc,:,tc)
            
            agg%t_sgj(sc,1,:,tc) = agg%t_sgj(sc,1,:,tc)  / (ft_lab*agg%poppar_sgj(sc,1,:,tc))
            agg%m_sgj(sc,1,:,tc) = agg%m_sgj(sc,1,:,tc)  / (earn_si_lo*agg%poppar_sgj(sc,1,:,tc))
            agg%t_sgj(sc,2,:,tc) = agg%t_sgj(sc,2,:,tc)  / (ft_lab*agg%poppar_sgj(sc,2,:,tc))
            agg%m_sgj(sc,2,:,tc) = agg%m_sgj(sc,2,:,tc)  / (earn_si_lo*agg%poppar_sgj(sc,2,:,tc))
            
            agg%t_sgj_cpl(sc,1,:,tc) = agg%t_sgj_cpl(sc,1,:,tc)  / (ft_lab*agg%poppar_sgj_cpl(sc,1,:,tc))
            agg%m_sgj_cpl(sc,1,:,tc) = agg%m_sgj_cpl(sc,1,:,tc)  / (earn_si_lo*agg%poppar_sgj_cpl(sc,1,:,tc))
            agg%t_sgj_cpl(sc,2,:,tc) = agg%t_sgj_cpl(sc,2,:,tc)  / (ft_lab*agg%poppar_sgj_cpl(sc,2,:,tc))
            agg%m_sgj_cpl(sc,2,:,tc) = agg%m_sgj_cpl(sc,2,:,tc)  / (earn_si_lo*agg%poppar_sgj_cpl(sc,2,:,tc))
            do sc1=1,ns
                agg%t_ssgj_cpl(sc,sc1,1,:,tc) = agg%t_ssgj_cpl(sc,sc1,1,:,tc)  / (ft_lab*agg%poppar_ssgj_cpl(sc,sc1,1,:,tc))
                agg%m_ssgj_cpl(sc,sc1,1,:,tc) = agg%m_ssgj_cpl(sc,sc1,1,:,tc)  / (earn_si_lo*agg%poppar_ssgj_cpl(sc,sc1,1,:,tc))
                agg%t_ssgj_cpl(sc,sc1,2,:,tc) = agg%t_ssgj_cpl(sc,sc1,2,:,tc)  / (ft_lab*agg%poppar_ssgj_cpl(sc,sc1,2,:,tc))
                agg%m_ssgj_cpl(sc,sc1,2,:,tc) = agg%m_ssgj_cpl(sc,sc1,2,:,tc)  / (earn_si_lo*agg%poppar_ssgj_cpl(sc,sc1,2,:,tc))
                agg%h_ssgj_cpl(sc,sc1,1,:,tc) = agg%h_ssgj_cpl(sc,sc1,1,:,tc)  / agg%poppar_ssgj_cpl(sc,sc1,1,:,tc)
                agg%h_ssgj_cpl(sc,sc1,2,:,tc) = agg%h_ssgj_cpl(sc,sc1,2,:,tc)  / agg%poppar_ssgj_cpl(sc,sc1,2,:,tc)
            enddo
            agg%h_sgj(sc,1,:,tc) = agg%h_sgj(sc,1,:,tc)  / agg%poppar_sgj(sc,1,:,tc)
            agg%h_sgj(sc,2,:,tc) = agg%h_sgj(sc,2,:,tc)  / agg%poppar_sgj(sc,2,:,tc)
            agg%h_sgj_cpl(sc,1,:,tc) = agg%h_sgj_cpl(sc,1,:,tc)  / agg%poppar_sgj_cpl(sc,1,:,tc)
            agg%h_sgj_cpl(sc,2,:,tc) = agg%h_sgj_cpl(sc,2,:,tc)  / agg%poppar_sgj_cpl(sc,2,:,tc) 
           
            agg%t_s(sc,tc) = sum(agg%t_sj(sc,jf:jt-1,tc)) / (ft_lab*(jt-jf))
            
            agg%m_s(sc,tc) = sum(agg%m_sj(sc,jf:jt-1,tc)) / (earn_si_lo*(jt-jf))
        enddo

print*, "hsc1sng", agg%h_sgj(1,2,jf:jt,tc)
print*, "hsc2sng", agg%h_sgj(2,2,jf:jt,tc)
print*, "hsc3sng", agg%h_sgj(3,2,jf:jt,tc)
print*, "hsc4sng", agg%h_sgj(4,2,jf:jt,tc)
print*, "hsc1cpl", agg%h_sgj_cpl(1,1,jf:jt,tc)
print*, "hsc2cpl", agg%h_sgj_cpl(2,1,jf:jt,tc)
print*, "hsc3cpl", agg%h_sgj_cpl(3,1,jf:jt,tc)
print*, "hsc4cpl", agg%h_sgj_cpl(4,1,jf:jt,tc)
        
        agg%tgrad(tc) = 0.5_dp * (agg%t_s(3,tc)/agg%t_s(1,tc) + agg%t_s(3,tc)/agg%t_s(1,tc)  )
        
        tinv_slope_j = 0.0_dp
        do jc = jf,jt-2
            
            tinv_slope_j(tc) =( agg%t_j(jc+1,tc) - agg%t_j(jc,tc) ) / (jc+1-jc)
            moninv_slope_j(tc) =( agg%m_j(jc+1,tc) - agg%m_j(jc,tc) ) / (jc+1-jc)
            
        enddo
        agg%tinv_slope(tc) =agg%t_j(jf,tc)/agg%t_j(jt-1,tc) !sum(tinv_slope_j ) / (jt-jf-1) !agg%t_j(jf,tc)/agg%t_j(jt-1,tc)   ! sum(tinv_slope_j ) / 7
        agg%moninv_slope(tc) =agg%m_j(jf,tc)/agg%m_j(jt-1,tc)  !sum(moninv_slope_j ) / (jt-jf-1) 
        
        agg%ivt_sg(:,1,tc) =( agg%ivt_sg(:,1,tc) / agg%poppar_sgj(:,1,jt,tc) )  /earn_si_lo 
        agg%ivt_sg(:,2,tc) = (agg%ivt_sg(:,2,tc) / agg%poppar_sgj(:,2,jt,tc) )  /earn_si_lo
        
        agg%ivt_sg_cpl(:,1,tc) =( agg%ivt_sg_cpl(:,1,tc) / agg%poppar_sgj_cpl(:,1,jt,tc) )  /earn_si_lo 
        agg%ivt_sg_cpl(:,2,tc) = (agg%ivt_sg_cpl(:,2,tc) / agg%poppar_sgj_cpl(:,2,jt,tc) )  /earn_si_lo
        
        agg%ivt_s(:,tc) = (agg%ivt_s(:,tc) / agg%poppar_sj(:,jt,tc) )  /earn_si_lo
        
        agg%ivt_pkid(tc) = agg%ivt_pkid(tc)/( agg%poppar_j(jt,tc)* earn_si_lo )
        
        print*, "here", agg%t_j(jf,tc), t1_str
        !pause
        
        print*, " average inv", agg%moninv(tc), agg%tinv(tc) , moninv_str, tinv_str
        print*, "ratio", agg%moninvratio(tc)
        print*, " m inv", agg%m_j(jf:jt,tc)
        print*, " t inv", agg%t_j(jf:jt,tc) 
        print*, "hc", agg%h_j(jf:jt,tc)
         
        print*, "t s", agg%t_s(:,tc)
        print*, "m s", agg%m_s(:,tc) 
        print*, "h s", agg%h_sj(:,jt,tc)
        
        !print*, "ts s", agg%t_s(:,tc)
        !print*, "ms s", agg%m_s(:,tc) 
        !print*, "hs s", agg%h_sj(:,jt,tc)

        print*, "cevs", agg%cev_s(:,tc) / agg%popjacol_s(:,tc) ! agg%poppar_sj(:,jt,tc) 
        print*, "hkjacol", agg%hkjacol_s(:,tc) / agg%popjacol_s(:,tc)
        print*, "hkja", agg%hkja_s(:,tc) / agg%popja_s(:,tc)
        
        do sc=1,ns
            print*, "m sng",sc, sum(agg%m_sgj(sc,1,jf:jt-1,tc))/(jt-jf), sum(agg%m_sgj(sc,2,jf:jt-1,tc))/(jt-jf)
	        print*, "m cpl",sc, sum(agg%m_sgj_cpl(sc,1,jf:jt-1,tc))/(jt-jf), sum(agg%m_sgj_cpl(sc,2,jf:jt-1,tc))/(jt-jf)
            print*, "m cpl11",sc, sum(agg%m_ssgj_cpl(sc,sc,1,jf:jt-1,tc))/(jt-jf), sum(agg%m_ssgj_cpl(sc,sc,2,jf:jt-1,tc))/(jt-jf)
            
            print*, "t sng",sc, sum(agg%t_sgj(sc,1,jf:jt-1,tc))/(jt-jf), sum(agg%t_sgj(sc,2,jf:jt-1,tc))/(jt-jf)
            print*, "t cpl",sc, sum(agg%t_sgj_cpl(sc,1,jf:jt-1,tc))/(jt-jf), sum(agg%t_sgj_cpl(sc,2,jf:jt-1,tc))/(jt-jf)
            print*, "t cpl11",sc, sum(agg%t_ssgj_cpl(sc,sc,1,jf:jt-1,tc))/(jt-jf), sum(agg%t_ssgj_cpl(sc,sc,2,jf:jt-1,tc))/(jt-jf)
        enddo
        
        print*, "ivt sng", agg%ivt_sg(:,1,tc), agg%ivt_sg(:,2,tc) 
        print*, "ivt cpl", agg%ivt_sg_cpl(:,1,tc), agg%ivt_sg_cpl(:,2,tc) 
        print*, "h sng", agg%h_sgj(:,1,jt,tc), agg%h_sgj(:,2,jt,tc)
        print*, "h cpl", agg%h_sgj_cpl(:,1,jt,tc), agg%h_sgj_cpl(:,2,jt,tc)

        !agg%avtax(tc) = 1.0_dp -  (1.0_dp - tau_pr) *agg%avtax(tc)/(jr(1) - js + 1)
        !agg%avtax_av(tc) = 1.0_dp - agg%avtax_av(tc)/(jr(1) - js + 1) - earn_si_lo * 0.167
        agg%avtax_av(tc) = sum(agg%avtax_av_j(js+1:jr(1) - 1,tc))/sum(agg%pop_j(js+1:jr(1)-1,tc))
        agg%avtax(tc) = sum(agg%avtax_j(js+1:jr(1) - 1,tc))/sum(agg%pop_j(js+1:jr(1)-1,tc))
        print*, "av marg tax", agg%avtax(tc)
        print*, "av av tax",agg%avtax_av(tc)
         
        !
        !
        !pause
        !
        !print*, "av marg tax", agg%avtax_j(:,tc)
        !print*, "av av tax",agg%avtax_av_j(:,tc)
        !
        !pause
        
        print*, "av marg tax bottom", agg%margtaxbot(tc)/(agg%avtaxbot_frac(tc)  ) ! age jr-5
        print*, "av av tax bottom", agg%avtaxbot(tc)/(agg%avtaxbot_frac(tc)  ) ! age jr-5
        print*, "av tax ratio", agg%avtax(tc)/agg%avtax_av(tc)

       ! agg%educ_matrix(2,:,tc) = agg%educ_matrix(2,:,tc) + agg%educ_matrix(3,:,tc)
        
        print*, "CL share by parent", agg%educ_matrix(1,:,tc) / sum(agg%educ_matrix(1,:,tc)), agg%educ_matrix(2,:,tc) / (sum(agg%educ_matrix(2,:,tc))), agg%educ_matrix(3,:,tc) / sum(agg%educ_matrix(3,:,tc)), agg%educ_matrix(4,:,tc) / sum(agg%educ_matrix(4,:,tc)) !, agg%educ_matrix(3,:,tc) , sum(agg%educ_matrix(3,:,tc))
        
        print*, "CL share by parsng", agg%educ_matrix_mg(1,2,1,:,tc) / sum(agg%educ_matrix_mg(1,2,1,:,tc)), & 
            agg%educ_matrix_mg(1,2,2,:,tc) / (sum(agg%educ_matrix_mg(1,2,2,:,tc))), agg%educ_matrix_mg(1,2,3,:,tc) / sum(agg%educ_matrix_mg(1,2,3,:,tc)), agg%educ_matrix_mg(1,2,4,:,tc) / sum(agg%educ_matrix_mg(1,2,4,:,tc)) !, agg%educ_matrix(3,:,tc) , sum(agg%educ_matrix(3,:,tc))
        
        write(tc_char , '(I2)')   tc
        
        open(unit=171,file='output/edumatrixsng'//tc_char//'.txt')
        do sc1=1,ns ! sc parent
            !do sc2=1,ns ! sckid
                write(171,'(4f50.16)') agg%educ_matrix_mg(1,2,sc1,1,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc)),agg%educ_matrix_mg(1,2,sc1,2,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc)),agg%educ_matrix_mg(1,2,sc1,3,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc)),agg%educ_matrix_mg(1,2,sc1,4,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc))  
            !enddo
        enddo
        close(171)

        open(unit=172,file='output/edumatrixcpl'//tc_char//'.txt')
        do sc1=1,ns ! sc parent
            !do sc2=1,ns ! sckid
                write(172,'(4f50.16)') agg%educ_matrix_mg(2,1,sc1,1,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc)),agg%educ_matrix_mg(2,1,sc1,2,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc)), & 
                    agg%educ_matrix_mg(2,1,sc1,3,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc)),agg%educ_matrix_mg(2,1,sc1,4,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc))  
            !enddo
        enddo
        close(172) 
        
        
        if (tc>1)then
        
        	open(unit=173,file='output/deltaedumatrixsng'//tc_char//'.txt')
        	do sc1=1,ns ! sc parent
            	!do sc2=1,ns ! sckid
                	write(173,'(4f50.16)') agg%educ_matrix_mg(1,2,sc1,1,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc))-agg%educ_matrix_mg(1,2,sc1,1,1) / sum(agg%educ_matrix_mg(1,2,sc1,:,1)),  & 
                        agg%educ_matrix_mg(1,2,sc1,2,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc))-agg%educ_matrix_mg(1,2,sc1,2,1) / sum(agg%educ_matrix_mg(1,2,sc1,:,1)), & 
                        agg%educ_matrix_mg(1,2,sc1,3,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc))-agg%educ_matrix_mg(1,2,sc1,3,1) / sum(agg%educ_matrix_mg(1,2,sc1,:,1)), & 
                        agg%educ_matrix_mg(1,2,sc1,4,tc) / sum(agg%educ_matrix_mg(1,2,sc1,:,tc)) -agg%educ_matrix_mg(1,2,sc1,4,1) / sum(agg%educ_matrix_mg(1,2,sc1,:,1)) 
            	!enddo
        	enddo
        	close(173)

        open(unit=174,file='output/deltaedumatrixcpl'//tc_char//'.txt')
        do sc1=1,ns ! sc parent
            !do sc2=1,ns ! sckid
                write(174,'(4f50.16)') agg%educ_matrix_mg(2,1,sc1,1,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc)) - agg%educ_matrix_mg(2,1,sc1,1,1) / sum(agg%educ_matrix_mg(2,1,sc1,:,1)), & 
                    agg%educ_matrix_mg(2,1,sc1,2,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc))-agg%educ_matrix_mg(2,1,sc1,2,1) / sum(agg%educ_matrix_mg(2,1,sc1,:,1)), & 
                    agg%educ_matrix_mg(2,1,sc1,3,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc))-agg%educ_matrix_mg(2,1,sc1,3,1) / sum(agg%educ_matrix_mg(2,1,sc1,:,1)), & 
                    agg%educ_matrix_mg(2,1,sc1,4,tc) / sum(agg%educ_matrix_mg(2,1,sc1,:,tc))  - agg%educ_matrix_mg(2,1,sc1,4,1) / sum(agg%educ_matrix_mg(2,1,sc1,:,1))  
            !enddo
        enddo
        close(174) 
        
        endif


        print*, "CL share by parCPL1", agg%educ_matrix_mg(2,1,1,:,tc) / sum(agg%educ_matrix_mg(2,1,1,:,tc)), & 
            agg%educ_matrix_mg(2,1,2,:,tc) / (sum(agg%educ_matrix_mg(2,1,2,:,tc))), agg%educ_matrix_mg(2,1,3,:,tc) / sum(agg%educ_matrix_mg(2,1,3,:,tc)), agg%educ_matrix_mg(2,1,4,:,tc) / sum(agg%educ_matrix_mg(2,1,4,:,tc)) !, agg%educ_matrix(3,:,tc) , sum(agg%educ_matrix(3,:,tc))
        print*, "CL share by parCPL2", agg%educ_matrix_mg(2,2,1,:,tc) / sum(agg%educ_matrix_mg(2,2,1,:,tc)), & 
            agg%educ_matrix_mg(2,2,2,:,tc) / (sum(agg%educ_matrix_mg(2,2,2,:,tc))), agg%educ_matrix_mg(2,2,3,:,tc) / sum(agg%educ_matrix_mg(2,2,3,:,tc)), agg%educ_matrix_mg(2,2,4,:,tc) / sum(agg%educ_matrix_mg(2,2,4,:,tc)) !, agg%educ_matrix(3,:,tc) , sum(agg%educ_matrix(3,:,tc))
        
        
        print*, "tgrad", agg%tgrad(tc), tgrad_str 
        print*, "tslope", agg%tinv_slope(tc), tinv_slope_str
        print*, "mslope", agg%moninv_slope(tc), moninv_slope_str
        print*, "t1", agg%t_j(jf,tclb),t1_str 
        
        agg%clkid_lhs(tclb) = agg%educ_matrix(1,3,tc) / sum(agg%educ_matrix(1,:,tc))
        agg%clkid_cl(tclb) = agg%educ_matrix(3,3,tc) / sum(agg%educ_matrix(3,:,tc))
        
        call sub_reg_ind(agg%t_j(jf:jt-1,tc) ,agg%betta0(tc),agg%betta1(tc),agg%betta2(tc))
        call sub_reg_ind(agg%m_j(jf:jt-1,tc) ,agg%metta0(tc),agg%metta1(tc),agg%metta2(tc))
        !agg%metta1(tc) = agg%m_j(jt-1,tc)/agg%m_j(jf,tc)
        !agg%metta2(tc) = agg%m_j(jf+1,tc)/agg%m_j(jf,tc) + agg%m_j(jf+2,tc)/agg%m_j(jf+1,tc) + agg%m_j(jf+3,tc)/agg%m_j(jf+2,tc) + agg%m_j(jf+4,tc)/agg%m_j(jf+3,tc)+ agg%m_j(jf+5,tc)/agg%m_j(jf+4,tc)
        !agg%metta2(tc) = agg%metta2(tc)/5
 
        !call sub_reg_ind_lin(agg%m_j(jf:jt-1,tc) ,agg%metta0(tc),agg%metta1(tc),agg%metta2(tc))
        !call sub_reg_ind_lin(agg%t_j(jf:jt-1,tc) ,agg%betta0(tc),agg%betta1(tc),agg%betta2(tc))

        print*, "betta1, betta2", agg%betta1(tc),agg%betta2(tc), demo%betta1_dat(1),demo%betta2_dat(1)
        
        ! edu specific objects
        do jc=1,nj
            do sc=1,ns
                wght_s = agg%pop_sj(sc,jc,tc)
                agg%lab_sj(sc,jc,tc) = agg%lab_sj(sc,jc,tc) / wght_s
                agg%cons_sj(sc,jc,tc) = agg%cons_sj(sc,jc,tc) / wght_s
                agg%ass_sj(sc,jc,tc) = agg%ass_sj(sc,jc,tc) / wght_s
                agg%labinc_sj(sc,jc,tc) = agg%labinc_sj(sc,jc,tc) / wght_s
                agg%totinc_sj(sc,jc,tc) = agg%totinc_sj(sc,jc,tc) / wght_s
                agg%frac_bc_sj(sc,jc,tc) = agg%frac_bc_sj(sc,jc,tc) / wght_s
                agg%frac_nettr_sj(sc,jc,tc) = agg%frac_nettr_sj(sc,jc,tc) / wght_s
            enddo
        enddo
        
        !open(unit=178,file='output/lcprofilesall.txt')
        !do jc=js,nj
        !    write(178,'(6f50.16)') agg%lab_sj(1,jc,tclb)/sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,1,:,:,js,tc)), agg%lab_sj(2,jc,tclb)/sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,2:3,:,:,js,tc)), &
        !        agg%cons_sj(1,jc,tclb)/sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,1,:,:,js,tc)), agg%cons_sj(2,jc,tclb)/sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,2:3,:,:,js,tc)), agg%ass_sj(1,jc,tclb)/sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,1,:,:,js,tc)), agg%ass_sj(2,jc,tclb)/sum(grid%Phi_guess(:,:,:,:,:,:,:,:,:,2:3,:,:,js,tc)) 
        !enddo
        !
        !close(178)
      
        write(tc_char , '(I2)')   tc
        
        open(unit=178,file='output/lcprofiles'//tc_char//'.txt')
        do sc=1,ns
            do jc=js,nj
                write(178,'(7f50.16)')  agg%cons_sj(sc,jc,tc),agg%ass_sj(sc,jc,tc),agg%frac_bc_sj(sc,jc,tc), agg%lab_sj(sc,jc,tc),agg%labinc_sj(sc,jc,tc),agg%totinc_sj(sc,jc,tc),agg%frac_nettr_sj(sc,jc,tc) 
            enddo
        enddo
        close(178)
        
        
        

!pause

      !  print*, "cons", agg%cons_j(:,tclb)
      ! pause 
       ! print*, "bc", agg%frac_bc(jf:jt)
        
       ! pause
        
        !pause       
        
        !call sub_saving(agg,demo,grid,pol,lc) 
        
    ! update G: general TTS budget constraint agg%penstaxrv(tc),
        ! g1 is primary deficit
            agg%gcons(tc) =fun_gcons(agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc)+agg%clsubs(tc) + agg%gdp(1) * gyr_str) 
            print*, "g1 is ", agg%gcons(tc),agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc),agg%clsubs(tc) !,agg%avprem(tc),agg%marprem(tc)/agg%marprem_pop(tc)
            print*, "marg wages CL", agg%marprem(3,tc)/agg%marprem_pop(3,tc)
            print*, "marg wages NCL", agg%marprem(1,tc)/agg%marprem_pop(1,tc)
            print*, "av wages CL", agg%avprem(3,tc)/agg%avprem_pop(3,tc)
            print*, "av wages NCL", agg%avprem(1,tc)/agg%avprem_pop(1,tc)
            
            print*, "poppar", agg%pop_j(:,tc)
    
            print*, "res constr", agg%sav(tc), agg%ass(tc),agg%tot_inc(tc), agg%cons(tc),agg%labinctaxrv(tc)+agg%penstaxrv(tc)+agg%capinctaxrv(tc)+agg%constaxrv(tc),agg%tau_p(tc)*agg%wage_inc(tc),agg%ben_p(tc),agg%clcost(tc) ,agg%moninv_tot(tc),agg%ivt_tot(tc)
            print*, "agg res",  agg%ass(tc)+agg%tot_inc(tc)+agg%ben_p(tc)
            print*, "agg exp", agg%sav(tc)+agg%labinctaxrv(tc)+agg%penstaxrv(tc)+agg%capinctaxrv(tc)+agg%constaxrv(tc)+agg%tau_p(tc)*agg%wage_inc(tc)+agg%clcost(tc)+agg%moninv_tot(tc)+agg%ivt_tot(tc)
            print*, "fees", fee_flow,col_subs_flow,priv_subs_param
        if (jmax==nj .and. opt_upd)then
            agg%pop_j_temp(:,tc) = agg%pop_j(:,tc)
            agg%totrev(tc) = agg%labinctaxrv(tc)+agg%penstaxrv(tc)+agg%capinctaxrv(tc)+agg%constaxrv(tc)
            
            if (tc>1)then
                do jc=nj,js,-1
                    !if (jc==js)then ! newborns
                    !    ! updated average fertility
                        fr_av = demo%nkids_input(2,1) *(1.0_dp -  agg%clfrac(tc)  ) +  demo%nkids_input(2,ns) *( agg%clfrac(tc) )
                        fr_av  =fr_av * (agg%poppar_j(jt,tc) / agg%pop_j(jt,tc) )
                    !
                    !    demo%frac_jt(jc,tc) = demo%frac_jt(jt,tc) * fr_av
                    !
                    !    print*, "av fert", fr_av, demo%frac_jt(jt,tc) * fr_av, demo%nkids_input(1,1),demo%nkids_input(1,3),agg%clfrac(tc) 
                    !else
                    !
                    !    demo%frac_jt(jc,tc) = demo%frac_jt(jc-1,tc-1)
                    !endif
                
                enddo
                if (t0<t1)then
                    demo%popgrt(tc) =sum(agg%pop_j(js:nj,tc)) / sum(agg%pop_j(js:nj,1) ) - 1.0_dp + popgr  ! sum(demo%frac_jt(js:nj,tc)) / sum(demo%frac_jt(js:nj,tc-1)) - 1.0_dp
                    
                    print*, "here", sum(demo%frac_jt(js:nj,tc-1)), sum(demo%frac_jt(js:nj,tc)) 
                else
                    !demo%popgrt(tc) = fr_av
                
                    !demo%frac_jt(jt,tc) = demo%frac_jt(ja,tc)/(popgr+1^(jt-js)) * surv
                    !demo%frac_jt(jt,tc) = demo%frac_jt(ja,tc) / fr_av
                
                    !fr_av = (popgr+1)^(jt-js)
                    fr_av =  demo%nkids_input(2,1) *(1.0_dp -  sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc))  ) +  demo%nkids_input(2,ns) *( sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)) ) !agg%pop_j(js,tc ) / agg%pop_j(jt,tc)
                    fr_av = fr_av * (agg%poppar_j(jt,tc) / agg%pop_j(jt,tc) )
                    demo%popgrt(tc) = fr_av**(1.0_dp/(jt-js)) - 1.0_dp
                
                endif
            
            print*, "pop growth tc", tc, demo%popgrt(tc) 
            
        else
                
                fr_av =  demo%nkids_input(2,1) *(1.0_dp -  sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc))  ) +  demo%nkids_input(2,ns) *( sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)) ) !agg%pop_j(js,tc ) / agg%pop_j(jt,tc)
                fr_av = fr_av * (agg%poppar_j(jt,tc) / agg%pop_j(jt,tc) )
                demo%popgrt(tc) = fr_av**(1.0_dp/(jt-js)) - 1.0_dp
                
            
        endif
        print*, "pop growth tc", tc, demo%popgrt(tc)
            
           ! if (opt_ge_upd==1)then
                ! update psi_lambda
                if (opt_lambda==1 )then ! balanced government budget
                    if (opt_tauc==0)then
                        agg%psi_lambda(tc) = f_lambda(agg%hsvtmp1(tc),agg%hsvtmp2(tc),gconst,agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc),agg%penstaxrv(tc),agg%constaxrv(tc))
                    else
                        agg%tau_c(tc) = f_tau_c(agg%cons(tc),gconst,agg%igov(tc)+agg%clsubs(tc),agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc))
                    endif
                    
                elseif (opt_lambda==2)then ! allow for gvt debt
                    
                    if (tc==1 ) then ! ss debt Here take out psi_lambda in finall ss, do not update it
                        ! because it will be updated in the transition
                        ! so, you guess some psilambda in the outer loop, then resolve finall ss, and transition, and 
                        ! solve rootfinding problem for psi inside, it's also fine to keep psi in the outer loop
                        ! gvt spending is taken as given, based on initial gyr
                        !agg%gcons(tc) = agg%gdp(tc) * gyr_str
                        ! byr is also exogenously given
                        gconst = gyr_str * agg%gdp(tc) !+ byr_str * agg%gdp(tc)
                        ! targeted debt level (use it to solve for lambda)
                        agg%debt(tc) = byr_str * agg%gdp(tc)
                        agg%byr(tc) = agg%debt(tc) / agg%gdp(tc)
                        
                        if (opt_ge_upd==1)then
                            agg%psi_lambda(tc) = f_lambda(agg%hsvtmp1(tc),agg%hsvtmp2(tc), & 
                                gconst+agg%debt(tc)*(agg%ret(tc)-demo%popgrt(tc) - demo%lamt(tc) - demo%popgrt(tc) * demo%lamt(tc)),agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc),agg%penstaxrv(tc),agg%constaxrv(tc))
                            print*, "updated lambda with debt", agg%psi_lambda(tc)
                        endif
                         !agg%debt(tc) = f_debtss(gconst,agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc), & 
                         !   agg%penstaxrv(tc),agg%constaxrv(tc),agg%labinctaxrv(tc),agg%ret(tc))
                        debtss = agg%gcons(tc) / (agg%ret(tc) - demo%popgrt(tc) - demo%lamt(tc) - demo%popgrt(tc) * demo%lamt(tc))
                        agg%debt(tc) =debtss
                        agg%byr(tc) = debtss / agg%gdp(tc)
                        print*, "init ss debt",agg%debt(tc), debtss, agg%byr(tc) 
                    else ! transition: backward shooting
                        ! if this is done backwards for all t, then cant do it within here
                        ! psi_lambda is not time varying: need to compute a one time policy change
                        ! therefore update tau only after solution for all t is computed
                        ! then from final ss guess debt, rolling backwards, with given tau, arrive at some initial ss
                        ! if doesnt match initial ss, update psi_lambda, which means also update final ss
                        ! in every period want to somehow compute deficit: bp1 is t, and the rest are from t-1
                        ! for given tau, compute required debt in the current period as residual
                        if (tc==nt)then ! update fin ss value when computing transition
                            if (tc==nt)then
                                !agg%debt(tc) = f_debtss(gconst,agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc), & 
                                !    agg%penstaxrv(tc),agg%constaxrv(tc),agg%labinctaxrv(tc),agg%ret(tc))
                                !agg%debt(tc) = agg%gcons(tc) / agg%ret(tc)
                                agg%debtss(tc) = agg%gcons(tc) / (agg%ret(tc) - demo%popgrt(tc) - demo%lamt(tc) - demo%popgrt(tc) * demo%lamt(tc))
                       
                                agg%byr(tc) = agg%debt(tc) / agg%gdp(tc)
                                print*, "fin ss debt", agg%debt(tc), agg%byr(tc),agg%debtss(nt)
                            endif
                        else
                            
                            !agg%debt(tc) = f_debt(gconst + agg%debt(tc-1) * f_aftertaxR(1.0_dp+agg%ret(tc-1),tau_k), & 
                            !    agg%igov(tc-1)+agg%clsubs(tc-1),agg%capinctaxrv(tc-1), & 
                            !    agg%penstaxrv(tc-1),agg%constaxrv(tc-1),agg%labinctaxrv(tc-1))
                            !agg%debt(tc) =( (1.0_dp + agg%ret(tc-1) ) * agg%debt(tc-1) - agg%gcons(tc-1) ) /( (1.0_dp +demo%lamt(tc-1) )*  (1.0_dp + demo%popgrt(tc-1)  ) )
                            !agg%byr(tc) = agg%debt(tc)/ agg%gdp(tc)
                            agg%def(tc) = agg%debt(tc) - agg%debt(tc-1) * f_aftertaxR(1.0_dp+agg%ret(tc-1),tau_k )
                            agg%dyr(tc) = agg%def(tc)/ agg%gdp(tc)
                            print*, "debt in period t", agg%debt(tc),agg%byr(tc),agg%def(tc), tc
                        endif
                        
                    endif
                    
                    
                endif
                
                    
                !! update labor tax rev
                !print*, "labrv before",agg%psi_lambda(tc), agg%labinctaxrv(tc)
                !agg%labinctaxrv(tc) = agg%hsvtmp1(tc) -  agg%hsvtmp2(tc) *  (agg%psi_lambda(tc) * 1.99_dp * 0.4_dp)**(0.18_dp)
                !print*, "labrv after",agg%psi_lambda(tc), agg%labinctaxrv(tc)
                !agg%gcons(tc) = fun_gcons(agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc)+agg%clsubs(tc) ) 
                !print*, "check gcons", agg%psi_lambda(tc), agg%gcons(tc), gconst
           ! endif
       
            !if (opt_cpl) agg%hrs(tc) = agg%hrs(tc) + agg%hrs1(tc) + agg%hrs2(tc)
           
            ! now have to make assets, capital and debt consistent
            if (tc==1)then
                agg%cap(tc) = agg%cap(tc)- agg%gdp(1) * 0.25_dp
            else
                !if (opt_pe==1)then
                !    agg%cap(tc) = agg%cap(tc)- agg%gdp(1) * 0.25_dp
                !else
                    if (opt_bconst==0)then
                        agg%cap(tc) = agg%cap(tc)- agg%debt(tc)/(1.0_dp  )
                    elseif (opt_bconst==1)then
                        agg%cap(tc) = agg%cap(tc)- agg%debt(1)/agg%gdp(1) * agg%gdp(tc)    
                    elseif (opt_bconst==2)then
                        agg%cap(tc) = agg%cap(tc)- agg%debt(1)/(1.0_dp  )    
                    endif
                !endif
            endif

            if (agg%cap(tc) <= 0.0_dp ) then
                print*, "non positive capital, how can this be?", agg%cap(tc),tc
            endif
            
            ! stocks in period 2 of transition:
        if (tc==2) then
            ! recall that aggregation is in terms of detrended variables: 
            agg%cap(tc)=agg%cap(tc-1)
            agg%bq(tc)=agg%bq(tc-1)
           ! agg%debt(tc) = agg%debt(tc-1)
           ! agg%def(tc) = agg%def(tc-1)
        endif
             
        
            ! aggregate output, wages and returns           
            call sub_outwageret(tc,agg%gdp(tc),agg%cap(tc),agg%lab(tc),agg%lab_s(:,tc), & 
                agg%wage(tc),agg%wage_s(:,tc),agg%ret(tc),1.0_dp)   
            
            if (opt_ge_upd==1 .and. opt_lambda==2 .and. tc>1)then
 
                if (opt_bconst==1)then ! BY
                
                
                    if (tc<nt)then

                                agg%psi_lambda(tc) = f_lambda(agg%hsvtmp1(tc),agg%hsvtmp2(tc), & 
                                    gconst+ agg%debt(1)/agg%gdp(1)*( (1.0_dp + agg%ret(tc) ) * agg%gdp(tc-1) - (1.0_dp + demo%popgrt(tc) ) * (1.0_dp + demo%lamt(tc) ) *agg%gdp(tc) ),agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc),agg%penstaxrv(tc),agg%constaxrv(tc)  )
    !                            print*, "updated lambda with debt", agg%psi_lambda(tc)

                    else


                                agg%psi_lambda(tc) = f_lambda(agg%hsvtmp1(tc),agg%hsvtmp2(tc), & 
                                    gconst+ agg%debt(1)/agg%gdp(1)* agg%gdp(tc)*(agg%ret(tc)-demo%popgrt(tc) - demo%lamt(tc) - demo%popgrt(tc) * demo%lamt(tc)),agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc),agg%penstaxrv(tc),agg%constaxrv(tc))
                                print*, "updated lambda with debt", agg%psi_lambda(tc)
                       
                    endif
                    
                elseif (opt_bconst==2)then ! B const
                    
                        agg%psi_lambda(tc) = f_lambda(agg%hsvtmp1(tc),agg%hsvtmp2(tc), & 
                                    gconst+ agg%debt(1)/agg%gdp(1)* agg%gdp(tc)*(agg%ret(tc)-demo%popgrt(tc) - demo%lamt(tc) - demo%popgrt(tc) * demo%lamt(tc)),agg%igov(tc)+agg%clsubs(tc),agg%capinctaxrv(tc),agg%penstaxrv(tc),agg%constaxrv(tc))
                                print*, "updated lambda with debt", agg%psi_lambda(tc)
                        
                    
                    
                endif
                
                    
                    
                    
                    
            endif

            
            ! per capita gdp
            agg%gdpn(tc) = agg%gdp(tc) / agg%popfull(tc)
            
            if (tc==1)then
                gconst = agg%gdp(1) * gyr_str
                !agg%debt(1) = agg%gdp(1) * byr_str
            endif
            
            
            ! the income measure to be used in the debt loop
            agg%labinctaxrvyr(tc) = agg%labinctaxrv(tc) / agg%psi_lambda(tc)
           
            print*,"agg prices", agg%cap(tc),agg%lab(tc),agg%wage(tc),agg%ret(tc),agg%wage_s(:,tc)
            !pause
        
            ! check that in soe didn't change interst
            if (opt_soe)then
                dist_ret = abs(agg%ret(tc)-agg%ret_soe(tc))
                if (dist_ret>epsi)then
                    print*, "In SOE ret remains constant!"
                    pause
                endif
            endif
            
            ! inter vivos transfers as a fraction of total assets
            agg%ivty(tc) = agg%ivt_pkid(tc) !/ agg%cap(tc) 
        
            !! update G: general TTS budget constraint agg%penstaxrv(tc),
            !agg%gcons(tc) = fun_gcons(agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc)+agg%clsubs(tc) ) 
            
            ! gross college cost as a fraction of GDP per capita
            agg%clcosty(tc) = fee_flow / earn_si_lo !agg%evearn(tclb) !agg%gdpn(tc)
            
            print*, "CL cost to GDPN", agg%clcosty(tc), agg%clcost(tc)/ earn_si_lo
            
           
            
            ! save baseline level of G (to compute resulting budget deficit in the experiment)
            if (opt_trref==0)then
                !agg%gcons(tc) = gyr_str * agg%gdp(tc)
                gcons_base = gyr_str * agg%gdp(tc)
                totinc_base = agg%tot_inc(tc)
            endif
                        
            ! in the lockdown, compute budget deficit: PARENT generation
           
            !print*, agg%igov(tc),agg%clsubs(tc), agg%gdp(tc)
            !pause
            
            !print*, "revenues", agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc), agg%gdp(tc),agg%cons(tc)
            !pause
            !if (opt_trref==0) then 
            !    ! baseline target: gyr
            !    gconst = gyr_str * agg%gdp(tc)
            !else
            !    ! balance gvt budget, i.e. g=g_base
            !    gconst = gcons_base
            !endif
             
            gconst = gcons_base
            
           ! agg%gcons(tc) = fun_gcons(agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc)+agg%clsubs(tc) )
            !! update G: general TTS budget constraint agg%penstaxrv(tc),
            !agg%gcons(tc) = fun_gcons(agg%labinctaxrv(tc),agg%penstaxrv(tc),agg%capinctaxrv(tc),agg%constaxrv(tc),agg%igov(tc)+agg%clsubs(tc) ) 
            !            
            
            
            !print*, "check gcons", agg%psi_lambda(tc), agg%gcons(tc), gconst,agg%gconsr(tc),agg%hsvtmp1(tc),agg%hsvtmp2(tc)
            !pause
            
            agg%ky(tc) = agg%cap(tc)/agg%gdp(tc)
            print*, "g is ", agg%gcons(tc), agg%asswp(tc)/agg%gdp(tc) , agg%cap(tc),agg%gdp(tc),agg%gdpn(tc),agg%ivt_pkid(tc)
           ! pause
           
           
            if (opt_ge_upd==1)then
                ! aggregate pension insurance contributions
                agg%pcontr(tc)=agg%tau_p(tc)*agg%wage_inc(tc)
            
                if (opt_trref==0)then
                    agg%rho_p(tc)=f_rho(agg%pcontr(tc),agg%ben_p(tc),agg%rho_p(tc))
                else
                    agg%tau_p(tc)=f_tau(agg%wage_inc(tc),agg%ben_p(tc))
                endif
                
        
                ! here we need avergae wage
                if (tc==1)then
                    agg%avwage(tc) = fun_averwage(agg%earn_ft(tc),agg%hrs_ft(tc))
                    agg%avearn(tc) = agg%earn(tc)/agg%allpopstock(tc)
                else
                    agg%avwage(tc) = agg%avwage(1) 
                    agg%avearn(tc) = agg%avearn(1)
                endif
                
                print*, "beq aggr", agg%bq(tc) , agg%allpopstock(tc), agg%avwage(tc)
                
                ! net acc be    quests
                !agg%bq(tc)  = agg%bq(tc) - fee_flow * priv_subs_param
                
                !if (agg%bq(tc) < 0.0_dp ) then
                !    print*, " neg acc beq?",tc
                !    pause
                !endif
                
                
                !agg%bq(tc) = agg%bq(tc) / agg%allpopstock(tc)
                if ( (agg%bq(tc) - 0.166 * agg%clcost(tc) ) < 0.0_dp )then
                    agg%privclsubs(tc) = agg%bq(tc) 
                else
                    agg%privclsubs(tc) = 0.166_dp *  agg%clcost(tc)
                endif
                print*, "priv subs",  agg%privclsubs(tc)
                agg%bqr(tc) = (agg%bq(tc)/ sum(agg%pop_j(js:jr(1)-1,tc)) )    / agg%avwage(tc)
                
                 
            endif
            !print*, "avearn", tc, agg%avearn(tc),agg%earn(tc),agg%allpopstock(tc)

            
         !   print*, "pens contr",agg%pcontr(tc), agg%tau_p(tc),agg%rho_p(tc),agg%wage_inc(tc),agg%ben_p(tc),agg%gcons(tc),agg%gdp(tc),agg%gcons(tc)/agg%gdp(tc)
          !  pause
        
            ! investment
            if (t0==t1) then		! steady state: no population growth
	            ! K_t+1 = K_t (1-delta)+I_t
                ! Detrending: k_t+1 (1+lam) = k_t (1-delta) + i_t
                ! Steady state: i = k*(delta+lam) 
                agg%inv(tc)=( (1.0_dp + demo%lamt(tc)   )  * (1.0_dp+demo%popgrt(tc)) -  (1.0_dp - delt))*agg%cap(tc)
            else
                ! recall that aggregation is in terms of detrended variables
                ! hence, we need growth adjustment here:
                ! Detrending: k_t+1 (1+lam) = k_t (1-delta) + i_t
                ! Hence: i_t = k_t+1 (1+lam) - k_t (1-delta)
                agg%inv(tcm1)= (1.0_dp + demo%lamt(tcm1) )* (1.0_dp+demo%popgrt(tc)) * agg%cap(tc)-agg%cap(tcm1)*(1.0_dp-delt)
                if (tc==nt) then
                    agg%inv(tc)=agg%inv(tcm1)   ! recall: aggregation is in terms of detrended variables, no pop growth in final steady state
                endif
            endif
        
            ! add consumption and depreciation to net savings to get gross savings
            agg%grsav(tc)=agg%grsav(tc)+delt*agg%cap(tc)-gconst
        
            ! capital output ratio
            if (opt_pe<1)then
                agg%ky(tc)=agg%cap(tc)/agg%gdp(tc)
            else
                !agg%ky(tc)=agg%asswp(tc) /agg%net_wage_inc(tc)
                agg%ky_j(:,tc)=agg%asswp_j(:,tc) /agg%net_wage_inc_j(:,tc)
               ! agg%ky(tc)=sum(agg%ky_j(jf:jr(tc)-4,tc) ) / (jr(tc)-4  - jf+1)
                !print*, agg%ky_j(jf:jr(tc)-1,tc)
            endif
                  
            if (opt_check) then
                print*, 'updated returns: ', agg%ret(tc)
                print*, 'updated marginal products of labor: '
                do sc=1,ns
                    
                    print*, agg%wage_s(sc,tc), ' '
                    
                end do
                print*, ' '
            
                print*, 'updated taxes: ', agg%tau_p(tc)
            endif
        endif
    
    
    endif
    
    agg%gconsr(tc)= gyr_str
    
    print*, "vars and moms when exiting ", agg%tau_p(tc), agg%gconsr(tc),agg%ky(tc),agg%beta(tc),agg%wage_s(:,tc),agg%psi_lambda(tc),agg%ivty(tc),agg%nu(tc),agg%avwage(tc),agg%avearn(tc),agg%rho_p(tc)
   ! pause
    
    !deallocate(Phi,Phi_kid,Phi_kid_old,Phi_kid_cpl,Phi_kid_old_cpl,Phi_child)
    deallocate(Phi,Phi_cpl,Phi_kid,Phi_kid_old,Phi_kid_cpl,Phi_kid_old_cpl,Phi_child)    
!end do  ! end do tc
    
  !  agg%ret(tc) = ret_old
!agg%wage_s(:,tc) = 1.0_dp
    
end subroutine sub_meas_aggr_full
! ---------------------------------------------------------------------------   


! ---------------------------------------------------------------------------------
subroutine sub_prob_dc_new_kid(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,ec, &
    hc,ic,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc_s,valsd_s,indsd_s,v_temp,logsum,prob,prob_dc_s,prob_sc,v_dc_s,min_pens,nd_glob,ndlloc,nsloc,scpar,Rafter)
    
USE CSHER_INT
USE UMACH_INT
USE CSVAL_INT
use pchip_module

implicit none
   
type(t_agg),intent(in)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(inout):: gr_wage,net_wage,gr_ben_p,ben_p,min_pens,wage_totax
real(dp),intent(in):: Rafter
real(dp),intent(in):: ass
integer,intent(in):: ic,gc,tc,jcm1,tcm1,dc_vec(:),jc,ec,nd_glob,ndlloc,hc,nsloc,scpar
real(dp),intent(inout)::lab_dc_s(:,:),valsd_s(:,:,:,:,:,:),v_temp(:),prob(:),prob_dc_s(:,:,:,:,:),v_dc_s(:,:),prob_sc(:)
integer,intent(inout):: indsd_s(:,:,:,:,:,:)
integer:: dc,dc_temp,dc_full,dc_loc,sc,pc,ickid,gckid,gen_id,yc,epsc,kc
real(dp):: v,logsum,prob_empret(2),v_empret(2),logsum_emp,logsum_s(ns),csbreak_sh(nx),cscoeff_sh(4,nx),explogsum_s(ns),prob_col(2),prob_fin(2),prob_fin_hs(2),val_col,ev_kid_choice(2),probfe(nk),tmp
real(dp):: coh
real(dp):: leis,lab,temp,coh_test,av_prod,v_lin,fe(1),xe(1)
real(dp),parameter::epsi=1.0e-06
logical:: ind_nonnan(nsloc),skip
integer:: ierr 

pc=1
gckid = 1
ickid = 1
gen_id = 2

! compute cash-on-hand
gr_wage=0.0_dp
net_wage=0.0_dp

ben_p=0.0_dp

!ass = ass/Rafter

explogsum_s = 0.0_dp

do sc = 1,nsloc
    
    if (nk>1)then
        probfe(nk) = f_probfe(grid%hk_grid(hc),sc,agg%gammah_s(sc,tc))
        probfe(1) = 1.0_dp -probfe(nk) 
    else
        probfe(1) = 1.0_dp
    endif
    
    do kc=1,nk
        
    
        do yc = 1,ny
            do epsc = 1,nw
        
                do dc_temp=1,size(dc_vec)
        
                    dc=dc_vec(dc_temp)
    
                    if (ec==1)then
            
                        if (dc<=ndlloc)then
            
                            ! THEY ALL WORK AT NO WAGES
                            if (sc<ns-1)then
                                call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(gc,jc,sc),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),sc,kc,dc,ec,jc,gen_id,agg%gammah_s(sc,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                            else
                                call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(1,tc),demo%ageprod(gc,jc,1),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),1,kc,dc,ec,jc,gen_id,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                            endif
                            
                            gr_wage = gr_wage * demo%grid_epsi(epsc)
                            net_wage = net_wage * demo%grid_epsi(epsc)
                            wage_totax = wage_totax * demo%grid_epsi(epsc)
                        
                            dc_loc = dc
           
                            coh = ass + agg%tr_sj(sc,js,tc) +  net_wage
                
                            !if (coh<grid%coh_child(dc,1,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc)  )then
                            !    
                            !    v = - 90000.0_dp
                            !    v_lin = v
                            !else
                    
          
                            ! interpolation: to determine values and weights 
                            lab_dc_s(sc,dc) = func_intp(grid%coh_guess(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,tc),pol%lab(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),coh, & 
                                valsd_s(sc,dc,kc,yc,epsc,:),indsd_s(sc,dc,kc,yc,epsc,:),.false.,.false.)
                            !v=func_intp(grid%coh_guess(dc,:,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,tc),pol%v_guess(dc,:,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,tc),coh, & 
                            !    valsd_s(sc,dc,:),indsd_s(sc,dc,:) ,.false.,.false.)
              
                            v=func_intp_mod(grid%coh_child(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,js,tc),pol%v_child(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc),coh, & 
                                valsd_s(sc,dc,kc,yc,epsc,:),indsd_s(sc,dc,kc,yc,epsc,:) ) !,.false.,.false.)
                    
                            v_lin = v
                            
                            tmp = func_intp(grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,js,tc),grid%ass(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc),ass, & 
                                valsd_s(sc,dc,kc,yc,epsc,:),indsd_s(sc,dc,kc,yc,epsc,:),.false.,.false.)
                    
                            !csbreak_sh = grid%break_sh_child(dc,:,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,js,tc)
                            !cscoeff_sh = grid%coeff_sh_child(:,dc,:,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,js,tc)
                            !
                            !v =  d_csval(coh,csbreak_sh,cscoeff_sh)
                            if (abs(sum(valsd_s(sc,dc,kc,yc,epsc,:))-1.0_dp)>epsi)then
                                print*, "bad vals", valsd_s(sc,dc,kc,yc,epsc,:)
                            endif
                            
                    
                            xe(1) = coh
                            skip = .true.
                    
                    
                            !if (grid%coh_child(dc,np1_aux-1,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc) == grid%coh_child(dc,np1_aux-2,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc) )then
                            !        
                            !    call dpchfe (nx-np1_aux+1, grid%coh_child(dc,np1_aux:nx,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,js,tc), & 
                            !        pol%v_child(dc,np1_aux:nx,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc), pol%vder_child(dc,np1_aux:nx,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc), & 
                            !        1, skip, 1, xe, fe, ierr)
                            !
                            !else
                            !
                            !    call dpchfe (nx, grid%coh_child(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,js,tc), & 
                            !        pol%v_child(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc), pol%vder_child(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,1,scpar,js,tc), & 
                            !        1, skip, 1, xe, fe, ierr)
                            !
                            !endif
                            !
                            !            
                            !v = fe(1)
                        
                          !  endif
                
                            !if (abs(v) > 9000.0_dp) then
                            !    print*, "infeasible child val!!",sc
                            !    !pause
                            !endif
                            
                        else
                            print*, "should not be here, hm1",tc,jc,ic
                            pause
             
                        endif 
    
                else ! ec=2
       
                    print*, "should not be here TOO, hm1",tc,jc,ic
                    pause
          
            
                endif
            
                v_temp(dc_temp)=v
       
                if ( (sum(valsd_s(sc,dc,kc,yc,epsc,:) )-1.0_dp )>epsi)then
                    print*, "wrong interpolation values3", valsd_s(sc,dc,kc,yc,epsc,:)
                    pause
                endif
                        
            enddo

            do dc_temp=1,size(dc_vec)
                if (isnan(v_temp(dc_temp)))then
                    ind_nonnan(dc_temp) = .false.
                else
                     ind_nonnan(dc_temp) = .true.
                endif
            enddo
   
            if (size(dc_vec) == ndl+1)then

                call sub_logsumprob(v_temp(1:ndl),logsum_emp,prob(1:ndl),nd_glob-1,sigma_emp)
                v_empret(1)=logsum_emp
                v_empret(2)=v_temp(ndl+1)
                call sub_logsumprob(v_empret,logsum,prob_empret,2,sigma_ret)
                do dc_temp=1,ndl
                    prob(dc_temp) = prob(dc_temp)*prob_empret(1)
                enddo
                prob(ndl+1) = prob_empret(2)

            else
    
        
                call sub_logsumprob(v_temp,logsum_s(sc),prob,nd_glob,sigma_emp)
            endif

            prob_dc_s(sc,kc,yc,epsc,:)=0.0_dp
        
            do dc_temp=1,size(dc_vec)
                dc=dc_vec(dc_temp) 
             
                do dc_full = 1,nd
                    if (dc==dc_full)then
                        prob_dc_s(sc,kc,yc,epsc,dc_full)=prob(dc_temp)
                        v_dc_s(sc,dc_full) = v_temp(dc_temp)
                    endif
        
                  !  if (v_temp(dc_full)<-7000.0_dp) v_temp(dc_full) = sqrt(-1.0_dp)
                enddo
            enddo
            
            if (abs(1.0_dp - sum(prob_dc_s(sc,kc,yc,epsc,:)))> epsi)then
                print*, "dcprob bad", prob_dc_s(sc,kc,yc,epsc,:)
            endif
            
    
            explogsum_s(sc) = explogsum_s(sc) + logsum_s(sc) * demo%pini(yc) * demo%prob_epsi(epsc) * probfe(kc)
    
            enddo
        enddo
    enddo

enddo ! sc

do sc=1,nsloc
    if (abs(logsum_s(sc)) > 9000.0_dp) then
        ind_nonnan(sc) = .false.
    else
        ind_nonnan(sc) = .true.
    endif
enddo

if (any(ind_nonnan))then
    ! all fine
else
   ! if (jc>17)then
    print*, "only nans child"
    pause
   ! endif
endif

if (opt_corr==1)then

! first DROPOUT DECISION, with taste shock
call sub_logsumprob(explogsum_s(ns-1:ns),logsum,prob_col,2,sigma_emp)

! now, to those willing to graduate, apply lottery
prob_fin(1) = f_probfin(grid%hk_grid(hc)) ! CL completion prob
prob_fin(2) = 1.0_dp - prob_fin(1) ! CL dropout prob

prob_fin(1) = prob_col(2) * prob_fin(1)
prob_fin(2) = 1.0_dp - prob_fin(1)

else

    ! first DROPOUT DECISION, with taste shock
    call sub_logsumprob(explogsum_s(ns-1:ns),logsum,prob_col,2,sigma_emp*0.0_dp)
    ! now, to those willing to graduate, apply lottery
    prob_fin(1) = f_probfin(grid%hk_grid(hc)) ! CL completion prob
    prob_fin(2) = 1.0_dp - prob_fin(1) ! CL dropout prob

    prob_fin(1) = prob_col(2) * prob_fin(1)
    prob_fin(2) = 1.0_dp - prob_fin(1)

endif 


prob_fin_hs(1) = f_probfin_hs(grid%hk_grid(hc)) ! HS completion prob
prob_fin_hs(2) = 1.0_dp - prob_fin_hs(1) ! HS dropout prob
val_col = prob_fin(1) * explogsum_s(ns) + prob_fin(2) * explogsum_s(ns-1) ! value of choosing college
prob_col = 0.0_dp
            
ev_kid_choice(1)= explogsum_s(2) ! not choose CL => end up as HS graduate
ev_kid_choice(2) = val_col ! choose CL =>  get val col
 
! compute prob of choosing college
call sub_logsumprob(ev_kid_choice,logsum,prob_col,2,sigma_emp)

prob_sc(1)=prob_fin_hs(2) ! HS dropout prob
prob_sc(2)=prob_fin_hs(1) *prob_col(1) ! HS graduate, choose NO CL
prob_sc(3)=prob_fin_hs(1) *prob_col(2) * prob_fin(2) ! HS graduate, choose CL, but dropout
prob_sc(4)=prob_fin_hs(1) *prob_col(2) * prob_fin(1) ! HS graduate, choose CL, finish CL

!print*, logsum_s(1:nsloc)
!pause

end subroutine sub_prob_dc_new_kid
! ---------------------------------------------------------------------------------



! ---------------------------------------------------------------------------------
subroutine sub_prob_dc_new_out(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,ec, &
    kc,hc,yc,epsc,ic,sc,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,logsum,prob,prob_dc,v_dc,min_pens,nd_glob,ndlloc,pc,ickid,gckid,gen_id)
    
implicit none
   
type(t_agg),intent(in)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(inout):: gr_wage,net_wage,gr_ben_p,ben_p,min_pens,wage_totax
real(dp),intent(in):: ass
integer,intent(in):: ic,sc,gc,tc,jcm1,tcm1,dc_vec(:),yc,epsc,jc,ec,kc,nd_glob,ndlloc,pc,ickid,gckid,hc,gen_id
real(dp),intent(inout)::lab_dc(:),valsd(:,:),v_temp(:),prob(:),prob_dc(:),v_dc(:)
integer,intent(inout):: indsd(:,:)
integer:: dc,dc_temp,dc_full,dc_loc
real(dp):: v,logsum,prob_empret(2),v_empret(2),logsum_emp
real(dp):: coh
real(dp):: leis,lab,temp,coh_test,av_prod
real(dp),parameter::epsi=1.0e-06
logical:: ind_nonnan(size(v_temp))
! compute cash-on-hand
gr_wage=0.0_dp
net_wage=0.0_dp

ben_p=0.0_dp

do dc_temp=1,size(dc_vec)
        
    dc=dc_vec(dc_temp)
    
    if (ec==1)then
            
        if (dc<=ndlloc)then
            
            if ( sc>2 .and. jc<jstud(sc) )then
          
                call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(1,tc),demo%ageprod(gc,jc,1),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),1,kc,dc,ec,jc,gen_id,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                
            else
                
                call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(gc,jc,sc),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),sc,kc,dc,ec,jc,gen_id,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
                
            endif
            
            gr_wage = gr_wage * demo%grid_epsi(epsc)
            net_wage = net_wage * demo%grid_epsi(epsc)
            wage_totax = wage_totax * demo%grid_epsi(epsc)
           
            
            dc_loc = dc
           
            coh = f_coh(ass,agg%tr_sj(sc,jc,tc),1.0_dp+agg%ret(tc),net_wage,0.0_dp,ec,dc,ic,sc,agg%tau_k(tc) )
            
            
            ! interpolation: to determine values and weights 
            lab_dc(dc) = func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%lab(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),coh, & 
                valsd(dc,:),indsd(dc,:),.false.,.false.)
            v=func_intp_nan(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),coh, & 
                valsd(dc,:),indsd(dc,:) ) !  ,.false.,.false.)
            
        elseif (dc==ndlloc+1)then
            print*, "should not be here, hm1",tc,jc,ic
            pause
                
          
        elseif (dc==nd)then
          
            lab_dc(dc)=0.0_dp
            av_prod = demo%ageprod(gc,jr(tc)-1,sc )  
            call sub_pens_new(gr_ben_p,agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc,tc), &
                av_prod ,demo%grid_y(yc),0.0_dp,jc,gc,dc,ec,tc,sc,kc,yc,nd)
            ben_p = fun_netpens(gr_ben_p,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
          
           ! coh=coh_in+ben_p        
            coh = f_coh(ass,agg%tr_sj(sc,jc,tc),1.0_dp+agg%ret(tc),0.0_dp,ben_p,ec,dc,ic,sc,agg%tau_k(tc) )
            
            v=func_intp_nan(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),coh, & 
                valsd(dc,:),indsd(dc,:) ) ! ,.false.,.false.)   
                
           
        endif
    
    else ! ec=2
       
        lab_dc(dc)=0.0_dp
        av_prod = demo%ageprod(gc,jr(tc)-1,sc )  
        call sub_pens_new(gr_ben_p,agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc,tc), &
            av_prod ,demo%grid_y(yc),0.0_dp,jc,gc,dc,ec,tc,sc,kc,yc,nd)
           
        ben_p = fun_netpens(gr_ben_p,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
            
        if (isnan(ben_p))then
            ben_p = 0.0_dp
        endif
            
                            
        ! coh=coh_in+ben_p        
        coh = f_coh(ass,agg%tr_sj(sc,jc,tc),1.0_dp+agg%ret(tc),0.0_dp,ben_p,ec,dc,ic,sc,agg%tau_k(tc) )
            
        v=func_intp(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),coh, & 
            valsd(dc,:),indsd(dc,:),.false.,.false.)  
          
            
    endif
            
    v_temp(dc_temp)=v
       
    if ( (sum(valsd(dc,:) )-1.0_dp )>epsi)then
        print*, "wrong interpolation values5"
        pause
    endif
                        
enddo

do dc_temp=1,size(dc_vec)
    if (isnan(v_temp(dc_temp)))then
        ind_nonnan(dc_temp) = .false.
    else
         ind_nonnan(dc_temp) = .true.
    endif
enddo
!if (any(ind_nonnan))then
!    ! all fine
!else
!   ! if (jc>17)then
!    print*, "only nans", v_temp,tc,jc,ic,grid%ass(1:3,1,1,1,1,ic,gc,1,sc,jc,tc),coh_in,ic,f_coh(grid%ass(1,1,1,1,1,ic,gc,1,sc,jc,tc),agg%tr_isj(ic,sc,jc,tc),1.0_dp+agg%ret(tc),net_wage,ben_p,ec,dc,ic,agg%tau_k(tc)),coh
!    pause
!   ! endif
!endif


if (size(dc_vec) == ndl+1)then

    call sub_logsumprob(v_temp(1:ndl),logsum_emp,prob(1:ndl),nd_glob-1,sigma_emp)
    v_empret(1)=logsum_emp
    v_empret(2)=v_temp(ndl+1)
    call sub_logsumprob(v_empret,logsum,prob_empret,2,sigma_ret)
    do dc_temp=1,ndl
        prob(dc_temp) = prob(dc_temp)*prob_empret(1)
    enddo
    prob(ndl+1) = prob_empret(2)

else
    
        
    call sub_logsumprob(v_temp,logsum,prob,nd_glob,sigma_emp)
endif

prob_dc=0.0_dp
        
do dc_temp=1,size(dc_vec)
    dc=dc_vec(dc_temp)
            
    do dc_full = 1,nd
        if (dc==dc_full)then
            prob_dc(dc_full)=prob(dc_temp)
            v_dc(dc_full) = v_temp(dc_temp)
        endif
        
      !  if (v_temp(dc_full)<-7000.0_dp) v_temp(dc_full) = sqrt(-1.0_dp)
    enddo
enddo

end subroutine sub_prob_dc_new_out
! ---------------------------------------------------------------------------------
    
! ---------------------------------------------------------------------------------
subroutine sub_prob_dc_new_out_cpl(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,ec, &
    kc1,kc2,yc1,yc2,epsc1,epsc2,sc1,sc2,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,v_temp,logsum,prob,prob_dc,v_dc,min_pens,nd_glob,ndlloc,pc)
    
implicit none
   
type(t_agg),intent(in)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(inout):: gr_wage(:),net_wage(:),gr_ben_p(:),ben_p(:),min_pens,wage_totax(:)
real(dp),intent(in):: ass
integer,intent(in):: sc1,sc2,tc,jcm1,tcm1,dc_vec(:),yc1,yc2,epsc1,epsc2,jc,ec,kc1,kc2,nd_glob,ndlloc,pc
real(dp),intent(inout)::lab_dc(:),valsd(:,:),v_temp(:),prob(:),prob_dc(:),v_dc(:)
integer,intent(inout):: indsd(:,:)
integer:: dc,dc_temp,dc_full,dc_loc,gc,yc(2),kc(2),sc(2),epsc(2)
real(dp):: v,logsum,prob_empret(2),v_empret(2),logsum_emp
real(dp):: coh
real(dp):: leis,lab,temp,coh_test,av_prod
real(dp),parameter::epsi=1.0e-06
logical:: ind_nonnan(size(v_temp))
! compute cash-on-hand
gr_wage=0.0_dp
net_wage=0.0_dp

ben_p=0.0_dp

kc(1) = kc1
kc(2) = kc2
sc(1) = sc1
sc(2) = sc2
yc(1) = yc1
yc(2) = yc2
epsc(1) = epsc1
epsc(2) = epsc2

do dc_temp=1,size(dc_vec)
        
    dc=dc_vec(dc_temp)
    
    if (ec==1)then
            
        if (dc<=ndlloc)then
            
            do gc = 1,ng    
            
                call sub_wage(gr_wage(gc),net_wage(gc),wage_totax(gc),agg%wage_s(sc(gc),tc),demo%ageprod(gc,jc,sc(gc)),demo%grid_y(yc(gc)),grid%hk_grid(1), & 
                    agg%tau_p(tc),sc(gc),kc(gc),dc,ec,jc,1,agg%gammah_s(sc(gc),tc),agg%abgrad_s(:,sc(gc)),agg%hk_cutoff(:,tc))
            
                gr_wage(gc) = gr_wage(gc) * demo%grid_epsi(epsc(gc))
                net_wage(gc) = net_wage(gc) * demo%grid_epsi(epsc(gc))
                wage_totax(gc) = wage_totax(gc) * demo%grid_epsi(epsc(gc))
           
            enddo
            
            dc_loc = dc
           
            coh = f_coh_cpl(ass,agg%tr_sj(sc1,jc,tc)+agg%tr_sj(sc2,jc,tc),1.0_dp+agg%ret(tc),net_wage,(/0.0_dp,0.0_dp/),ec,dc,1,agg%tau_k(tc) )
            
            
            ! interpolation: to determine values and weights 
            lab_dc(dc) = func_intp(grid%coh_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),coh, & 
                valsd(dc,:),indsd(dc,:),.false.,.false.)
            v=func_intp_nan(grid%coh_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),pol%v_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),coh, & 
                valsd(dc,:),indsd(dc,:) ) !  ,.false.,.false.)
            
        elseif (dc==ndlloc+1)then
            print*, "should not be here, hm1",tc,jc
            pause
                
          
        elseif (dc==nd)then
          
            print*, "reminder: endo retirement to be updated"
           ! lab_dc(dc)=0.0_dp
           ! av_prod = demo%ageprod(gc,jr(tc)-1,sc )  
           ! call sub_pens_new(gr_ben_p,agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc,tc), &
           !     av_prod ,demo%grid_y(yc),0.0_dp,jc,gc,dc,ec,tc,sc,kc,yc,nd)
           ! ben_p = fun_netpens(gr_ben_p,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
           !
           !! coh=coh_in+ben_p        
           ! coh = f_coh(ass,agg%tr_sj(sc,jc,tc),1.0_dp+agg%ret(tc),0.0_dp,ben_p,ec,dc,ic,agg%tau_k(tc) )
           ! 
           ! v=func_intp_nan(grid%coh(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%v(dc,:,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),coh, & 
           !     valsd(dc,:),indsd(dc,:) ) ! ,.false.,.false.)   
                
           
        endif
    
    else ! ec=2
       
        lab_dc(dc)=0.0_dp
        do gc = 1,ng
        
            av_prod = demo%ageprod(gc,jr(tc)-1,sc(gc) )  
            call sub_pens_new(gr_ben_p(gc),agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc(gc),tc), &
                av_prod ,demo%grid_y(yc(gc)),0.0_dp,jc,gc,dc,ec,tc,sc(gc),kc(gc),yc(gc),nd)
           
            ben_p(gc) = fun_netpens(gr_ben_p(gc),0.0_dp,0.0_dp,2,gc,ec,dc,1,jc,tc,avearn) 
            
            if (isnan(ben_p(1)))then
                ben_p = 0.0_dp
            endif
            
        enddo
        
        ! coh=coh_in+ben_p        
        coh = f_coh_cpl(ass,agg%tr_sj(sc1,jc,tc) + agg%tr_sj(sc2,jc,tc),1.0_dp+agg%ret(tc),(/0.0_dp,0.0_dp/),ben_p,ec,dc,1,agg%tau_k(tc) )
            
        v=func_intp(grid%coh_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),pol%v_cpl(dc,:,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),coh, & 
            valsd(dc,:),indsd(dc,:),.false.,.false.)  
          
            
    endif
            
    v_temp(dc_temp)=v
       
    if ( (sum(valsd(dc,:) )-1.0_dp )>epsi)then
        print*, "wrong interpolation values5"
        pause
    endif
                        
enddo

do dc_temp=1,size(dc_vec)
    if (isnan(v_temp(dc_temp)))then
        ind_nonnan(dc_temp) = .false.
    else
         ind_nonnan(dc_temp) = .true.
    endif
enddo
!if (any(ind_nonnan))then
!    ! all fine
!else
!   ! if (jc>17)then
!    print*, "only nans", v_temp,tc,jc,ic,grid%ass(1:3,1,1,1,1,ic,gc,1,sc,jc,tc),coh_in,ic,f_coh(grid%ass(1,1,1,1,1,ic,gc,1,sc,jc,tc),agg%tr_isj(ic,sc,jc,tc),1.0_dp+agg%ret(tc),net_wage,ben_p,ec,dc,ic,agg%tau_k(tc)),coh
!    pause
!   ! endif
!endif


if (size(dc_vec) == ndl+1)then

    call sub_logsumprob(v_temp(1:ndl),logsum_emp,prob(1:ndl),nd_glob-1,sigma_emp)
    v_empret(1)=logsum_emp
    v_empret(2)=v_temp(ndl+1)
    call sub_logsumprob(v_empret,logsum,prob_empret,2,sigma_ret)
    do dc_temp=1,ndl
        prob(dc_temp) = prob(dc_temp)*prob_empret(1)
    enddo
    prob(ndl+1) = prob_empret(2)

else
    
        
    call sub_logsumprob(v_temp,logsum,prob,nd_glob,sigma_emp)
endif

prob_dc=0.0_dp
        
do dc_temp=1,size(dc_vec)
    dc=dc_vec(dc_temp)
            
    do dc_full = 1,nd
        if (dc==dc_full)then
            prob_dc(dc_full)=prob(dc_temp)
            v_dc(dc_full) = v_temp(dc_temp)
        endif
        
      !  if (v_temp(dc_full)<-7000.0_dp) v_temp(dc_full) = sqrt(-1.0_dp)
    enddo
enddo

end subroutine sub_prob_dc_new_out_cpl
! ---------------------------------------------------------------------------------
     
    
    
! ---------------------------------------------------------------------------------
subroutine sub_prob_dc_new_out_2dim(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,dcm1,ecm1,ycm1,epscm1,icm1,ec, &
    kc,hc,yc,epsc,ic,sc,gc,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,valsx,indsx,valsh,indsh,v_temp,prob,prob_dc,v_dc,min_pens,nd_glob,ndlloc,xc,pc,ickid,gckid,ickidm1,gckidm1)
    
implicit none
   
type(t_agg),intent(in)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(inout):: gr_wage,net_wage,gr_ben_p,ben_p,min_pens,wage_totax
real(dp),intent(in):: ass
integer,intent(in):: ic,sc,gc,tc,jcm1,tcm1,dc_vec(:),yc,epsc,jc,ec,kc,nd_glob,ndlloc,xc,pc,ickid,gckid,dcm1,ecm1,ycm1,icm1,ickidm1,gckidm1,epscm1,hc
real(dp),intent(inout):: lab_dc(:),valsd(:,:),valsx(:,:,:),valsh(:,:),v_temp(:),prob(:),prob_dc(:),v_dc(:)
integer,intent(inout):: indsd(:,:),indsx(:,:,:),indsh(:,:)
integer:: dc,dc_temp,dc_full,dc_loc,indsx_test(nd,2,2),indsh_test(nd,2)
real(dp):: v,logsum,prob_empret(2),v_empret(2),logsum_emp,hk_kid,m_intp,t1_intp,valsx_test(nd,2,2),valsh_test(nd,2),inv_ces,grinc_dc(ndl)
real(dp):: coh
real(dp):: leis,lab,temp,coh_test,av_prod,v_test
real(dp),parameter::epsi=1.0e-06
logical:: ind_nonnan(size(v_temp))


if (tcm1==1)then
    opt_trref_hk = 0
else
    opt_trref_hk = opt_trref
endif

! compute cash-on-hand
gr_wage=0.0_dp
net_wage=0.0_dp

ben_p=0.0_dp

do dc_temp=1,size(dc_vec)
        
    dc=dc_vec(dc_temp)
    
    if (ec==1)then
            
        if (dc<=ndlloc)then
          
            call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(gc,jc,sc),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),sc,kc,dc,ec,jc,1,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
            
            gr_wage = gr_wage * demo%grid_epsi(epsc)
            net_wage = net_wage * demo%grid_epsi(epsc)
            wage_totax = wage_totax * demo%grid_epsi(epsc)
            
            dc_loc = dc
           
            coh = f_coh(ass,agg%tr_sj(sc,jc,tc),1.0_dp+agg%ret(tc),net_wage,0.0_dp,ec,dc,ic,sc,agg%tau_k(tc) )
            
            if (jc>jf)then
                
                if (opt_inv ==1)then
                
                    
                    hk_kid = f_hh_tp1(grid%hk_grid(pc),1.0_dp,pol%m(dcm1,xc,pc,ycm1,ecm1,epscm1,gc,kc,hc,sc,ickidm1,gckidm1,jcm1,tcm1), & 
                        pol%t1(dcm1,xc,pc,ycm1,ecm1,icm1,gc,kc,hc,sc,ickidm1,gckidm1,jcm1,tcm1),jcm1-jf+1,inv_ces)
                      
                else
                   
                    !if (jcm1==jf)then
                    !    hk_kid =grid%h0_grid(ickidm1)
                    !else
                        
                        hk_kid = grid%hk_grid(pc)
                    !endif
                    
                    
                    
                endif
                
            else
                print*, "2dim routine is called only after age jf!"
                pause
            endif
            
            if (isnan(hk_kid))then
                print*, "hk kid is nan", grid%hk_grid(pc),1.0_dp,m_intp,t1_intp,coh
                pause
            endif 
            
                
            
            !print*, "so", jc,coh,hk_kid
            !pause
            
            lab_dc(dc) = f_hyb2d_inter(coh,hk_kid,grid%coh(dc,:,:,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),grid%hk_grid,pol%lab(dc,:,:,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc) )
            
            grinc_dc(dc)= gr_wage * lab_dc(dc)
            !f_hyb2d_inter(x,y,grid_x,grid_y,fun_xy)
            !vals_2x,inds_2x,vals_y,inds_y)
            
            v=f_hyb2d_inter_mod(coh,hk_kid,grid%coh(dc,:,:,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),grid%hk_grid,pol%v(dc,:,:,yc,ec,epsc,gc,kc,hc,sc,ickid,gckid,jc,tc),valsx(dc,:,:),indsx(dc,:,:),valsh(dc,:),indsh(dc,:) )
           
            
            if ( (sum(valsx(dc,:,1) )-1.0_dp )>epsi)then
                print*, "wrong interpolation values x",valsx(dc,1,:)
                pause
            endif
          
            if ( (sum(valsh(dc,:) )-1.0_dp )>epsi)then
                print*, "wrong interpolation values h",valsx(dc,1,:)
                pause
            endif
            
            
        else
            print*, "should not be here, hmp1",tc,jc,ic
            pause
                
         
        endif
    
    else ! ec=2
       
        print*, "should not be here EITHER",tc,jc,ic
        pause
            
    endif
            
    v_temp(dc_temp)=v
    
    
       
    if ( (sum(valsd(dc,:) )-1.0_dp )>epsi)then
        print*, "wrong interpolation values7"
        pause
    endif
                        
enddo

!print*, "soooo", v_temp
!pause

do dc_temp=1,size(dc_vec)
    if (isnan(v_temp(dc_temp)))then
        ind_nonnan(dc_temp) = .false.
    else
         ind_nonnan(dc_temp) = .true.
    endif
enddo


if (size(dc_vec) == ndl+1)then

    call sub_logsumprob(v_temp(1:ndl),logsum_emp,prob(1:ndl),nd_glob-1,sigma_emp)
    v_empret(1)=logsum_emp
    v_empret(2)=v_temp(ndl+1)
    call sub_logsumprob(v_empret,logsum,prob_empret,2,sigma_ret)
    do dc_temp=1,ndl
        prob(dc_temp) = prob(dc_temp)*prob_empret(1)
    enddo
    prob(ndl+1) = prob_empret(2)

else
    
        
    call sub_logsumprob(v_temp,logsum,prob,nd_glob,sigma_emp)
endif

prob_dc=0.0_dp
        
do dc_temp=1,size(dc_vec)
    dc=dc_vec(dc_temp)
            
    do dc_full = 1,nd
        if (dc==dc_full)then
            prob_dc(dc_full)=prob(dc_temp)
            v_dc(dc_full) = v_temp(dc_temp)
        endif
        
      !  if (v_temp(dc_full)<-7000.0_dp) v_temp(dc_full) = sqrt(-1.0_dp)
    enddo
enddo

opt_trref_hk = opt_trref

end subroutine sub_prob_dc_new_out_2dim
! ---------------------------------------------------------------------------------    
    
! ---------------------------------------------------------------------------------
subroutine sub_prob_dc_new_out_2dim_cpl(agg,demo,grid,pol,ass,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,dcm1,ecm1,yc1m1,yc2m1,epsc1m1,epsc2m1,ec, &
    kc1,kc2,yc1,yc2,epsc1,epsc2,sc1,sc2,jc,tc,jcm1,tcm1,dc_vec,lab_dc,valsd,indsd,valsx,indsx,valsh,indsh,v_temp,prob,prob_dc,v_dc,min_pens,nd_glob,ndlloc,xc,pc)
    
implicit none
   
type(t_agg),intent(in)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(inout):: gr_wage(:),net_wage(:),gr_ben_p(:),ben_p(:),min_pens,wage_totax(:)
real(dp),intent(in):: ass
integer,intent(in):: sc1,sc2,tc,jcm1,tcm1,dc_vec(:),yc1,yc2,epsc1,epsc2,jc,ec,kc1,kc2,nd_glob,ndlloc,xc,pc,dcm1,ecm1,yc1m1,yc2m1,epsc1m1,epsc2m1
real(dp),intent(inout):: lab_dc(:),valsd(:,:),valsx(:,:,:),valsh(:,:),v_temp(:),prob(:),prob_dc(:),v_dc(:)
integer,intent(inout):: indsd(:,:),indsx(:,:,:),indsh(:,:)
integer:: dc,dc_temp,dc_full,dc_loc,indsx_test(nd,2,2),indsh_test(nd,2),gc,kc(2),yc(2),sc(2),epsc(2)
real(dp):: v,logsum,prob_empret(2),v_empret(2),logsum_emp,hk_kid,m_intp,t1_intp,valsx_test(nd,2,2),valsh_test(nd,2),inv_ces,grinc_dc(ndl)
real(dp):: coh
real(dp):: leis,lab,temp,coh_test,av_prod,v_test
real(dp),parameter::epsi=1.0e-06
logical:: ind_nonnan(size(v_temp))

kc(1) = kc1
kc(2) = kc2
sc(1) = sc1
sc(2) = sc2
yc(1) = yc1
yc(2) = yc2
epsc(1) = epsc1
epsc(2) = epsc2

if (tcm1==1)then
    opt_trref_hk = 0
else
    opt_trref_hk = opt_trref
endif


! compute cash-on-hand
gr_wage=0.0_dp
net_wage=0.0_dp

ben_p=0.0_dp

do dc_temp=1,size(dc_vec)
        
    dc=dc_vec(dc_temp)
    
    if (ec==1)then
            
        if (dc<=ndlloc)then
          
            do gc = 1,ng
            
                call sub_wage(gr_wage(gc),net_wage(gc),wage_totax(gc),agg%wage_s(sc(gc),tc),demo%ageprod(gc,jc,sc(gc)),demo%grid_y(yc(gc)),grid%hk_grid(1), & 
                    agg%tau_p(tc),sc(gc),kc(gc),dc,ec,jc,1,agg%gammah_s(sc(gc),tc),agg%abgrad_s(:,sc(gc)),agg%hk_cutoff(:,tc))
            
                gr_wage(gc) = gr_wage(gc) * demo%grid_epsi(epsc(gc))
                net_wage(gc) = net_wage(gc) * demo%grid_epsi(epsc(gc))
                wage_totax(gc) = wage_totax(gc) * demo%grid_epsi(epsc(gc))
                
            enddo
            
            
            dc_loc = dc
           
            coh = f_coh_cpl(ass,agg%tr_sj(sc1,jc,tc) + agg%tr_sj(sc2,jc,tc),1.0_dp+agg%ret(tc),net_wage,(/0.0_dp,0.0_dp/),ec,dc,1,agg%tau_k(tc) )
            
            if (jc>jf)then
                
                if (opt_inv ==1)then
                
                    if (opt_corr==1)then 
                        hk_kid = f_hh_tp1(grid%hk_grid(pc),1.0_dp,pol%m_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1), & 
                            pol%t1_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1)+pol%t2_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1),jcm1-jf+1,inv_ces)
                    else
                        hk_kid = f_hh_tp1(grid%hk_grid(pc),1.0_dp,pol%m_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1), & 
                            pol%t1_cpl(dcm1,xc,pc,yc1m1,yc2m1,ecm1,epsc1m1,epsc2m1,kc1,kc2,sc1,sc2,jcm1,tcm1),jcm1-jf+1,inv_ces)
                    endif
                    
                else
                   
                    !if (jcm1==jf)then
                    !    hk_kid =grid%h0_grid(ickidm1)
                    !else
                        
                        hk_kid = grid%hk_grid(pc)
                    !endif
                    
                    
                    
                endif
                
            else
                print*, "2dim routine is called only after age jf!"
                pause
            endif
            
            if (isnan(hk_kid))then
                print*, "hk kid is nan", grid%hk_grid(pc),1.0_dp,m_intp,t1_intp,coh
                pause
            endif 
            
                
            
            !print*, "so", jc,coh,hk_kid
            !pause
            
            lab_dc(dc) = f_hyb2d_inter(coh,hk_kid,grid%coh_cpl(dc,:,:,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc), & 
                grid%hk_grid,pol%lab1_cpl(dc,:,:,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) )
            
            grinc_dc(dc)= gr_wage(1) * lab_dc(dc)
            !f_hyb2d_inter(x,y,grid_x,grid_y,fun_xy)
            !vals_2x,inds_2x,vals_y,inds_y)
            
            v=f_hyb2d_inter_mod(coh,hk_kid,grid%coh_cpl(dc,:,:,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),grid%hk_grid, & 
                pol%v_cpl(dc,:,:,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),valsx(dc,:,:),indsx(dc,:,:),valsh(dc,:),indsh(dc,:) )
           
            
            if ( (sum(valsx(dc,:,1) )-1.0_dp )>epsi)then
                print*, "wrong interpolation values x",valsx(dc,1,:)
                pause
            endif
          
            if ( (sum(valsh(dc,:) )-1.0_dp )>epsi)then
                print*, "wrong interpolation values h",valsx(dc,1,:)
                pause
            endif
            
            
        else
            print*, "should not be here, hmp1",tc,jc
            pause
                
         
        endif
    
    else ! ec=2
       
        print*, "should not be here EITHER cpl",tc,jc
        pause
            
    endif
            
    v_temp(dc_temp)=v
    
    
       
    if ( (sum(valsd(dc,:) )-1.0_dp )>epsi)then
        print*, "wrong interpolation values7"
        pause
    endif
                        
enddo

!print*, "soooo", v_temp
!pause

do dc_temp=1,size(dc_vec)
    if (isnan(v_temp(dc_temp)))then
        ind_nonnan(dc_temp) = .false.
    else
         ind_nonnan(dc_temp) = .true.
    endif
enddo


if (size(dc_vec) == ndl*ndl+1)then

    call sub_logsumprob(v_temp(1:ndl*ndl),logsum_emp,prob(1:ndl*ndl),nd_glob-1,sigma_emp)
    v_empret(1)=logsum_emp
    v_empret(2)=v_temp(ndl*ndl+1)
    call sub_logsumprob(v_empret,logsum,prob_empret,2,sigma_ret)
    do dc_temp=1,ndl*ndl
        prob(dc_temp) = prob(dc_temp)*prob_empret(1)
    enddo
    prob(ndl+1) = prob_empret(2)

else
    
        
    call sub_logsumprob(v_temp,logsum,prob,nd_glob,sigma_emp)
endif

prob_dc=0.0_dp
        
do dc_temp=1,size(dc_vec)
    dc=dc_vec(dc_temp)
            
    do dc_full = 1,nd_cpl
        if (dc==dc_full)then
            prob_dc(dc_full)=prob(dc_temp)
            v_dc(dc_full) = v_temp(dc_temp)
        endif
        
      !  if (v_temp(dc_full)<-7000.0_dp) v_temp(dc_full) = sqrt(-1.0_dp)
    enddo
enddo

opt_trref_hk = opt_trref

end subroutine sub_prob_dc_new_out_2dim_cpl
! ---------------------------------------------------------------------------------    
 
! ---------------------------------------------------------------------------------
subroutine sub_compudistr_out_2dim(Phi,Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac,indsx,valsx,indsh,valsh, &
    gr_wage_in,net_wage_in,wage_totax_in,gr_ben_p_in,ben_p_in,leis,lab,ec,dc,yc,epsc,kc,hc,sc,ic,gc,jc,tc,tcp1,tcm1,jmax,ndlloc,pc,ickid,gckid,uc,vc,nc,xc_dec)
! compute the distribution, given population fraction [SINGLES]
implicit none
    
real(dp),intent(inout):: Phi(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid_old(:,:,:,:,:,:,:,:,:,:,:,:),agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(in)::frac
real(dp),intent(in)::valsx(:,:),valsh(:),gr_wage_in,net_wage_in,ben_p_in,leis,lab,gr_ben_p_in,wage_totax_in
integer,intent(in):: indsx(:,:),indsh(:)
integer,intent(in):: yc,ec,dc,kc,ic,gc,jc,tc,sc,tcp1,jmax,ndlloc,pc,ickid,gckid,tcm1,xc_dec,epsc,hc
integer,intent(inout)::uc,nc,vc
real(dp):: vals_loc(2)
integer:: inds_loc(2)
real(dp):: ben_u,ben_p,gr_wage,net_wage,ifrac,gr_ben_p,wage_totax,prob_sc_kid(ns),wght_kid,ifrac_aggr
integer:: wc,wcc,sc_kid
    
gr_wage=0.0_dp
net_wage=0.0_dp
gr_ben_p = 0.0_dp
ben_p = 0.0_dp    
wage_totax = 0.0_dp
   
if (ec==1 .and. dc<=ndlloc)then
    gr_wage = gr_wage_in
    net_wage = net_wage_in
    wage_totax = wage_totax_in
    
elseif ( (ec==1 .and. dc==ndlloc+1) )then
       
    print*, "should not be here"
    pause
        
elseif (ec==ne .or. dc==nd)then
       
    ben_p = ben_p_in
    gr_ben_p = gr_ben_p_in
  
endif
     
if ( indsh(1) <1 .or. indsh(1) >np )then
    print*, "this can't be"
    pause
endif

if ( indsh(2) <1 .or. indsh(2) >np )then
    print*, "this can't be2"
    pause
endif

do wc=1,2 ! h
    
    do wcc=1,2
    
        ifrac=valsx(wcc,wc)*valsh(wc)*frac     ! current fraction
       ! ifrac=valsx(wcc,2)*frac
        !print*, indsx(wcc,wc), indsh(wc)
        !pause
    
        ! aggregation, weighting with current fration
        if (jc>=jf-1 .and. opt_cpl_aggr==1)then
           
            ifrac_aggr = ifrac *probmar_s(1,gc,sc) / 0.75_dp
        else
            ifrac_aggr = ifrac    
        endif
        
        call sub_aggr_out(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac_aggr,indsx(wcc,wc),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,demo%sr(jc,tc), &
            ec,dc,yc,epsc,kc,hc,ic,sc,gc,jc,tc,tcp1,indsh(wc),ickid,1,1,1,nc,xc_dec)
        ! add current fraction to distribution: 
        Phi(dc,indsx(wcc,wc),indsh(wc),yc,ec,kc,gc,epsc,hc,sc,ickid,gckid)=Phi(dc,indsx(wcc,wc),indsh(wc),yc,ec,kc,gc,epsc,hc,sc,ickid,gckid)+ifrac
            
        if (jc==jt)then
            !call kid_meas_update(Phi_kid,Phi_kid_old,agg_vec,prob_sc_kid,agg,demo,grid,pol,ifrac_aggr,pol%b(dc,indsx(wcc,wc),indsh(wc),yc,ec,epsc,gc,kc,sc,ickid,gckid,tc),f_aftertaxR(agg%ret(tc) + 1.0_dp,agg%tau_k(tc)),gc,sc,indsx(wcc,wc),yc,epsc,kc,indsh(wc),ickid,gckid,tc,tcm1,tcp1,uc,vc,xc_dec)
            
            !if (opt_trref==1)then
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns) *( ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp  ) *fee_flow_frac*earn_si_lo + 0.0139_dp*earn_si_lo  ) * demo%numchild_gsj(gc,sc,jc,tc)
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns-1) *( ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp ) *fee_flow_frac *0.5_dp * earn_si_lo + 0.0139_dp*0.5_dp*earn_si_lo )* demo%numchild_gsj(gc,sc,jc,tc)            
            !else
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns) * ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp ) *fee_flow_frac *earn_si_lo* demo%numchild_gsj(gc,sc,jc,tc)
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns-1) * ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp  ) *fee_flow_frac *0.5_dp *earn_si_lo* demo%numchild_gsj(gc,sc,jc,tc)    
            !endif
            !agg%educ_matrix(sc,:,tc) = agg%educ_matrix(sc,:,tc) + ifrac * prob_sc_kid(:) 
            !agg%educ_matrix_g(gc,sc,:,tc) = agg%educ_matrix_g(gc,sc,:,tc) + ifrac * prob_sc_kid(:) 
        endif
                        
         
    enddo
    
end do
    
end subroutine sub_compudistr_out_2dim 
! ---------------------------------------------------------------------------------
    
! ---------------------------------------------------------------------------------
subroutine sub_compudistr_out_2dim_cpl(Phi,Phi_kid,Phi_kid_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac,probmar,indsx,valsx,indsh,valsh, &
    gr_wage_in,net_wage_in,wage_totax_in,gr_ben_p_in,ben_p_in,leis,lab,ec,dc,yc1,yc2,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc,tcp1,tcm1,jmax,ndlloc,pc,uc,vc,nc)
! compute the distribution, given population fraction [SINGLES]
implicit none
    
real(dp),intent(inout):: Phi(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid(:,:,:,:,:,:,:,:,:,:,:,:),Phi_kid_old(:,:,:,:,:,:,:,:,:,:,:,:),agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(in)::frac,probmar
real(dp),intent(in)::valsx(:,:),valsh(:),gr_wage_in(:),net_wage_in(:),ben_p_in(:),leis,lab,gr_ben_p_in(:),wage_totax_in(:)
integer,intent(in):: indsx(:,:),indsh(:)
integer,intent(in):: yc1,yc2,ec,dc,kc1,kc2,jc,tc,sc1,sc2,tcp1,jmax,ndlloc,pc,tcm1,epsc1,epsc2
integer,intent(inout)::uc,nc,vc
real(dp):: vals_loc(2)
integer:: inds_loc(2)
real(dp):: ben_u,ben_p(2),gr_wage(2),net_wage(2),ifrac,gr_ben_p(2),wage_totax(2),prob_sc_kid(ns),wght_kid,ifrac_aggr
integer:: wc,wcc,sc_kid
    
gr_wage=0.0_dp
net_wage=0.0_dp
gr_ben_p = 0.0_dp
ben_p = 0.0_dp    
wage_totax = 0.0_dp
   
if (ec==1 .and. dc<=ndlloc)then
    gr_wage = gr_wage_in
    net_wage = net_wage_in
    wage_totax = wage_totax_in
    
elseif ( (ec==1 .and. dc==ndlloc+1) )then
       
    print*, "should not be here"
    pause
        
elseif (ec==ne .or. dc==nd)then
       
    ben_p = ben_p_in
    gr_ben_p = gr_ben_p_in
  
endif
     
if ( indsh(1) <1 .or. indsh(1) >np )then
    print*, "this can't be"
    pause
endif

if ( indsh(2) <1 .or. indsh(2) >np )then
    print*, "this can't be2"
    pause
endif

do wc=1,2 ! h
    
    do wcc=1,2
    
        ifrac=valsx(wcc,wc)*valsh(wc)*frac     ! current fraction
        ifrac_aggr = ifrac * probmar/ 0.75_dp
       ! ifrac=valsx(wcc,2)*frac
        !print*, indsx(wcc,wc), indsh(wc)
        !pause
    
        ! aggregation, weighting with current fration
        call sub_aggr_out_cpl(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac_aggr,probmar,indsx(wcc,wc),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,demo%sr(jc,tc), &
            ec,dc,yc1,yc2,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc,tcp1,indsh(wc),nc)
        ! add current fraction to distribution: 
        Phi(dc,indsx(wcc,wc),indsh(wc),yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2)=Phi(dc,indsx(wcc,wc),indsh(wc),yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2)+ifrac
            
        if (jc==jt)then
            !call kid_meas_update_cpl(Phi_kid,Phi_kid_old,agg_vec,prob_sc_kid,agg,demo,grid,pol,ifrac_aggr,pol%b_cpl(dc,indsx(wcc,wc),indsh(wc),yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc),f_aftertaxR(agg%ret(tc) + 1.0_dp,agg%tau_k(tc)),sc1,sc2,indsx(wcc,wc),yc1,yc2,epsc1,epsc2,kc1,kc2,indsh(wc),tc,tcm1,tcp1,uc,vc)
               !  call kid_meas_update(Phi_kid,Phi_kid_old,agg_vec,prob_sc_kid,agg,demo,grid,pol,ifrac_aggr,pol%b_cpl(dc,indsx(wcc,wc),indsh(wc),yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc),f_aftertaxR(agg%ret(tc) + 1.0_dp,agg%tau_k(tc)),1,max(sc1,sc2),indsx(wcc,wc),yc1,epsc1,kc1,indsh(wc),1,1,tc,tcm1,tcp1,uc,vc,1)
            
            !if (opt_trref==1)then
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns) *( ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp  ) *fee_flow_frac*earn_si_lo + 0.0139_dp*earn_si_lo  ) * demo%numchild_gsj(gc,sc,jc,tc)
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns-1) *( ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp ) *fee_flow_frac *0.5_dp * earn_si_lo + 0.0139_dp*0.5_dp*earn_si_lo )* demo%numchild_gsj(gc,sc,jc,tc)            
            !else
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns) * ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp ) *fee_flow_frac *earn_si_lo* demo%numchild_gsj(gc,sc,jc,tc)
            !   agg%clsubs(tc) = agg%clsubs(tc) + ifrac * prob_sc_kid(ns-1) * ( (1.0_dp/(1.0_dp-0.388_dp-0.166_dp)) * 0.388_dp  ) *fee_flow_frac *0.5_dp *earn_si_lo* demo%numchild_gsj(gc,sc,jc,tc)    
            !endif
            !agg%educ_matrix(sc,:,tc) = agg%educ_matrix(sc,:,tc) + ifrac * prob_sc_kid(:) 
            !agg%educ_matrix_g(gc,sc,:,tc) = agg%educ_matrix_g(gc,sc,:,tc) + ifrac * prob_sc_kid(:) 
        endif
                        
         
    enddo
    
end do
    
end subroutine sub_compudistr_out_2dim_cpl
! ---------------------------------------------------------------------------------
    
    
    
! ---------------------------------------------------------------------------------
subroutine sub_compudistr_out(Phi,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac,inds,vals, &
    gr_wage_in,net_wage_in,wage_totax_in,gr_ben_p_in,ben_p_in,leis,lab,ec,dc,yc,epsc,kc,hc,sc,ic,gc,jc,tc,tcp1,jmax,ndlloc,pc,ickid,gckid,opt_kid,uc,vc,scpar,gcpar,xcpar,ycpar,nc)
! compute the distribution, given population fraction [SINGLES]
implicit none
    
real(dp),intent(inout):: Phi(:,:,:,:,:,:,:,:,:,:,:,:),agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(in)::frac
real(dp),intent(in)::vals(:),gr_wage_in,net_wage_in,wage_totax_in,ben_p_in,leis,lab,gr_ben_p_in
integer,intent(in):: inds(:)
integer,intent(in):: yc,ec,dc,kc,ic,gc,jc,tc,sc,tcp1,jmax,ndlloc,pc,ickid,gckid,hc,opt_kid,scpar,gcpar,xcpar,ycpar,epsc
integer,intent(inout):: uc,nc,vc
real(dp):: vals_loc(2)
integer:: inds_loc(2)
real(dp):: ben_u,ben_p,gr_wage,net_wage,ifrac,gr_ben_p,wage_totax,ifrac_aggr
integer:: wc
    
gr_wage=0.0_dp
net_wage=0.0_dp
gr_ben_p = 0.0_dp
ben_p = 0.0_dp    
wage_totax = 0.0_dp
   
if (ec==1 .and. dc<=ndlloc)then
    gr_wage = gr_wage_in
    net_wage = net_wage_in
    wage_totax = wage_totax_in
    
elseif ( (ec==1 .and. dc==ndlloc+1) )then
       
    print*, "should not be here"
    pause
        
elseif (ec==ne .or. dc==nd)then
       
    ben_p = ben_p_in
    gr_ben_p = gr_ben_p_in
  
endif
                                                                                       
do wc=1,2
    
    !print*, dc,qc,inds(wc)
    !pause
  
    ifrac=vals(wc)*frac     ! current fraction
    
    if (isnan(ifrac))then
        print*, "nan",vals(wc),frac,ec,dc,jc
       ! pause
    endif
    
    
    ! aggregation, weighting with current fration
    !call sub_aggr(ifrac,inds(wc),leis,gr_wage,net_wage,ben_u,ben_p,demo%sr(ic,jc,tc))
    if (opt_kid==0)then
        if (jc>=jf-1 .and. opt_cpl_aggr==1)then
            ifrac_aggr = ifrac * probmar_s(1,gc,sc)/ 0.75_dp
        else
            ifrac_aggr = ifrac
        endif
        call sub_aggr_out(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac_aggr,inds(wc),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,demo%sr(jc,tc), &
            ec,dc,yc,epsc,kc,hc,ic,sc,gc,jc,tc,tcp1,pc,ickid,gckid,1,1,nc,1) !agg%inc_thrs(tc))
    
        ! add current fraction to distribution: 
        Phi(dc,inds(wc),pc,yc,ec,kc,gc,epsc,hc,sc,ickid,gckid)=Phi(dc,inds(wc),pc,yc,ec,kc,gc,epsc,hc,sc,ickid,gckid)+ifrac
    else
        print*, "for optkid the other routine should be called!"
        pause
        Phi(dc,inds(wc),pc,yc,ec,1,gc,epsc,hc,sc,gcpar,scpar)=Phi(dc,inds(wc),pc,yc,ec,ic,gc,epsc,hc,sc,gcpar,scpar)+ifrac
    endif
    
   
  
end do
 
end subroutine sub_compudistr_out 
! ---------------------------------------------------------------------------------
    
! ---------------------------------------------------------------------------------
subroutine sub_compudistr_out_cpl(Phi,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac,probmar,inds,vals, &
    gr_wage_in,net_wage_in,wage_totax_in,gr_ben_p_in,ben_p_in,leis,lab,ec,dc,yc1,yc2,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc,tcp1,jmax,ndlloc,pc,opt_kid,uc,vc,nc)
! compute the distribution, given population fraction [SINGLES]
implicit none
    
real(dp),intent(inout):: Phi(:,:,:,:,:,:,:,:,:,:,:,:),agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(in)::frac
real(dp),intent(in)::vals(:),gr_wage_in(:),net_wage_in(:),wage_totax_in(:),ben_p_in(:),leis,lab,gr_ben_p_in(:),probmar
integer,intent(in):: inds(:)
integer,intent(in):: yc1,yc2,ec,dc,kc1,kc2,jc,tc,sc1,sc2,tcp1,jmax,ndlloc,pc,opt_kid,epsc1,epsc2
integer,intent(inout):: uc,nc,vc
real(dp):: vals_loc(2)
integer:: inds_loc(2)
real(dp):: ben_u,ben_p(2),gr_wage(2),net_wage(2),ifrac,gr_ben_p(2),wage_totax(2),ifrac_aggr
integer:: wc
    
gr_wage=0.0_dp
net_wage=0.0_dp
gr_ben_p = 0.0_dp
ben_p = 0.0_dp    
wage_totax = 0.0_dp
   
if (ec==1 .and. dc<=ndlloc)then
    gr_wage = gr_wage_in
    net_wage = net_wage_in
    wage_totax = wage_totax_in
    
elseif ( (ec==1 .and. dc==ndlloc+1) )then
       
    print*, "should not be here cpl"
    pause
        
elseif (ec==ne .or. dc==nd)then
       
    ben_p = ben_p_in
    gr_ben_p = gr_ben_p_in
  
endif
                                                                                       
do wc=1,2
    
    !print*, dc,qc,inds(wc)
    !pause
  
    ifrac=vals(wc)*frac     ! current fraction
    
    ifrac_aggr = ifrac * probmar/ 0.75_dp
    
    if (isnan(ifrac))then
        print*, "nan",vals(wc),frac,ec,dc,jc
       ! pause
    endif
    
    
    ! aggregation, weighting with current fration
    !call sub_aggr(ifrac,inds(wc),leis,gr_wage,net_wage,ben_u,ben_p,demo%sr(ic,jc,tc))
    if (opt_kid==0)then
        
        call sub_aggr_out_cpl(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac_aggr,probmar,inds(wc),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,demo%sr(jc,tc), &
            ec,dc,yc1,yc2,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc,tcp1,pc,nc) !agg%inc_thrs(tc))
    
        
       ! (agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac,probmar,xc,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,sr,ec,dc,yc1,yc2,epsc1,epsc2, & 
    !kc1,kc2,sc1,sc2,jc,tc,tcp1,pc,nc)
        ! add current fraction to distribution: 
        Phi(dc,inds(wc),pc,yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2)=Phi(dc,inds(wc),pc,yc1,yc2,ec,kc1,kc2,epsc1,epsc2,sc1,sc2)+ifrac
    else
        print*, "for optkid the other routine should be called!"
        pause
        !Phi(dc,inds(wc),pc,yc,ec,1,gc,epsc,hc,sc,gcpar,scpar)=Phi(dc,inds(wc),pc,yc,ec,ic,gc,epsc,hc,sc,gcpar,scpar)+ifrac
    endif
    
   
  
end do
 
end subroutine sub_compudistr_out_cpl 
! ---------------------------------------------------------------------------------   
    
    
! ---------------------------------------------------------------------------------
subroutine sub_compudistr_out_kid(Phi,Phi_old,agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,frac,inds,vals, &
    gr_wage_in,net_wage_in,wage_totax_in,gr_ben_p_in,ben_p_in,leis,lab,ec,dc,yc,epsc,kc,hc,sc,ic,gc,jc,tc,tcp1,jmax,ndlloc,pc,ickid,gckid,opt_kid,uc,vc,scpar,scpar2,mc,gcpar,xcpar,ycpar,epscpar,kcpar,nc)
! compute the distribution, given population fraction [SINGLES]
implicit none
    
real(dp),intent(inout):: Phi(:,:,:,:,:,:,:,:,:,:,:,:),Phi_old(:,:,:,:,:,:,:,:,:,:,:,:),agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(in)::frac
real(dp),intent(in)::vals(:),gr_wage_in,net_wage_in,wage_totax_in,ben_p_in,leis,lab,gr_ben_p_in
integer,intent(in):: inds(:)
integer,intent(in):: yc,ec,dc,kc,ic,gc,jc,tc,sc,tcp1,jmax,ndlloc,pc,ickid,gckid,hc,opt_kid,scpar,scpar2,gcpar,ycpar,xcpar,epsc,kcpar,epscpar,mc
integer,intent(inout):: uc,nc,vc
real(dp):: vals_loc(2)
integer:: inds_loc(2)
real(dp):: ben_u,ben_p,gr_wage,net_wage,ifrac,gr_ben_p,wage_totax
integer:: wc
    
gr_wage=0.0_dp
net_wage=0.0_dp
gr_ben_p = 0.0_dp
ben_p = 0.0_dp    
wage_totax = 0.0_dp
   
if (ec==1 .and. dc<=ndlloc)then
    gr_wage = gr_wage_in
    net_wage = net_wage_in
    wage_totax = wage_totax_in
    
elseif ( (ec==1 .and. dc==ndlloc+1) )then
       
    print*, "should not be here"
    pause
        
elseif (ec==ne .or. dc==nd)then
       
    ben_p = ben_p_in
    gr_ben_p = gr_ben_p_in
  
endif
                                                                                       
do wc=1,2
    
    !print*, dc,qc,inds(wc)
    !pause
  
    ifrac=vals(wc)*frac     ! current fraction
    
    if (isnan(ifrac))then
        print*, "nan",vals(wc),frac,ec,dc,jc
       ! pause
    endif
     
    
    Phi(dc,inds(wc),pc,yc,ec,kc,gc,epsc,hc,sc,ickid,max(scpar,scpar2))=Phi(dc,inds(wc),pc,yc,ec,kc,gc,epsc,hc,sc,ickid,max(scpar,scpar2))+ifrac
    Phi_old(dc,inds(wc),pc,yc,ec,1,gc,epsc,hc,sc,ickid,gckid)=Phi_old(dc,inds(wc),pc,yc,ec,1,gc,epsc,hc,sc,ickid,gckid)+ifrac
    
    call sub_aggr_out(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac,inds(wc),gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,1.0_dp, &
        ec,dc,yc,epsc,kc,hc,ic,sc,gc,jc,tc,tcp1,pc,ickid,scpar,gcpar,mc,nc,1)
    
    if (opt_kid==1 .and. opt_ige ==.true.)then
    if (mc==1)then ! single
        agg%educ_matrix(scpar,sc,tc) = agg%educ_matrix(scpar,sc,tc) + ifrac
        agg%educ_matrix_g(gcpar,scpar,sc,tc) = agg%educ_matrix_g(gcpar,scpar,sc,tc) + ifrac
        agg%educ_matrix_mg(mc,gcpar,scpar,sc,tc) = agg%educ_matrix_mg(mc,gcpar,scpar,sc,tc) + ifrac
         
        agg%pop_kid_s(sc,tc) = agg%pop_kid_s(sc,tc) + ifrac * demo%numchild_gsj(gcpar,scpar,jf,tc)
    else
        agg%educ_matrix(scpar,sc,tc) = agg%educ_matrix(scpar,sc,tc) + ifrac * 0.5_dp
        agg%educ_matrix_g(1,scpar,sc,tc) = agg%educ_matrix_g(1,scpar,sc,tc) + ifrac * 0.5_dp
        
        agg%educ_matrix(scpar2,sc,tc) = agg%educ_matrix(scpar2,sc,tc) + ifrac * 0.5_dp
        
        agg%educ_matrix_g(2,scpar2,sc,tc) = agg%educ_matrix_g(2,scpar2,sc,tc) + ifrac * 0.5_dp
        
        agg%educ_matrix_mg(mc,1,max(scpar,scpar2),sc,tc) = agg%educ_matrix_mg(mc,1,max(scpar,scpar2),sc,tc) + ifrac* 0.5_dp
        agg%educ_matrix_mg(mc,2,scpar2,sc,tc) = agg%educ_matrix_mg(mc,2,scpar2,sc,tc) + ifrac* 0.5_dp
         
        agg%pop_kid_s(sc,tc) = agg%pop_kid_s(sc,tc) + ifrac * demo%numchild_gsj(2,scpar2,jf,tc)
    endif
    
    ! earn persistence
    !call sub_ige(agg,demo,grid,pol,ifrac ,js,sc,inds(wc),yc,hc,jt,gcpar,scpar,xcpar,ycpar,uc,vc,jr(tc)-1,tc,1)
    ! edu persistence
    if (opt_ige==.true.)then
        if (opt_ige_compute>0)then
            call sub_ige(agg,demo,grid,pol,ifrac ,js,pc,sc,inds(wc),yc,epsc,kc,jt,gcpar,scpar,xcpar,ycpar,epscpar,kcpar,uc,vc,jr(tc)-1,tc,0)
        endif
        if (opt_ige_compute==2)then
            call sub_ige(agg,demo,grid,pol,ifrac ,js,pc,sc,inds(wc),yc,epsc,kc,jt,gcpar,scpar,xcpar,ycpar,epscpar,kcpar,uc,vc,jr(tc)-1,tc,1)
        endif
    endif
    endif
  
end do
  
!if (opt_kid==1 .and. opt_ige ==.true.) call sub_ige(agg,demo,grid,pol,frac ,js,sc,1,yc,hc,jt,gcpar,scpar,xcpar,ycpar,uc,vc,jr(tc)-1,tc,0)
  
end subroutine sub_compudistr_out_kid
! ---------------------------------------------------------------------------------    
    
   
        
    
    
! ---------------------------------------------------------------------------------    
subroutine sub_ige(agg,demo,grid,pol,frac,jc,pc,sc,xc,yc,epsc,kc,jcpar,gcpar,scpar,xcpar,ycpar,epscpar,kcpar,uc,vc,jfin,tc,opt_earn)
    
implicit none
   
type(t_agg),intent(in)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
real(dp),intent(in):: frac
integer,intent(in):: jc,sc,xc,yc,kc,jcpar,scpar,xcpar,ycpar,jfin,tc,gcpar,opt_earn,kcpar,pc,epsc,epscpar
integer,intent(inout):: uc,vc
real(dp):: PhiC(np,nx,ny,nw),PhiP(nx,ny,nw),PhiCm1(np,nx,ny,nw),PhiPm1(nx,ny,nw),inv_ces,hk_kid_kid
integer:: jst,ijc,ixc,iyc,iycc,iic,xc0par,xc1par,yc0par,yc1par,ixcpar,iycpar,iepsc,iepscpar,hc_min,hc_max,pckid,pcckid,pcckid_tmp,iepscc, quint_par, quint_kid, ixp, tcm1
real(dp):: gr_wage,net_wage,wage_totax,ass_kid,coh_kid,coh_kid_intp,dist_kid,dist_par,coh_par,coh_par_intp,ass_par,distr_par,distr_kid,inc_kid,inc_par,hp1
integer:: inds(2),inds_h(2)
real(dp):: vals(2),vals_h(2)
real(dp),parameter:: epsi = 1.0e-06

if (tc==1 .or. tc==nt ) then
    tcm1=tc
else
   
    tcm1=max(1,tc-1)
       ! careful: when computing transition, have to start in period 1, so that capital stock in period 2 is computed
    
endif

if (frac==0.0) return

if (opt_earn==0)then

    ! educational persistense
    uc=uc+1

    if (uc<=nu_long) then

        grid%phi_edu_long(uc) = frac

        grid%pc_edu_long(uc,1) = scpar ! real(max(scpar,2))
        grid%pc_edu_long(uc,2) =sc ! real(max(sc,2))
     
        !print*, sc, scpar, frac

    endif    

else

    ! earnings persistence
    PhiC = 0.0_dp
    PhiP = 0.0_dp
    PhiCm1 = 0.0_dp
    PhiPm1 = 0.0_dp

    PhiCm1(1,xc,yc,epsc) = frac
    PhiPm1(xcpar,ycpar,epscpar) = 1.0_dp

    jst = jc+1

    do ijc = jst,jfin
        
        !if (ijc>jt) print*, "age ige", ijc
   
        do pckid = 1,pc_max(1,ijc-1)
    
            do ixc = 1,nx
                
                do iyc = 1,ny
                    
                    do iepsc = 1,nw
            
                        if (PhiCm1(pckid,ixc,iyc,iepsc)==0.0_dp) cycle
                    
                        if (ijc>jf .and. ijc<=jt)then
                        
                            hk_kid_kid = f_hh_tp1(grid%hk_grid(pckid),1.0_dp,pol%m(1,ixc,pckid,iyc,1,iepsc,1,kc,1,sc,1,1,ijc-1,tcm1), & 
                                pol%t1(1,ixc,pckid,iyc,1,iepsc,1,kc,1,sc,1,1,ijc-1,tcm1),ijc-1-jf+1,inv_ces)
                        
                            call basefun (grid%hk_grid,np,hk_kid_kid,vals_h,inds_h)
                        
                        elseif (ijc ==jf)then
                        
                            ! initial probs
                            call basefun(grid%hk_grid,np,agg%h0distr(1,sc),vals_h,inds_h)
                                                           
                        else !if ! (ijc < jf)then
                        
                            inds_h = 1
                            vals_h = 0.0_dp
                            vals_h(1) = 1.0_dp
                        
                        endif
                    
                        
                        
                        do pcckid_tmp = 1,2
                        
                            pcckid = inds_h(pcckid_tmp)
            
                            do iycc = 1,ny
                    
                                do iepscc = 1,nw
                
                                    ! wage of children
                                    if (sc>2 .and. ijc<jstud(sc) ) then
           
                                        call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(1,tc),demo%ageprod(1,ijc,1),demo%grid_y(iycc),grid%hk_grid(1),agg%tau_p(tc),1,kc,1,1,ijc,1,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                
                                    else
                
                                        call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(1,ijc,sc),demo%grid_y(iycc),grid%hk_grid(1),agg%tau_p(tc),sc,kc,1,1,ijc,1,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
                
                                    endif
                        
                                    gr_wage = gr_wage * demo%grid_epsi(iepscc) 
                                    net_wage = net_wage * demo%grid_epsi(iepscc) 
                                    wage_totax = wage_totax * demo%grid_epsi(iepscc) 
                        
                       
               
                                    ! child coh today
                                    ass_kid = grid%sav(1,ixc,pckid,iyc,1,iepsc,1,kc,1,sc,1,1,ijc-1,tcm1)
                                    coh_kid = f_coh(ass_kid,agg%tr_sj(sc,ijc,tc),1.0_dp+agg%ret(tc),net_wage,0.0_dp,1,1,1,1,agg%tau_k(tc) )
                 
                                    !call basefun(grid%hk_grid,np,agg%h0distr(1,sc),vals_h,inds_h)
                                    coh_kid_intp = func_intp(grid%coh(1,:,pcckid,iycc,1,iepscc,1,kc,1,sc,1,1,ijc,tc),grid%coh(1,:,pcckid,iycc,1,iepscc,1,kc,1,sc,1,1,ijc,tc),coh_kid,vals,inds,.false.,.false.)
                
                                    do iic=1,2
                        
                            
                                        PhiC(pcckid,inds(iic),iycc,iepscc) = PhiC(pcckid,inds(iic),iycc,iepscc) + PhiCm1(pckid,ixc,iyc,iepsc) * vals(iic) * demo%prob_y(iyc,iycc)* demo%prob_epsi(iepscc) * vals_h(pcckid_tmp)
                                
                                        ! grid%Phi(1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)
                           
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        dist_kid = abs(sum(PhiC)-frac)
        if (dist_kid > epsi)then
            print*, 'something is wrong with measure of children',ijc
            print*, 'distance is: ', dist_kid,sum(PhiC),frac
            pause
        endif
        
        
        if (ijc >=jcpar)then
            if (ijc==jcpar)then
                PhiP(:,:,:)=PhiPm1(:,:,:)
                xc0par=xcpar
                xc1par=xcpar
                yc0par=ycpar
                yc1par=ycpar
            else
                xc0par=1
                xc1par=nx
                yc0par=1
                yc1par=ny
                
                !print*, "computing transition for parents", ijc
            
                ! transition for parents
               
                    do ixc = 1,nx
                
                        do iyc = 1,ny
                    
                            do iepsc = 1,nw
            
                                if (PhiPm1(ixc,iyc,iepsc)==0.0_dp) cycle
                    
                                do iycc = 1,ny
                    
                                    do iepscc = 1,nw
                
                                        call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(scpar,tc),demo%ageprod(1,ijc,scpar),demo%grid_y(iycc),grid%hk_grid(1),agg%tau_p(tc),scpar,kcpar,1,1,ijc,1,agg%gammah_s(scpar,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
                
                                        gr_wage = gr_wage * demo%grid_epsi(iepscc) 
                                        net_wage = net_wage * demo%grid_epsi(iepscc) 
                                        wage_totax = wage_totax * demo%grid_epsi(iepscc) 
                        
                       
               
                                        ! par coh today
                                        ass_kid = grid%sav(1,ixc,1,iyc,1,iepsc,1,kcpar,1,scpar,1,1,ijc-1,tcm1)
                                        coh_kid = f_coh(ass_kid,agg%tr_sj(scpar,jcpar,tc),1.0_dp+agg%ret(tc),net_wage,0.0_dp,1,1,1,1,agg%tau_k(tc) )
                 
                                        !call basefun(grid%hk_grid,np,agg%h0distr(1,sc),vals_h,inds_h)
                                        coh_kid_intp = func_intp(grid%coh(1,:,1,iycc,1,iepscc,1,kcpar,1,scpar,1,1,ijc,tc),grid%coh(1,:,1,iycc,1,iepscc,1,kcpar,1,scpar,1,1,ijc,tc),coh_kid,vals,inds,.false.,.false.)
                
                                        do iic=1,2
                        
                            
                                            PhiP(inds(iic),iycc,iepscc) = PhiP(inds(iic),iycc,iepscc) + PhiPm1(ixc,iyc,iepsc) * vals(iic) * demo%prob_y(iyc,iycc)* demo%prob_epsi(iepscc) 
                                
                                            ! grid%Phi(1,xc,pc,ycm1,ecm1,kc,gc,epscm1,hcm1,sc,ickidm1,scpar,jcm1,tcm1)
                           
                                        enddo
                                    enddo
                                enddo
                            enddo
            
                        enddo
                    enddo
                
            endif
        
            dist_par = abs(sum(PhiP)-1.0_dp)
            if (dist_par > epsi)then
                print*, 'something is wrong with measure of parents'
                print*, 'distance is: ', dist_par
                pause
            endif
            
            do ixcpar =1,nx
                do iycpar = 1,ny
                    do iepscpar = 1,nw
                
                        distr_par = PhiP(ixcpar,iycpar,iepscpar)
                
                        if (distr_par ==0.0_dp) cycle
                        
                        
                
                        ! wage of parents
                        call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(scpar,tc),demo%ageprod(gcpar,ijc,scpar),demo%grid_y(iycpar),grid%hk_grid(1),agg%tau_p(tc),scpar,kcpar,1,1,ijc,1,agg%gammah_s(scpar,tc),agg%abgrad_s(:,scpar),agg%hk_cutoff(:,tc))
                
                        gr_wage = gr_wage * demo%grid_epsi(iepscpar)
                        net_wage = net_wage* demo%grid_epsi(iepscpar)
                        wage_totax = wage_totax* demo%grid_epsi(iepscpar)
                        
                
                        inc_par = pol%grwageinc(1,ixcpar,1,iycpar,1,iepscpar,1,kcpar,1,scpar,1,1,ijc,tc)  ! grid%lab(gcpar,1,scpar)  !+ agg%ret(tc) * grid%ass(1,ixcpar,hc,iycpar,1,1,gcpar,1,1,scpar,1,1,ijc,tc)
                                 !  pol%grwageinc(1,ixc,1,iyc,1,iepsc,1,kc,1,sc,1,1,ijc,tc)
                        quint_par = fun_dec(inc_par,qvec_inc_base)
                        
                   
                
                        do pcckid = 1,1 !pc_max(1,ijc)
                        
                            do ixc = 1,nx
                                do iyc = 1,ny
                                    do iepsc=1,nw
                        
                                        distr_kid = PhiC(pcckid,ixc,iyc,iepsc)
                            
                                        if (distr_kid ==0.0_dp) cycle
                        
                                       ! if (ijc>jt) print*, "age ige", ijc
                                        
                                        call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(1,ijc,sc),demo%grid_y(iyc),grid%hk_grid(1),agg%tau_p(tc),sc,kc,1,1,ijc,1,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
                
                                        gr_wage = gr_wage * demo%grid_epsi(iepsc)
                                        net_wage = net_wage* demo%grid_epsi(iepsc)
                                        wage_totax = wage_totax* demo%grid_epsi(iepsc)
                        
                            
                                        inc_kid = pol%grwageinc(1,ixc,pcckid,iyc,1,iepsc,1,kc,1,sc,1,1,ijc,tc) !grid%lab(1,1,sc) !+ agg%ret(tc) * grid%ass_child(1,ixc,1,iyc,1,1,1,1,ihc,sc,1,scpar,ijc,tc)
                                                  !pol%grwageinc(1,ixc,pcckid,iyc,1,iepsc,1,kc,pc,sc,1,1,ijc,tc)
                                        quint_kid = fun_dec(inc_kid,qvec_inc)
                        
                                        if (ijc>jcpar)then
                                            vc = vc+1
                            
                                           ! print*, "here1", vc, nv_long, distr_kid,inc_par,inc_kid
                                            
                                            if (vc>nv_long) return
                            
                                            grid%phi_earn_long(vc)=distr_kid
                                            grid%pc_earn_long(vc,1)=inc_par
                                            grid%pc_earn_long(vc,2)=inc_kid
                                            
                                          ! print*, "here", distr_kid,inc_par,inc_kid
                            
                                        endif
                        
                                    enddo
                                enddo
                            enddo
                        enddo
                        
                
                    enddo
                
                enddo
            enddo
        
                
        
        endif
    
        PhiCm1(:,:,:,:)=PhiC(:,:,:,:)
        PhiC(:,:,:,:)=0.0_dp
        if (ijc>=jcpar) then
            PhiPm1(:,:,:)=PhiP(:,:,:)
            PhiP(:,:,:)=0.0_dp
        endif
    
    
    enddo

endif

end subroutine sub_ige
! ---------------------------------------------------------------------------------     
     

    
 
! ---------------------------------------------------------------------------    
subroutine sub_aggr_out(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac,xc,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,sr,ec,dc,yc,epsc,kc,hc,ic,sc,gc,jc,tc,tcp1,pc,ickid,scpar,gcpar,mcpar,nc,xc_dec)
! aggregation for given measures (singles)
! based on detrended (by technology level) variables 
implicit none
    
real(dp),intent(inout)::agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
integer,intent(in):: jc,tc,ic,sc,gc,ec,dc,yc,kc,tcp1,pc,ickid,scpar,gcpar,mcpar,hc,xc_dec,epsc
real(dp),intent(in)::ifrac,gr_wage,net_wage,ben_p,sr,gr_ben_p,wage_totax
integer,intent(in)::xc
integer,intent(inout)::nc
real(dp)::popgr_tmp,afcev1,cev_loc,tcost,lab,wght,wage_inc,net_wage_inc,ass_inc,net_ass_inc,tot_inc,disp_inc,ap,bq,ass,cons,temp,frisch_loc,av_prod,inv_ces_p,hp1,dist_foc,probfe(nk),dprobfe(nk),ev_kid_choice(2),logsum_educ,prob_col(2),avtax,margtax,nkid
real(dp):: moninv,muc,rhs,mut,moninvratio
integer:: jcm1,tcm1,ind_below,jcc
real(dp),parameter::epsi=1.0e-05_dp
jcm1 = max(js,jc-1)
tcm1 = max(1,tc-1)


! weight used in aggregation: shares times population numbers
if (jc>js)then
    if (tc==1)then
         
        wght=ifrac*agg%pop_j_temp(jc-1,1) / (demo%popgrt(tc) + 1.0_dp) * demo%sr(jc-1,max(1,tc-1))   !demo%frac_jt(jc,tc) !*demo%sr(jc-1,tc)
    elseif (tc ==nt) then
        wght=ifrac*agg%pop_j_temp(jc-1,tc) / (demo%popgrt(tc) + 1.0_dp) * demo%sr(jc-1,tc)
    else
        popgr_tmp =agg%pop_j(jc,max(1,tc-1)) / agg%pop_j(jc-1,max(1,tc-1))
        wght=ifrac*agg%pop_j(max(1,jc-1),max(1,tc-1))* popgr_tmp 
    endif
    
else
    if (tc==1)then
        wght=ifrac*agg%pop_j_temp(jt,tc) 
    elseif (tc==nt)then
        wght=ifrac*agg%pop_j_temp(jt,tc) 
    else
        wght=ifrac*agg%pop_j(jt,tc) 
    endif
    
    if (mcpar==1)then
        nkid = demo%numchild_gsj(gcpar,scpar,jf,1 )
    else
        nkid = demo%numchild_gsj(2,scpar,jf,1 )
    endif
    wght = wght *nkid
   
    wght = wght * 0.5_dp / 0.75_dp
endif

if (isnan(wght))then
    print*, "nan here",ifrac,jc
    pause
endif

if (pol%net_trr(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ==1)then
    agg%net_trr_j(jc,tc)=agg%net_trr_j(jc,tc) + wght 
endif

agg%pop_j(jc,tc) =agg%pop_j(jc,tc) + wght 
agg%pop_s(sc,tc) =agg%pop_s(sc,tc) + wght
agg%pop_sj(sc,jc,tc) =agg%pop_sj(sc,jc,tc) + wght



if (jc==js)then
    
    if (mcpar==1)then
        nkid = demo%numchild_gsj(gcpar,scpar,jf,tc)
    else
        nkid = demo%numchild_gsj(2,scpar,jf,tc)
    endif
    tcost = func_tcost(grid%hk_grid(hc),sc,scpar)

    ! first compute adjustment factor
        afcev1 = 0.0_dp
        do jcc=js,nj !jstud(3),nj
            	afcev1 =afcev1 + betta**(jcc-js) *demo%sr(jcc,1)
                
                
        enddo

    cev_loc = fun_cev(pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)+tcost,afcev1)
    if (sc>2) agg%cev_s(scpar,tc) =agg%cev_s(scpar,tc) + cev_loc * wght !/nkid
    if (sc>2) agg%popjacol_s(scpar,tc) =agg%popjacol_s(scpar,tc) +  wght !/nkid
    if (sc>2) agg%hkjacol_s(scpar,tc) =agg%hkjacol_s(scpar,tc) +  wght * grid%hk_grid(hc)
    agg%popja_s(scpar,tc) =agg%popja_s(scpar,tc) +  wght !/nkid
    agg%hkja_s(scpar,tc) =agg%hkja_s(scpar,tc) +  wght * grid%hk_grid(hc)


    agg%v_kid(jc,tc)  = agg%v_kid(jc,tc) + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * wght

    if (sc==ns)then
        agg%clsubs(tc) = agg%clsubs(tc) + wght * col_subs_cost
        agg%clcost(tc) = agg%clcost(tc) + wght * fee_flow 
    elseif (sc==ns-1)then
        agg%clsubs(tc) = agg%clsubs(tc) + wght *col_subs_cost  * 0.5_dp
        agg%clcost(tc) = agg%clcost(tc) + wght * fee_flow * 0.5_dp 
    endif
    
    if (sc==ns-1)then
        
        if ( (pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc-1,ickid,scpar,jc,tc) - pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc))> epsi )then
            agg%regretcl(tc) = agg%regretcl(tc) + wght
        endif
        
        
    elseif (sc==ns)then
        if ( (pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc-1,ickid,scpar,jc,tc) - pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc))> epsi )then
            agg%valuedrop(tc) = agg%valuedrop(tc) + wght
        endif
        
    endif
     
    
    ev_kid_choice(1) = pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,1,ickid,scpar,jc,tc)
    ev_kid_choice(2) = pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,3,ickid,scpar,jc,tc)*f_probfin(grid%hk_grid(hc)) + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,2,ickid,scpar,jc,tc)*(1.0_dp-f_probfin(grid%hk_grid(hc)) ) 
    call sub_logsumprob(ev_kid_choice,logsum_educ,prob_col,2,sigma_emp)

    !if (abs(pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,1,ickid,scpar,jc,tc) - (pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,3,ickid,scpar,jc,tc)*f_probfin(grid%hk_grid(hc)) + pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,2,ickid,scpar,jc,tc)*(1.0_dp-f_probfin(grid%hk_grid(hc)) ) ) ) < 0.3_dp ) then
    if (prob_col(1) > 0.01_dp .and. prob_col(1) <0.98_dp ) then
   
       agg%marprem(sc,tc) = agg%marprem(sc,tc) + wght *   agg%wage_s(sc,tc) * sum(demo%ageprod(gc,js+1:jr(1)- 1,sc) )/(jr(1) - js) * gammafe(sc,kc) 
       agg%marprem_pop(sc,tc) = agg%marprem_pop(sc,tc) + wght 
    endif 
    
    agg%avprem(sc,tc) = agg%avprem(sc,tc) + wght * (  agg%wage_s(sc,tc) * sum(demo%ageprod(gc,js+1:jr(1)- 1,sc) )/(jr(1) - js) ) ! /   (agg%wage_s(1,tc) * sum(demo%ageprod(gc,js+1:jr(1)- 1,1) )/(jr(1) - js) ) 
    agg%avprem_pop(sc,tc) = agg%avprem_pop(sc,tc) + wght 
    
endif
 
if (jc==jt .and. gc==2)then
    agg%v_delta(jc+1,tc) = agg%v_delta(jc+1,tc) + wght/0.5_dp * (pol%v(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) -  pol%v_nokid(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ) 
endif


agg%popfull(tc) = agg%popfull(tc) + wght
    
if (jc<jr(1) .and. jc>js ) then
    if (  pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)  > 0.0_dp ) agg%emprate(tc) = agg%emprate(tc) + wght
endif

agg%ass(tc) = agg%ass(tc) + wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
agg%sav(tc) = agg%sav(tc) + wght*grid%sav(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)

! savings
ap=grid%sav(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)/((1.0_dp+demo%lamt(tc)) *(1.0_dp+demo%popgrt(tc))  )
!ap=grid%sav(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)/(1.0_dp+demo%lamt(tc))
agg_vec(1)=agg_vec(1)+wght*ap

! accidental bequests, cum interest:
bq=ap*(1.0_dp-sr)*f_aftertaxR(1.0_dp+agg%ret(tcp1),agg%tau_k(tcp1)) 
agg_vec(2)=agg_vec(2)+wght*bq

! labor
lab=pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)*demo%ageprod(gc,jc,sc)*demo%grid_y(yc)*gammafe(sc,kc)

! by skill group (this goes in agg LAB in outwageret)
!if ( pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) > (0.1_dp * ft_lab) )then
    agg_str_s(1,sc)=agg_str_s(1,sc)+wght*lab
    if (jc>js)then
        agg%lab_s(sc,tc) = agg%lab_s(sc,tc) + wght*lab
    else
        agg%lab_s(1,tc) = agg%lab_s(1,tc) + wght*lab
    endif
!endif


if (lab * net_wage <=inc_thrs_param)then
    ind_below = 1
else
    ind_below = 0
endif


! working age population 
if (jc<jr(tc) )agg_str_gj(1,gc,jc)= agg_str_gj(1,gc,jc) +wght
 
! full time earnings
if (ec==1)then
   if (dc==1 )then  !if (dc<nd)then
       agg_vec(25) = agg_vec(25) + wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)*net_wage 
       agg_vec(26) = agg_vec(26) + wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
   endif
endif

agg%pop_tot(tc) = agg%pop_tot(tc) + wght

! college share TOTAL
if (sc==ns) agg%clshr_tot(tc) = agg%clshr_tot(tc) + wght

if (jc==js) then

    agg%clshr_jasp(sc,scpar,tc) = agg%clshr_jasp(sc,scpar,tc) + wght
    agg%hk_jasp(sc,scpar,tc) = agg%hk_jasp(sc,scpar,tc) + wght * grid%hk_grid(hc)
    agg%pop_jasp(hc,sc,scpar,tc) = agg%pop_jasp(hc,sc,scpar,tc) + wght
    
    agg%grwage(tc) = agg%grwage(tc) + wght * gr_wage
endif


! total inv
if (jc>=jf .and. jc<jt .and. gc==2 )then
    
    muc = pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)**(-1.0_dp) / (1.0_dp + agg%tau_c(tc))
    mut =(pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * demo%numchild_gsj(gc,sc,jf,tc) )**(pssi(gc)) *  kappat_param
    rhs =( mut  )**(1.0_dp/(1.0_dp-fitm_param)) * (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)) &
        * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * (mean_tinv/mean_moninv)**(fitm_param/(1.0_dp-fitm_param) )
   
    
    moninv =  rhs / (( muc )**(1.0_dp/(1.0_dp-fitm_param)) )
    if (pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) > 0.0_dp )then
        moninvratio = moninv / pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    else
        moninvratio = 0.0_dp
    endif
    
    agg%moninv_tot(tc) = agg%moninv_tot(tc) + wght * moninv * demo%numchild_gsj(gc,sc,jf,tc)
    agg%tinv_tot(tc) = agg%tinv_tot(tc) + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * demo%numchild_gsj(gc,sc,jf,tc)
    agg%poppar_inv(tc) = agg%poppar_inv(tc) + wght
elseif (jc==jt) then
    agg%ivt_tot(tc) = agg%ivt_tot(tc) + wght * pol%b(dc,xc,pc,yc,ec,epsc,gc,kc,sc,hc,scpar,tc) * demo%numchild_gsj(gc,sc,jf,tc)
    
endif

! how to define HK stock. INIT SS: hk_ja * pop_tot / pop_tot = hk_ja. First period of transition: the same. Technically: ((pop_tot - 1)*agg%hk_tot(tcm1) + hk_ja)/pop_tot

if (jc==js+1)then
    agg%meanwage_s(sc,tc) = agg%meanwage_s(sc,tc) +  wght* ( gammafe(sc,kc)  ) !probfe(kc) * gammafe(kc)
    
    
    if (wght<0.0_dp)then
        print*, "neg wght????"
        pause
    endif
    
    agg%slopewage_s(sc,tc) =agg%slopewage_s(sc,tc) + (dprobfe(nk)   *( gammafe(sc,2) -  gammafe(sc,1) )  )  *  wght! * grid%hk_grid(hc) !/(probfe(1) * gammafe(sc,1) + probfe(2) * gammafe(sc,2) )
 
endif


!hours
agg_vec(3)=agg_vec(3)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)  
if (gc==1)then
    agg_vec(19)=agg_vec(19)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
elseif (gc==2)then
    agg_vec(20)=agg_vec(20)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
endif

! full time hours
if (jc>js .and. jc<jr(tc))then
    if (ec==1 )then
        if (dc<=ndl .or. dc==nd)then
            if (gc==1) agg_vec(27)= agg_vec(27)+ wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
            if (gc==2) agg_vec(28) = agg_vec(28) + wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
            agg_str_g(1,gc)= agg_str_g(1,gc) +wght
        endif
        
    else        
        agg_str_g(1,gc)= agg_str_g(1,gc) +wght  
    endif
    
    call sub_tax(margtax,avtax,wage_totax*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc),0.0_dp,0.0_dp,pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc),0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_p(tc))
    
    !print*, margtax,avtax,wage_totax*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    !pause

    !margtax =  pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)**(-tau_pr) * lambda_pr * avearn**0.18_dp * (1.0_dp - tau_pr)
    !avtax =   pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)**(-tau_pr) * lambda_pr * avearn**0.18_dp
    
    agg%avtax_j(jc,tc) = agg%avtax_j(jc,tc)+ wght * margtax
    agg%avtax_av_j(jc,tc) = agg%avtax_av_j(jc,tc) + wght * avtax
    
    agg%avtaxrat(tc) = agg%avtaxrat(tc) + wght * margtax/avtax
    
    if ( pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)<=pvec_inc(1) )then
        agg%avtaxbot(tc) = agg%avtaxbot(tc) + wght * avtax
        agg%margtaxbot(tc) = agg%margtaxbot(tc) + wght * margtax
        agg%avtaxbot_frac(tc) = agg%avtaxbot_frac(tc) + wght
    endif
    
 
endif

! consumption
cons = pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
agg_vec(4)=agg_vec(4)+wght*cons

! consumption by age (lc profile)
agg%cons_j(jc,tc) = agg%cons_j(jc,tc) +wght*cons

!agg%pop_sj(sc,jc,tc) = agg%pop_sj(sc,jc,tc) +wght

!if (sc==1)then
    agg%lab_sj(sc,jc,tc) = agg%lab_sj(sc,jc,tc)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    agg%cons_sj(sc,jc,tc) = agg%cons_sj(sc,jc,tc)+wght*pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    agg%ass_sj(sc,jc,tc) = agg%ass_sj(sc,jc,tc)+wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    agg%labinc_sj(sc,jc,tc) = agg%labinc_sj(sc,jc,tc) + wght * pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * gr_wage
    agg%totinc_sj(sc,jc,tc) = agg%totinc_sj(sc,jc,tc) + wght * pol%grinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    
    if (grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)<= grid%amin_age(gc,sc,1,jc-1) ) agg%frac_bc_sj(sc,jc,tc) = agg%frac_bc_sj(sc,jc,tc) + wght
    
    if  (pol%labinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) <0.0_dp)  agg%frac_nettr_sj(sc,jc,tc) = agg%frac_nettr_sj(sc,jc,tc) + wght
!else
!    agg%lab_sj(2,jc,tc) = agg%lab_sj(2,jc,tc)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!    agg%cons_sj(2,jc,tc) = agg%cons_sj(2,jc,tc)+wght*pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!    agg%ass_sj(2,jc,tc) = agg%ass_sj(2,jc,tc)+wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!
!endif


agg%ass_j(jc,tc) = agg%ass_j(jc,tc) +wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) !ap

agg%lab_j(jc,tc) = agg%lab_j(jc,tc) +wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
 
! aggregate gross savings: NEGATIVE SAV
if (ap<0.0_dp)then
    agg_vec(5)=agg_vec(5)+wght*grid%sav(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
endif

! gr_wage income
wage_inc=pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)*gr_wage 
agg_vec(6)=agg_vec(6)+wght*wage_inc

if (jc<jr(1) ) then
    agg%wage_inc_s(sc,tc) = agg%wage_inc_s(sc,tc) + wght * wage_inc
    agg%fe_s(sc,tc) = agg%fe_s(sc,tc) + wght * gr_wage
    
endif

av_prod = demo%ageprod(gc,jr(tc)-1,sc )  
agg%pens_base(tc) = agg%pens_base(tc) + pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * agg%wage_s(sc,tc) * av_prod
    
if (pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) * net_wage <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif

! net_wage income / after tax earnings (so net of all taxes)
net_wage_inc=pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)*net_wage - fun_hsv_new(wage_totax*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc),0.0_dp,0.0_dp,pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc),0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,ind_below) !,avearn_base,ind_below) 
!agg_vec(7)=agg_vec(7)+wght*net_wage_inc

!if (grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc)<=0.0_dp .and. grid%amin_age(gc,sc,1,jc)<=0.0_dp .and. grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) - grid%amin_age(gc,sc,1,jc)<-0.00001_dp)then
if (abs(grid%sav(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) -grid%amin_age(gc,sc,1,jc))<epsi)then 
   agg%indbcexact_j(jc,tc) = agg%indbcexact_j(jc,tc) + wght
  
   !agg%indbc_xj(xc_dec,jc,tc) = agg%indbc_xj(xc_dec,jc,tc) + wght
   !print*, "bc doesnt work", jc, grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc), grid%amin_age(gc,sc,1,jc)
endif

if ( grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) < net_wage_inc / 6.0_dp  )then 
   agg%indbc_j(jc,tc) = agg%indbc_j(jc,tc) + wght
  
   !agg%indbc_xj(xc_dec,jc,tc) = agg%indbc_xj(xc_dec,jc,tc) + wght
   !print*, "bc doesnt work", jc, grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc), grid%amin_age(gc,sc,1,jc)
endif

!if (jc>=(js+1) .and. jc<=jr(tc) -1 )then
    !if (jc<=jr(tc) -1)then
    !    agg_vec(7)=agg_vec(7)+wght*wage_inc
    !endif
        
    agg%asswp(tc) = agg%asswp(tc) +wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) !ap
    agg%ky_j(jc,tc) =  agg%ky_j(jc,tc) + wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)/net_wage_inc
!endif
agg%asswp_j(jc,tc) = agg%asswp_j(jc,tc) +wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
agg%net_wage_inc_j(jc,tc) = agg%net_wage_inc_j(jc,tc) +wght*net_wage_inc 

agg%wage_inc_j(jc,tc) = agg%wage_inc_j(jc,tc) +wght*wage_inc
    
! total income
ass=grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
ass_inc=(ass)*agg%ret(tc) ! gross asset income
tot_inc=wage_inc + ass_inc ! gross !wage_inc
agg_vec(8)=agg_vec(8)+wght*tot_inc
agg%tot_inc_j(jc,tc) = agg%tot_inc_j(jc,tc) +wght*tot_inc
    
! disposable income:
net_ass_inc=ass*agg%ret(tc) - f_captax(ass,1.0_dp+agg%ret(tc),agg%tau_k(tc)) ! capital income taxes yet so gross is equal to net capital income
disp_inc=net_wage_inc+net_ass_inc
agg_vec(9)=agg_vec(9)+wght*disp_inc
! disp income by skill, cit group

! tax revenues
agg_vec(10)=agg_vec(10)+wght*pol%labinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ! labor
agg%labinctaxrv(tc) = agg%labinctaxrv(tc)+wght*pol%labinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
agg_vec(11)=agg_vec(11)+wght*pol%capinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ! cap
agg_vec(12)=agg_vec(12)+wght*pol%constaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ! cons

agg%hsvtmp1(tc) = agg%hsvtmp1(tc) + wght*pol%hsvtmp1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
agg%hsvtmp2(tc) = agg%hsvtmp2(tc) + wght*pol%hsvtmp2(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)

!print*, pol%labinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc), pol%hsvtmp1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!pause

agg%labinctaxrv_j(jc,tc) = agg%labinctaxrv_j(jc,tc) + wght*pol%labinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ! labor

agg%capinctaxrv_j(jc,tc) = agg%capinctaxrv_j(jc,tc) + wght*pol%capinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ! cap
agg%constaxrv_j(jc,tc) = agg%constaxrv_j(jc,tc) + wght*pol%constaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) ! cons  

agg%pccontr_j(jc,tc) = agg%pccontr_j(jc,tc) + wght * gr_wage * agg%tau_p(tc)  

! aggregate negative asset position
if ( ass<0.0_dp) then
    agg_vec(13)=agg_vec(13)+wght*ass
endif
    
! for aggregation checks: gross wage income by aggregating across households
agg_vec(14)=agg_vec(14)+wght*pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    
! for aggregation checks: total gross income by aggregating across households
agg_vec(15)=agg_vec(15)+wght*pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)

if ( (ec==1 .and. dc==nd) .or. ec==2)then
    ! aggregate pension beneefits
    agg_vec(17)=agg_vec(17)+wght*gr_ben_p ! ben_p
    
    ! pension tax revenues (net of transfers / minpens, thus can be negative)
    agg_vec(24)=agg_vec(24)+wght*pol%penstaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
   
    ! number of pensioners
    agg_vec(18)=agg_vec(18)+wght
endif
     
if (jc<jr(tc))then
    if (ec==1)then
        if (dc<nd  )then !.or. dc==nd)then
        
            ! this is for average earmomgs, that's why net of early retirees
            agg_vec(30)= agg_vec(30) +wght
           
            ! gross wage income
            agg_vec(21)=agg_vec(21)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)*net_wage
        endif
    
    endif
    
endif

 


if (jc>=jf .and. jc<jt .and. gc==2)then 
    
    !! foc error
    !call sub_fochk_check(pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) ,pol%t1(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) , & 
    !    pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc)**(-1.0_dp)/ (1.0_dp + tau_c) ,pol%lab(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) , demo%numchild_gsj(gc,sc,jc,tc),gc,jc,dist_foc)
    
    !if (pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) > epsi .and. pol%t1(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc)> epsi .and. abs(dist_foc)> 0.001_dp)then
    !    agg%hc_foc_error_j(jc) =agg%hc_foc_error_j(jc) + dist_foc * wght
    !    print*, dist_foc,pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) ,pol%t1(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    !endif
    
    !! for regressions
    !if (wght>0.0_dp )then !.and. pol%t1(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) > 0.0_dp .and.  & 
    !    !pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc)>0.0_dp) then
    !    nc = nc +1
    !    grid%phi_inv_long(nc) = wght !/ (jt-jf)
    !    grid%p_inv_long(nc,2) = pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)  
    !    grid%p_inv_long(nc,1) = pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    !endif 
        
    ! parent population
    agg%poppar(tc) = agg%poppar(tc) + wght
    ! aggregate monetary inv PER CHILD
    agg%moninv(tc) =  agg%moninv(tc)  + wght * moninv !pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    
    agg%moninvratio(tc) =agg%moninvratio(tc)  + wght * moninvratio 
    
    ! aggregate time inv PER CHILD
    agg%tinv(tc) = agg%tinv(tc)  + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    !if (jc==jf)then
    !agg%moninv_x(xc_dec,tc) =  agg%moninv_x(xc_dec,tc)  + wght * pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    !! aggregate time inv PER CHILD
    !agg%tinv_x(xc_dec,tc) = agg%tinv_x(xc_dec,tc)  + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    !endif
    ! per child money and time inv by age 
    agg%m_j(jc,tc) = agg%m_j(jc,tc) + wght * moninv !pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    agg%t_j(jc,tc) = agg%t_j(jc,tc) + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    
    !agg%m_xj(xc_dec,jc,tc) = agg%m_xj(xc_dec,jc,tc) + wght * pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    !agg%t_xj(xc_dec,jc,tc) = agg%t_xj(xc_dec,jc,tc) + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    
    agg%m_yj(yc,jc,tc) = agg%m_yj(yc,jc,tc) + wght * moninv !pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    agg%t_yj(yc,jc,tc) = agg%t_yj(yc,jc,tc) + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    
    hp1 = f_hh_tp1(grid%hk_grid(pc),gamma_s(sc),moninv,  &
                                            pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc),jc-jf+1,inv_ces_p) 
    agg%inv_j(jc,tc) = agg%inv_j(jc,tc) + wght * inv_ces_p
    
    ! per child money and time inv by skill
    agg%t_s(sc,tc) = agg%t_s(sc,tc) + wght * pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    agg%m_s(sc,tc) = agg%m_s(sc,tc) + wght * moninv ! pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    ! parent population by skill
    agg%poppar_s(sc,tc) = agg%poppar_s(sc,tc) + wght 
    
    ! parent population by skill, age
    agg%poppar_sj(sc,jc,tc) = agg%poppar_sj(sc,jc,tc) + wght
    ! parent population by skill, marital  status, age
    agg%poppar_sgj(sc,gc,jc,tc) = agg%poppar_sgj(sc,gc,jc,tc) + wght
    
    ! per child money and time inv by skill, age
    agg%m_sj(sc,jc,tc) = agg%m_sj(sc,jc,tc) + wght* moninv !pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    agg%t_sj(sc,jc,tc) = agg%t_sj(sc,jc,tc) + wght* pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    
    agg%h_sj(sc,jc,tc) = agg%h_sj(sc,jc,tc) + wght* grid%hk_grid(pc) 
    !if (jc==jf)then
    !    agg%h_sgj(sc,gc,jc,tc) = agg%h_sgj(sc,gc,jc,tc) + wght* grid%h0_grid(ickid) 
    !else
        agg%h_sgj(sc,gc,jc,tc) = agg%h_sgj(sc,gc,jc,tc) + wght* grid%hk_grid(pc) 
       
    !endif
    
    ! per child money and time inv by skill, marital status, age
    agg%m_sgj(sc,gc,jc,tc) = agg%m_sgj(sc,gc,jc,tc) + wght* moninv !pol%m(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    agg%t_sgj(sc,gc,jc,tc) = agg%t_sgj(sc,gc,jc,tc) + wght* pol%t1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
    
    !agg%m_gj
    
    ! acuqired HK at age j
    !if (jc==jf)then
    !    agg%h_j(jc,tc) = agg%h_j(jc,tc) + wght * grid%h0_grid(ickid)
    !else
        agg%h_j(jc,tc) = agg%h_j(jc,tc) + wght * grid%hk_grid(pc)
    !endif
    
    ! parent population by age
    agg%poppar_j(jc,tc) = agg%poppar_j(jc,tc) + wght 
     
    ! governemnt expenditures on education [primary plus secondary, PER CHILD, thus multiply by number of children]
    agg%igov(tc) = agg%igov(tc) + wght * igov_j(jc-jf + 1) * demo%numchild_gsj(gc,sc,jc,tc) !* 1.5_dp
    if (jc==jf) agg%igov_kg(tc) = agg%igov_kg(tc) + wght * igov_j(jc-jf + 1) * demo%numchild_gsj(gc,sc,jc,tc) !* 1.5_dp
    if (jc>jf) agg%igov_hs(tc) = agg%igov_hs(tc) + wght * igov_j(jc-jf + 1) * demo%numchild_gsj(gc,sc,jc,tc) !* 1.5_dp
    if (jc>jf) agg%igov_hs_base(tc) = agg%igov_hs_base(tc) + wght * igovbase_j(jc-jf + 1) * demo%numchild_gsj(gc,sc,jc,tc) !* 1.5_dp

    
     
elseif (jc==jt .and. gc==2)then

    !agg%igov(tc) = agg%igov(tc) + wght * igov_frac *earn_si_lo * demo%numchild_gsj(gc,sc,jc,tc) !* 1.5_dp

     
    ! parent population at ivt period
    agg%poppar_ivt(tc) = agg%poppar_ivt(tc) + wght
    ! aggregate ivt transfers FOR ALL CHILDREN in hh (used to computed ivt transfers as a fraction of assets)
    agg%ivt(tc) = agg%ivt(tc) + wght * pol%b(dc,xc,pc,yc,ec,epsc,gc,kc,sc,hc,scpar,tc) * demo%numchild_gsj(gc,sc,jc,tc)

    agg%ivt_pkid(tc) = agg%ivt_pkid(tc) + wght * pol%b(dc,xc,pc,yc,ec,epsc,gc,kc,sc,hc,scpar,tc)  


    ! per child ivt transfsers by skill group
    agg%ivt_s(sc,tc) = agg%ivt_s(sc,tc) + wght * pol%b(dc,xc,pc,yc,ec,epsc,gc,kc,sc,hc,scpar,tc) 
    ! per child ivt transfers by marital status and skill group
    agg%ivt_sg(sc,gc,tc) = agg%ivt_sg(sc,gc,tc) + wght * pol%b(dc,xc,pc,yc,ec,epsc,gc,kc,sc,hc,scpar,tc) 
    
    ! parent population by age
    agg%poppar_j(jc,tc) = agg%poppar_j(jc,tc) + wght 
    agg%poppar_sj(sc,jc,tc) = agg%poppar_sj(sc,jc,tc) + wght
    ! parent population by skill, marital  status, age
    agg%poppar_sgj(sc,gc,jc,tc) = agg%poppar_sgj(sc,gc,jc,tc) + wght
    
    ! parent population by age
    agg%poppar_j(jc+1,tc) = agg%poppar_j(jc+1,tc) + wght 
    agg%poppar_sj(sc,jc+1,tc) = agg%poppar_sj(sc,jc+1,tc) + wght
    ! parent population by skill, marital  status, age
    agg%poppar_sgj(sc,gc,jc+1,tc) = agg%poppar_sgj(sc,gc,jc+1,tc) + wght
    
    ! acuqired HK at age j
    agg%h_j(jc,tc) = agg%h_j(jc,tc) + wght * grid%hk_grid(pc)
    agg%h_sj(sc,jc,tc) = agg%h_sj(sc,jc,tc) + wght* grid%hk_grid(pc) 
    agg%h_sgj(sc,gc,jc,tc) = agg%h_sgj(sc,gc,jc,tc) + wght* grid%hk_grid(pc) 
    
    agg%h_j(jc+1,tcp1) = agg%h_j(jc+1,tcp1) + wght * grid%hk_grid(pc)
    
endif


if (jc==js)then
    
    agg%phi_hc(hc) = agg%phi_hc(hc) + wght
    agg%phi_shc(sc,hc) = agg%phi_shc(sc,hc) + wght 
    agg%wage_hc(sc,hc) = agg%wage_hc(sc,hc) +wght * gammafe(sc,kc) 
    
    
endif



end subroutine sub_aggr_out
! ---------------------------------------------------------------------------    

     
! ---------------------------------------------------------------------------    
subroutine sub_aggr_out_cpl(agg_vec,agg_str_s,agg_str_gj,agg_str_g,agg,demo,grid,pol,ifrac,probmar,xc,gr_wage,net_wage,wage_totax,gr_ben_p,ben_p,sr,ec,dc,yc1,yc2,epsc1,epsc2, & 
    kc1,kc2,sc1,sc2,jc,tc,tcp1,pc,nc)
! aggregation for given measures (singles) 
! based on detrended (by technology level) variables 
implicit none
    
real(dp),intent(inout)::agg_vec(:),agg_str_s(:,:),agg_str_gj(:,:,:),agg_str_g(:,:)
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
integer,intent(in):: jc,tc,sc1,sc2,ec,dc,yc1,yc2,kc1,kc2,tcp1,pc,epsc1,epsc2
real(dp),intent(in)::ifrac,gr_wage(:),net_wage(:),ben_p(:),sr,gr_ben_p(:),wage_totax(:),probmar
integer,intent(in)::xc
integer,intent(inout)::nc
real(dp)::wght,wage_inc,net_wage_inc,ass_inc,net_ass_inc,tot_inc,disp_inc,ap,bq,ass,cons,temp, & 
    frisch_loc,av_prod,inv_ces_p,hp1,dist_foc,probfe(nk),dprobfe(nk),ev_kid_choice(2),logsum_educ,prob_col(2),avtax,margtax,totlabinc
integer:: jcm1,tcm1,ind_below,sc(2),gc
real(dp),parameter::epsi=1.0e-05_dp
real(dp):: lab,lab1,lab2,lab_cpl,hrs1,hrs2,tinvtot,moninv,tinv1,tinv2,popgr_tmp,muc,mut1,rhs,moninvratio
jcm1 = max(js,jc-1)


tcm1 = max(1,tc-1)

sc(1) = sc1
sc(2) = sc2
! weight used in aggregation: shares times population numbers
if (tc==1)then

    
    wght=ifrac*agg%pop_j_temp(max(1,jc-1),max(1,tc-1)) / (demo%popgrt(tc) + 1.0_dp) * demo%sr(jc-1,1) *0.5_dp  !*demo%frac_jt(jc,tc) *0.5_dp !demo%sr(jc-1,tc) *0.5_dp
elseif (tc==nt)then
    wght=ifrac*agg%pop_j_temp(max(1,jc-1),tc) / (demo%popgrt(tc) + 1.0_dp) * demo%sr(jc-1,1) *0.5_dp 
else
    popgr_tmp = agg%pop_j(jc,max(1,tc-1)) /agg%pop_j(jc-1,max(1,tc-1))
    wght=ifrac*agg%pop_j(jc-1,max(1,tc-1)) * popgr_tmp * 0.5_dp ! / (demo%popgrt(max(1,tc-1)) + 1.0_dp) * demo%sr(jc-1,1) *0.5_dp  
endif

if (isnan(wght))then
    print*, "nan here",ifrac
endif

if (pol%net_trr_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) ==1)then
    agg%net_trr_j(jc,tc)=agg%net_trr_j(jc,tc) + wght 
endif

agg%poppar_j(jc,tc) = agg%poppar_j(jc,tc) + wght 

agg%popfull(tc) = agg%popfull(tc) + wght
    
if (jc<jr(1) .and. jc>js ) then
    if (  pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) > 0.0_dp ) agg%emprate(tc) = agg%emprate(tc) + wght * 0.5_dp 
    if (  pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) > 0.0_dp ) agg%emprate(tc) = agg%emprate(tc) + wght * 0.5_dp 
endif

agg%pop_j(jc,tc) = agg%pop_j(jc,tc) + wght
agg%pop_s(sc2,tc) = agg%pop_s(sc2,tc) + wght * 0.5_dp
agg%pop_s(sc1,tc) = agg%pop_s(sc1,tc) + wght * 0.5_dp
!agg%pop_sj(sc2,jc,tc) = agg%pop_sj(sc2,jc,tc) + wght

agg%ass(tc) = agg%ass(tc) + +wght*grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
agg%sav(tc) = agg%sav(tc) + +wght*grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)

! savings 
ap=grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)/((1.0_dp+demo%lamt(tc))*(1.0_dp+demo%popgrt(tc)))
agg_vec(1)=agg_vec(1)+wght*ap

! accidental bequests, cum interest:
bq=ap*(1.0_dp-sr)*f_aftertaxR(1.0_dp+agg%ret(tcp1),agg%tau_k(tcp1)) 
agg_vec(2)=agg_vec(2)+wght*bq

! labor
hrs1 = pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)!*0.5_dp
hrs2 = pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)!*0.5_dp
lab1=hrs1*demo%ageprod(1,jc,sc1)*demo%grid_y(yc1)*gammafe(sc1,kc1)*demo%grid_epsi(epsc1)!*0.5_dp
lab2=hrs2*demo%ageprod(2,jc,sc2)*demo%grid_y(yc2)*gammafe(sc2,kc2)*demo%grid_epsi(epsc2)!*0.5_dp
lab = lab1 + lab2


totlabinc = hrs1 * net_wage(1) + hrs2 * net_wage(2)

! by skill group (this goes in GE outwageret)
!if (hrs1 > (0.1_dp * ft_lab))then
    agg_str_s(1,sc1)=agg_str_s(1,sc1)+wght*lab1
    agg%lab_s(sc1,tc) = agg%lab_s(sc1,tc) + wght*lab1 
!endif
    
!if (hrs2 > (0.1_dp * ft_lab))then    
    agg_str_s(1,sc2)=agg_str_s(1,sc2)+wght*lab2 

    agg%lab_s(sc2,tc) = agg%lab_s(sc2,tc) + wght*lab2 
!endif
    
if (totlabinc <=inc_thrs_param)then
    ind_below = 1
else
    ind_below = 0
endif


! working age population 
if (jc<jr(tc) )then
    agg_str_gj(1,1,jc)= agg_str_gj(1,1,jc) +wght !* 0.5_dp
    agg_str_gj(1,2,jc)= agg_str_gj(1,2,jc) +wght !* 0.5_dp
endif

! full time earnings
if (ec==1)then
   if (dc==1 )then  !if (dc<nd)then
       agg_vec(25) = agg_vec(25) + wght*(totlabinc ) 
       agg_vec(26) = agg_vec(26) + wght*(hrs1 + hrs2)
   endif
endif


agg%pop_tot(tc) = agg%pop_tot(tc) + wght

! college share TOTAL
if (sc1==ns) agg%clshr_tot(tc) = agg%clshr_tot(tc) + wght !*0.5_dp
if (sc2==ns) agg%clshr_tot(tc) = agg%clshr_tot(tc) + wght !*0.5_dp

if (jc==jt) then
    agg%v_delta(jc+1,tc) = agg%v_delta(jc+1,tc)+ (pol%v_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) -pol%v_nokid_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)   ) * wght
endif

 
! total inv
if (jc>=jf .and. jc<jt )then
    
    tinv1 = pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !*0.5_dp
    tinv2 = pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)!*0.5_dp
    tinvtot = tinv1 + tinv2

    muc = pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)**(-1.0_dp) / (1.0_dp + agg%tau_c(tc))
    mut1 = (tinv1 * demo%numchild_gsj(2,sc2,jc,tc)  )**(pssi(2)) *  kappat_param
       
    rhs =( mut1  )**(1.0_dp/(1.0_dp-fitm_param)) * (kappai_j_param(jc-jf+1)  )**(1.0_dp/(fitm_param-1.0_dp)) &
        * tinv1 * (mean_tinv/mean_moninv)**(fitm_param/(1.0_dp-fitm_param) )
    
    moninv = rhs / (( muc )**(1.0_dp/(1.0_dp-fitm_param)) )
    if (tinvtot> 0.0_dp)then
        moninvratio = moninv / tinvtot
    else
        moninvratio = 0.0_dp
    endif
    !moninv = pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    
    agg%moninv_tot(tc) = agg%moninv_tot(tc) + wght * moninv * demo%numchild_gsj(2,sc2,jc,tc)
    agg%tinv_tot(tc) = agg%tinv_tot(tc) + wght * tinvtot * demo%numchild_gsj(2,sc2,jc,tc)
    agg%poppar_inv(tc) = agg%poppar_inv(tc) + wght !* demo%numchild_gsj(2,sc2,jc,tc)
elseif (jc==jt) then
    agg%ivt_tot(tc) = agg%ivt_tot(tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc) * demo%numchild_gsj(2,sc2,jc,tc)
endif




!hours
agg_vec(3)=agg_vec(3)+wght*(hrs1 + hrs2 ) !*0.5_dp
!if (gc==1)then
    agg_vec(19)=agg_vec(19)+wght*hrs1 !*0.5_dp
!elseif (gc==2)then
    agg_vec(20)=agg_vec(20)+wght*hrs2 !*0.5_dp
!endif

! full time hours
if (jc>js .and. jc<jr(tc))then
    if (ec==1 )then
        if (dc<=ndl .or. dc==nd)then
            agg_vec(27)= agg_vec(27)+ wght*hrs1 !*0.5_dp
            agg_vec(28) = agg_vec(28) + wght*hrs2 !*0.5_dp
            do gc = 1,ng
                agg_str_g(1,gc)= agg_str_g(1,gc) +wght  !* 0.5_dp
            enddo
            
        endif
        
    else    
        do gc = 1,ng
            agg_str_g(1,gc)= agg_str_g(1,gc) +wght !* 0.5_dp
        enddo
        
    endif
    
    call sub_tax(margtax,avtax,wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) +wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) , & 
        wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc), & 
        pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),&
        pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc),&
        1,1,ec,dc,1,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_p(tc))
    
    !print*, margtax,avtax,wage_totax*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
    !pause

    !margtax =  pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)**(-tau_pr) * lambda_pr * avearn**0.18_dp * (1.0_dp - tau_pr)
    !avtax =   pol%grwageinc(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)**(-tau_pr) * lambda_pr * avearn**0.18_dp
    
    agg%avtax_j(jc,tc) = agg%avtax_j(jc,tc)+ wght * margtax
    agg%avtax_av_j(jc,tc) = agg%avtax_av_j(jc,tc) + wght * avtax
    
    agg%avtaxrat(tc) = agg%avtaxrat(tc) + wght * margtax/avtax
    
    if ( pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)<=pvec_inc(1) )then
        agg%avtaxbot(tc) = agg%avtaxbot(tc) + wght * avtax
        agg%margtaxbot(tc) = agg%margtaxbot(tc) + wght * margtax
        agg%avtaxbot_frac(tc) = agg%avtaxbot_frac(tc) + wght
    endif
    
 
endif

! consumption
cons = pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
agg_vec(4)=agg_vec(4)+wght*cons

! consumption by age (lc profile)
agg%cons_j(jc,tc) = agg%cons_j(jc,tc) +wght*cons

agg%pop_sj(max(sc1,sc2),jc,tc) = agg%pop_sj(max(sc1,sc2),jc,tc)+wght

!if (sc==1)then
    agg%lab_sj(max(sc1,sc2),jc,tc) = agg%lab_sj(max(sc1,sc2),jc,tc)+wght*(pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc))
    agg%cons_sj(max(sc1,sc2),jc,tc) = agg%cons_sj(max(sc1,sc2),jc,tc)+wght*pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%ass_sj(max(sc1,sc2),jc,tc) = agg%ass_sj(max(sc1,sc2),jc,tc)+wght*grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%labinc_sj(max(sc1,sc2),jc,tc) = agg%labinc_sj(max(sc1,sc2),jc,tc) + wght *( pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) * gr_wage(1) + &
        pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) * gr_wage(2) )
    
    agg%totinc_sj(max(sc1,sc2),jc,tc) = agg%totinc_sj(max(sc1,sc2),jc,tc) + wght * pol%grinc_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    
    if (grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)<= grid%amin_age(1,max(sc1,sc2),1,jc-1) ) agg%frac_bc_sj(max(sc1,sc2),jc,tc) = agg%frac_bc_sj(sc1,jc,tc) + wght
    
    if  (pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) <0.0_dp)  agg%frac_nettr_sj(max(sc1,sc2),jc,tc) = agg%frac_nettr_sj(max(sc1,sc2),jc,tc) + wght
!else
!    agg%lab_sj(2,jc,tc) = agg%lab_sj(2,jc,tc)+wght*pol%lab(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!    agg%cons_sj(2,jc,tc) = agg%cons_sj(2,jc,tc)+wght*pol%cons(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!    agg%ass_sj(2,jc,tc) = agg%ass_sj(2,jc,tc)+wght*grid%ass(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!
!endif


agg%ass_j(jc,tc) = agg%ass_j(jc,tc) +wght*grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !ap

agg%lab_j(jc,tc) = agg%lab_j(jc,tc) +wght*(hrs1 + hrs2 )
 
! aggregate gross savings:
agg_vec(5)=agg_vec(5)+wght*pol%netsav_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)


! gr_wage income
wage_inc=hrs1*gr_wage(1) +hrs2*gr_wage(2) 
agg_vec(6)=agg_vec(6)+wght*wage_inc

if (jc<jr(1) ) then
    agg%wage_inc_s(sc1,tc) = agg%wage_inc_s(sc1,tc) + wght * wage_inc
    agg%fe_s(sc1,tc) = agg%fe_s(sc1,tc) + wght * gr_wage(1)
   ! agg%pop_s(sc1,tc) = agg%pop_s(sc1,tc) + wght 
    
    agg%wage_inc_s(sc2,tc) = agg%wage_inc_s(sc2,tc) + wght * wage_inc
    agg%fe_s(sc2,tc) = agg%fe_s(sc2,tc) + wght * gr_wage(2)
  !  agg%pop_s(sc2,tc) = agg%pop_s(sc2,tc) + wght 
endif

do gc = 1,ng
    av_prod = demo%ageprod(gc,jr(tc)-1,sc(gc) )  
    if (gc==1)then
        agg%pens_base(tc) = agg%pens_base(tc) + pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) * agg%wage_s(sc(gc),tc) * av_prod !* 0.5_dp
    else
        agg%pens_base(tc) = agg%pens_base(tc) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) * agg%wage_s(sc(gc),tc) * av_prod !* 0.5_dp
    endif
    
enddo


! net_wage income / after tax earnings (so net of all taxes)
net_wage_inc=totlabinc - & 
    fun_hsv_new(wage_totax(1)*hrs1+ wage_totax(2)*hrs2, & 
    wage_totax(1)*hrs1, & 
    wage_totax(2)*hrs2,hrs1+hrs2,hrs1,hrs2,1,1,1,ec,dc,1,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,ind_below) !,avearn_base,ind_below) 
agg_vec(7)=agg_vec(7)+wght*net_wage_inc

!if (grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc)<=0.0_dp .and. grid%amin_age(gc,sc,1,jc)<=0.0_dp .and. grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc) - grid%amin_age(gc,sc,1,jc)<-0.00001_dp)then
if (abs(grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) -grid%amin_age(1,max(sc1,sc2),1,jc))<epsi)then 
   agg%indbcexact_j(jc,tc) = agg%indbcexact_j(jc,tc) + wght
   !agg%indbcexact_gsxj(gc,sc,1,jc,tc) = agg%indbcexact_gsxj(gc,sc,1,jc,tc) + wght
   !agg%indbc_xj(xc_dec,jc,tc) = agg%indbc_xj(xc_dec,jc,tc) + wght
   !print*, "bc doesnt work", jc, grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc), grid%amin_age(gc,sc,1,jc)
endif

if ( grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) < net_wage_inc / 6.0_dp  )then 
   agg%indbc_j(jc,tc) = agg%indbc_j(jc,tc) + wght
   !agg%indbc_gsxj(gc,sc,1,jc,tc) = agg%indbc_gsxj(gc,sc,1,jc,tc) + wght
   !agg%indbc_xj(xc_dec,jc,tc) = agg%indbc_xj(xc_dec,jc,tc) + wght
   !print*, "bc doesnt work", jc, grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,scpar,jc,tc), grid%amin_age(gc,sc,1,jc)
endif 

!if (jc>=(js+1) .and. jc<=jr(tc) -1 )then
    !if (jc<=jr(tc) -1)then
    !    agg_vec(7)=agg_vec(7)+wght*wage_inc
    !endif
        
    agg%asswp(tc) = agg%asswp(tc) +wght*grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !ap
    agg%ky_j(jc,tc) =  agg%ky_j(jc,tc) + wght*grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)/net_wage_inc
!endif
agg%asswp_j(jc,tc) = agg%asswp_j(jc,tc) +wght*grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
agg%net_wage_inc_j(jc,tc) = agg%net_wage_inc_j(jc,tc) +wght*net_wage_inc 
  
agg%wage_inc_j(jc,tc) = agg%wage_inc_j(jc,tc) +wght*wage_inc

! total income
ass=grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
ass_inc=(ass)*agg%ret(tc) ! gross asset income
tot_inc=wage_inc + ass_inc ! gross !wage_inc
agg_vec(8)=agg_vec(8)+wght*tot_inc
agg%tot_inc_j(jc,tc) = agg%tot_inc_j(jc,tc) +wght*tot_inc
    
! disposable income:
net_ass_inc=ass*agg%ret(tc) - f_captax(ass,1.0_dp+agg%ret(tc),agg%tau_k(tc)) ! capital income taxes yet so gross is equal to net capital income
disp_inc=net_wage_inc+net_ass_inc
agg_vec(9)=agg_vec(9)+wght*disp_inc
! disp income by skill, cit group

! tax revenues
agg_vec(10)=agg_vec(10)+wght*pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)! labor
agg%labinctaxrv(tc) = agg%labinctaxrv(tc)+wght*pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
agg_vec(11)=agg_vec(11)+wght*pol%capinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)! cap
agg_vec(12)=agg_vec(12)+wght*pol%constaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) ! cons

agg%hsvtmp1(tc) = agg%hsvtmp1(tc) + wght*pol%hsvtmp1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
agg%hsvtmp2(tc) = agg%hsvtmp2(tc) + wght*pol%hsvtmp2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)

!print*, pol%labinctaxrv(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc), pol%hsvtmp1(dc,xc,pc,yc,ec,epsc,gc,kc,hc,sc,ickid,scpar,jc,tc)
!pause

agg%labinctaxrv_j(jc,tc) = agg%labinctaxrv_j(jc,tc) + wght*pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)! labor

agg%capinctaxrv_j(jc,tc) = agg%capinctaxrv_j(jc,tc) + wght*pol%capinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)! cap
agg%constaxrv_j(jc,tc) = agg%constaxrv_j(jc,tc) + wght*pol%constaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)! cons  

agg%pccontr_j(jc,tc) = agg%pccontr_j(jc,tc) + wght *sum( gr_wage ) * agg%tau_p(tc)  

! aggregate negative asset position
if ( ass<0.0_dp) then
    agg_vec(13)=agg_vec(13)+wght*ass
endif
    
! for aggregation checks: gross wage income by aggregating across households
agg_vec(14)=agg_vec(14)+wght*pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    
! for aggregation checks: total gross income by aggregating across households
agg_vec(15)=agg_vec(15)+wght*pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)

if ( (ec==1 .and. dc==nd) .or. ec==2)then
    ! aggregate pension beneefits
    agg_vec(17)=agg_vec(17)+wght*sum(gr_ben_p) ! ben_p
    
    ! pension tax revenues (net of transfers / minpens, thus can be negative)
    agg_vec(24)=agg_vec(24)+wght*pol%penstaxrv_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
   
    ! number of pensioners
    agg_vec(18)=agg_vec(18)+wght
endif
     
if (jc<jr(tc))then
    if (ec==1)then
        if (dc<nd  )then !.or. dc==nd)then
        
            ! this is for average earmomgs, that's why net of early retirees
            agg_vec(30)= agg_vec(30) +wght
           
            ! gross wage income
            agg_vec(21)=agg_vec(21)+wght*(totlabinc)
        endif
    
    endif
    
endif


if (jc>=jf .and. jc<jt)then 
  
        
    ! parent population
    agg%poppar(tc) = agg%poppar(tc) + wght
    ! aggregate monetary inv PER CHILD
    agg%moninv(tc) =  agg%moninv(tc)  + wght * moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    ! aggregate time inv PER CHILD
    agg%tinv(tc) = agg%tinv(tc)  + wght * tinvtot
    
    agg%moninvratio(tc) =agg%moninvratio(tc)  + wght * moninvratio 
   
    ! per child money and time inv by age 
    agg%m_j(jc,tc) = agg%m_j(jc,tc) + wght *moninv ! pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%t_j(jc,tc) = agg%t_j(jc,tc) + wght * tinvtot
    
    agg%m_yj(yc1,jc,tc) = agg%m_yj(yc1,jc,tc) + wght * moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) 
    agg%t_yj(yc1,jc,tc) = agg%t_yj(yc1,jc,tc) + wght * tinvtot
    
    hp1 = f_hh_tp1(grid%hk_grid(pc),gamma_s(sc1),moninv  , &
                                            pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) +pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) ,jc-jf+1,inv_ces_p) 
    agg%inv_j(jc,tc) = agg%inv_j(jc,tc) + wght * inv_ces_p
    
    ! per child money and time inv by skill
    agg%t_s(sc1,tc) = agg%t_s(sc1,tc) + wght * (pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) ) !* 0.5_dp
    agg%t_s(sc2,tc) = agg%t_s(sc2,tc) + wght * (pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) ) !* 0.5_dp
    
    agg%m_s(sc1,tc) = agg%m_s(sc1,tc) + wght * moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !* 0.5_dp
    agg%m_s(sc2,tc) = agg%m_s(sc2,tc) + wght * moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !* 0.5_dp
    ! parent population by skill
    agg%poppar_s(sc1,tc) = agg%poppar_s(sc1,tc) + wght !*0.5_dp
    agg%poppar_s(sc2,tc) = agg%poppar_s(sc2,tc) + wght !*0.5_dp
    
    ! parent population by skill, age
    agg%poppar_sj(sc1,jc,tc) = agg%poppar_sj(sc1,jc,tc) + wght !*0.5_dp
    agg%poppar_sj(sc2,jc,tc) = agg%poppar_sj(sc2,jc,tc) + wght !*0.5_dp
    ! parent population by skill, marital  status, age
    agg%poppar_sgj_cpl(max(sc1,sc2),1,jc,tc) = agg%poppar_sgj_cpl(max(sc1,sc2),1,jc,tc) + wght
    agg%poppar_sgj_cpl(sc2,2,jc,tc) = agg%poppar_sgj_cpl(sc2,2,jc,tc) + wght
    
    agg%poppar_ssgj_cpl(sc1,sc2,1,jc,tc) = agg%poppar_ssgj_cpl(sc1,sc2,1,jc,tc) + wght
    agg%poppar_ssgj_cpl(sc1,sc2,2,jc,tc) = agg%poppar_ssgj_cpl(sc1,sc2,2,jc,tc) + wght
    !
    ! per child money and time inv by skill, age
    agg%m_sj(sc1,jc,tc) = agg%m_sj(sc1,jc,tc) + wght* moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !*0.5_dp
    agg%m_sj(sc2,jc,tc) = agg%m_sj(sc2,jc,tc) + wght* moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) !*0.5_dp
    agg%t_sj(sc1,jc,tc) = agg%t_sj(sc1,jc,tc) + wght* (pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)  )
    agg%t_sj(sc2,jc,tc) = agg%t_sj(sc2,jc,tc) + wght* (pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)  )
    !
    agg%h_sj(sc1,jc,tc) = agg%h_sj(sc1,jc,tc) + wght* grid%hk_grid(pc) 
    agg%h_sj(sc2,jc,tc) = agg%h_sj(sc2,jc,tc) + wght* grid%hk_grid(pc) 
    
    agg%h_sgj_cpl(max(sc1,sc2),1,jc,tc) = agg%h_sgj_cpl(max(sc1,sc2),1,jc,tc) + wght* grid%hk_grid(pc) 
    agg%h_sgj_cpl(sc2,2,jc,tc) = agg%h_sgj_cpl(sc2,2,jc,tc) + wght* grid%hk_grid(pc) 
      
    
    ! per child money and time inv by skill, marital status, age
    agg%m_sgj_cpl(max(sc1,sc2),1,jc,tc) = agg%m_sgj_cpl(max(sc1,sc2),1,jc,tc) + wght* moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%m_sgj_cpl(sc2,2,jc,tc) = agg%m_sgj_cpl(sc2,2,jc,tc) + wght* moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%t_sgj_cpl(max(sc1,sc2),1,jc,tc) = agg%t_sgj_cpl(max(sc1,sc2),1,jc,tc) + wght* (pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) + pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc) )
    agg%t_sgj_cpl(sc2,2,jc,tc) = agg%t_sgj_cpl(sc2,2,jc,tc) + wght* (pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)  )
    
    agg%m_ssgj_cpl(sc1,sc2,1,jc,tc) = agg%m_ssgj_cpl(sc1,sc2,1,jc,tc) + wght* moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%m_ssgj_cpl(sc1,sc2,2,jc,tc) = agg%m_ssgj_cpl(sc1,sc2,2,jc,tc) + wght* moninv !pol%m_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    agg%t_ssgj_cpl(sc1,sc2,1,jc,tc) = agg%t_ssgj_cpl(sc1,sc2,1,jc,tc) + wght* (pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)  )
    agg%t_ssgj_cpl(sc1,sc2,2,jc,tc) = agg%t_ssgj_cpl(sc1,sc2,2,jc,tc) + wght* (pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)  )
    
    !if (wght> 0.0_dp ) print*, "t cpl", pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc), pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,jc,tc)
    !
    
    ! acuqired HK at age j   
    agg%h_j(jc,tc) = agg%h_j(jc,tc) + wght * grid%hk_grid(pc)
   
    ! parent population by age
    !agg%poppar_j(jc,tc) = agg%poppar_j(jc,tc) + wght 
    
    ! governemnt expenditures on education [primary plus secondary, PER CHILD, thus multiply by number of children]
    agg%igov(tc) = agg%igov(tc) + wght * igov_j(jc-jf + 1) * demo%numchild_gsj(2,sc2,jc,tc) 
    if (jc==jf) agg%igov_kg(tc) = agg%igov_kg(tc) + wght * igov_j(jc-jf + 1) * demo%numchild_gsj(2,sc2,jc,tc) 
    if (jc>jf) agg%igov_hs(tc) = agg%igov_hs(tc) + wght * igov_j(jc-jf + 1) * demo%numchild_gsj(2,sc2,jc,tc) 
    if (jc>jf) agg%igov_hs_base(tc) = agg%igov_hs_base(tc) + wght * igovbase_j(jc-jf + 1) * demo%numchild_gsj(2,sc2,jc,tc)
     
elseif (jc==jt)then 

    !agg%igov(tc) = agg%igov(tc) + wght * igov_frac *earn_si_lo * demo%numchild_gsj(2,sc2,jc,tc)
     
    ! parent population at ivt period
    agg%poppar_ivt(tc) = agg%poppar_ivt(tc) + wght
    ! aggregate ivt transfers FOR ALL CHILDREN in hh (used to computed ivt transfers as a fraction of assets)
    agg%ivt(tc) = agg%ivt(tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc)  * demo%numchild_gsj(2,sc2,jc,tc)

    agg%ivt_pkid(tc) = agg%ivt_pkid(tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc) 


    ! per child ivt transfsers by skill group
    agg%ivt_s(sc1,tc) = agg%ivt_s(sc1,tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc) !*0.5_dp
    agg%ivt_s(sc2,tc) = agg%ivt_s(sc2,tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc) !*0.5_dp
    ! per child ivt transfers by marital status and skill group
    agg%ivt_sg_cpl(max(sc1,sc2),1,tc) = agg%ivt_sg_cpl(max(sc1,sc2),1,tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc) !*0.5_dp
    agg%ivt_sg_cpl(sc2,2,tc) = agg%ivt_sg_cpl(sc2,2,tc) + wght * pol%b_cpl(dc,xc,pc,yc1,yc2,ec,epsc1,epsc2,kc1,kc2,sc1,sc2,tc)  !*0.5_dp
    
    ! parent population by age
    !agg%poppar_j(jc,tc) = agg%poppar_j(jc,tc) + wght 
    agg%poppar_sj(sc1,jc,tc) = agg%poppar_sj(sc1,jc,tc) + wght !* 0.5_dp
    agg%poppar_sj(sc2,jc,tc) = agg%poppar_sj(sc2,jc,tc) + wght !* 0.5_dp
    ! parent population by skill, marital  status, age
    agg%poppar_sgj_cpl(max(sc1,sc2),1,jc,tc) = agg%poppar_sgj_cpl(max(sc1,sc2),1,jc,tc) + wght
    agg%poppar_sgj_cpl(sc2,2,jc,tc) = agg%poppar_sgj_cpl(sc2,2,jc,tc) + wght
    
    ! parent population by age
   ! agg%poppar_j(jc+1,tc) = agg%poppar_j(jc+1,tc) + wght 
    agg%poppar_sj(sc1,jc+1,tc) = agg%poppar_sj(sc1,jc+1,tc) + wght
    agg%poppar_sj(sc2,jc+1,tc) = agg%poppar_sj(sc2,jc+1,tc) + wght
    ! parent population by skill, marital  status, age
    agg%poppar_sgj_cpl(max(sc1,sc2),1,jc+1,tc) = agg%poppar_sgj_cpl(max(sc1,sc2),1,jc+1,tc) + wght
    agg%poppar_sgj_cpl(max(sc1,sc2),2,jc+1,tc) = agg%poppar_sgj_cpl(max(sc1,sc2),2,jc+1,tc) + wght
    
    ! acuqired HK at age j
    agg%h_j(jc,tc) = agg%h_j(jc,tc) + wght * grid%hk_grid(pc)
    agg%h_sj(sc1,jc,tc) = agg%h_sj(sc1,jc,tc) + wght* grid%hk_grid(pc) !*0.5_dp
    agg%h_sj(sc2,jc,tc) = agg%h_sj(sc2,jc,tc) + wght* grid%hk_grid(pc) !*0.5_dp
    agg%h_sgj_cpl(max(sc1,sc2),1,jc,tc) = agg%h_sgj_cpl(max(sc1,sc2),1,jc,tc) + wght* grid%hk_grid(pc) 
    agg%h_sgj_cpl(sc2,2,jc,tc) = agg%h_sgj_cpl(sc2,2,jc,tc) + wght* grid%hk_grid(pc) 
    
    agg%h_j(jc+1,tcp1) = agg%h_j(jc+1,tcp1) + wght * grid%hk_grid(pc)
   
endif

end subroutine sub_aggr_out_cpl
! ---------------------------------------------------------------------------   
!
!! ------------------------------------------------------------------- 
!subroutine sub_checkbudget(cons,lab,sav,ass,tr,net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,gc,jc,tc,numchld,avearn,tau_c,tau_k,flag_stud,flag_bc)
!                                        
!implicit none 
!                                        
!real(dp),intent(in)::cons,lab,sav,ass,tr,net_wage,wage_totax,ben_p,R,numchld,avearn,tau_c,tau_k
!integer,intent(in)::ic,ec,dc,gc,jc,tc,flag_stud
!logical,intent(out):: flag_bc 
!logical,intent(in)::ind_bc
!real(dp)::tot_res,tot_exp,dist
!real(dp),parameter::epsi=1.0e-06
!integer:: ndlloc
!flag_bc = .false.
!ndlloc = ndl
!                                    
!tot_res=(ass+tr)* f_aftertaxR(R,tau_k)
!
!if (ec==1 .and. dc<=ndlloc) then
!  
!    tot_res=tot_res+net_wage * biggam
!    
!    
!elseif ( (ec==1 .and. dc==ndlloc+1))then
!    print*, "should not be here2"
!    pause
!else !then
!    tot_res=tot_res+ben_p
!endif
!                                        
!tot_exp=cons*(1.0_dp+tau_c) 
!if (ec==1 .and. dc<=ndlloc) then
!    
!    tot_exp=tot_exp+net_wage*(biggam-lab) + fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,numchld,avearn,avearn_base )  
!   
!endif
!
!!if (ind_bc==0) then   ! add expenditures on savings
!    ! recall the dynamic budget constraint from standard model:
!    ! S+C+w*Leis = A(1+r)+w
!    tot_exp=tot_exp+sav
!!endif
!    
!if (flag_stud==2)then
!    tot_exp = tot_exp + fee_flow - col_subs_flow
!endif
!                                        
!dist=abs(tot_exp-tot_res)   ! take absolute rather than relative distance measure b/c you may have very small numbers
!
!if ( dist>epsi ) then
!    print*, 'budget constraint does not hold sing',ind_bc,ec,dc,dist,tot_exp,tot_res,tr,ben_p,cons,sav,lab,jc
!    print*, "seee", f_aftertaxR(R,tau_k) * ass+ ben_p, cons*(1.0_dp+tau_c) + sav
!    flag_bc = .true.
!   ! pause
!endif
!                                        
!end subroutine sub_checkbudget
!! -------------------------------------------------------------------     
!    
 
! ---------------------------------------------------------------------------    
subroutine sub_outwageret(tc,gdp,cap,lab,lab_s,wage,wage_s,ret,lam)
! computes output, wages, wages by types, aggregate return
implicit none
    
integer,intent(in)::tc
real(dp),intent(in)::lab_s(ns),lam
real(dp),intent(inout)::gdp,lab,wage,wage_s(ns),ret,cap

integer::sc,ic,scc,icc
real(dp)::capint,temp,mpl,avg_mpl,mpl_s(ns),ces_lab,wght(ni,ns),capint_soe,lab_no
real(dp),parameter::epsi=1.0e-03

! compute labor and ces aggregate of labor by skill and citizenship
lab=0.0_dp
ces_lab=0.0_dp

lab_no = sum(lab_s(1:3))

lab = lab_no + lab_s(4) 

if ( abs(rhho_s-1.0)>epsi ) ces_lab=lab_no**rhho_s+lab_s(4)**rhho_s     ! aggregated CES labor: aggregation with skill-specific s.e.

if ( abs(rhho_s-1.0)>epsi ) then
    ces_lab=ces_lab**(1.0/rhho_s)   ! outer aggregation to CES
else
    ces_lab=lab
endif

     
if (opt_pe<1 .and. opt_ge_upd==1)then

    ! compute wages by skill 
    ! sc = 2
    if ( abs(rhho_s-1.0)>epsi ) then
        temp = 0.0_dp
       
        temp = temp + lab_s(ns)**rhho_s
        
        temp=( 1.0_dp + (temp/lab_no**rhho_s) )**( (1.0-rhho_s)/rhho_s )
        
    else
        temp = 1.0_dp
    endif
    ! RS factors in mpl(sc)
    mpl_s(1:3)=temp
    
    ! sc = 3
    if ( abs(rhho_s-1.0)>epsi ) then 
        temp = 0.0_dp
       
        temp = temp + lab_no**rhho_s
        
        temp=( 1.0_dp + (temp/lab_s(ns)**rhho_s) )**( (1.0-rhho_s)/rhho_s )
        
    else
        temp = 1.0_dp
    endif
    ! RS factors in mpl(sc)
    mpl_s(ns)=temp
    
    if (opt_soe)then
        capint_soe = f_capint(ret)
        capint = capint_soe
        cap = capint * ces_lab
        !gamma_adj = 1.0_dp
        !af_tech = 1.0_dp
    else
        ! normalization: compute adjustment factor (relevant for case with imperfect substitutes)
        ! thereby: lab=af_tech*ces_lab so that cap/lab = cap/(af_tech*ces_lab), hence rate of return
        ! for given lab, ces_lab, is the same with perfect and imperfect substitutes
        if (opt_init_ss .and. tc==tclb .and. opt_trref==0) af_tech=lab/ces_lab !* lam !if (tc==1) af_tech=lab/ces_lab

        ! add aggregate wage component
        capint=cap/(af_tech*ces_lab)

        if (opt_init_ss .and. tc==tclb .and. opt_trref==0) gamma_adj =  ( (1.0_dp-alph)**(1.0_dp/(1.0_dp-alph)) * capint**(alph/(1.0_dp-alph)) * lvl_ref**(1.0_dp/(alph-1.0_dp)) )**(-1.0_dp) !*af_tech**(1.0_dp/(alph-1.0_dp))
     !if (tc==1) gamma_adj =  ( (1.0_dp-alph)**(1.0_dp/(1.0_dp-alph)) * capint**(alph/(1.0_dp-alph)) * lvl_ref**(1.0_dp/(alph-1.0_dp)) )**(-1.0_dp) !*af_tech**(1.0_dp/(alph-1.0_dp))

    endif

    if (opt_prod_cb .or. abs(se_kl-1.0_dp)<epsi )then
        mpl=(1.0_dp-alph)*capint**alph * gamma_adj**(1.0_dp-alph)    ! aggregate MPL component
    else
        mpl = ( alph*cap**rhho_kl + (1.0_dp-alph)*(af_tech*ces_lab)**rhho_kl )**( (1.0_dp-rhho_kl)/rhho_kl )  &
            * (1.0_dp-alph) * (af_tech*ces_lab)**(rhho_kl-1.0_dp)  !* gamma_adj**alph ACHTUNG TBC
    endif
    ! add aggregate wage component

    mpl_s(:)=mpl*mpl_s(:)         ! add aggregate MPL component

    wage=af_tech*mpl
    wage_s(:)=af_tech*mpl_s(:)   ! scale wages with adjustment factor
    
else
    
    if (opt_init_ss .and. tc==tclb .and. opt_trref==0) af_tech=lab/ces_lab  !* lam !if (tc==1) af_tech=lab/ces_lab

    ! add aggregate wage component
    capint=cap/(af_tech*ces_lab)

    if (opt_init_ss .and. tc==tclb .and. opt_trref==0) gamma_adj =  ( (1.0_dp-alph)**(1.0_dp/(1.0_dp-alph)) * capint**(alph/(1.0_dp-alph)) * lvl_ref**(1.0_dp/(alph-1.0_dp)) )**(-1.0_dp) !*af_tech**(1.0_dp/(alph-1.0_dp))
    
endif




! GDP given the CES aggregate of labor:
if (opt_prod_cb .or. abs(se_kl-1.0_dp)<epsi)then
    gdp = cap**alph * (af_tech*ces_lab *gamma_adj )**(1.0_dp-alph)
    
    !temp = 0.0_dp
    !do sc=1,ns
    !    
    !    temp=temp+wage_s(sc)*lab_s(sc) 
    !    
    !end do  
    !
    !gdp = (ret + delt) * cap + temp 

    
else
    gdp = ( alph * cap**rhho_kl + (1.0_dp-alph) * (af_tech*ces_lab)**rhho_kl )**(1.0/rhho_kl) ! achtung tbc normalizations
endif

if (opt_pe<1 .and. opt_ge_upd==1 .and. opt_soe==0)then
    ! aggregate return, given capital intensity:
    if (opt_prod_cb .or. abs(se_kl-1.0_dp)<epsi)then
        ret=alph*  (capint )**(alph-1.0_dp) *gamma_adj**(1.0_dp-alph )-delt ! achtung added adj 
    else
        ret = ( alph*cap**rhho_kl + (1.0_dp-alph)*(af_tech*ces_lab)**rhho_kl )**( (1.0_dp-rhho_kl)/rhho_kl ) * (alph) * cap**(rhho_kl-1.0_dp) - delt ! achtung adj
    endif
endif

end subroutine sub_outwageret
! ---------------------------------------------------------------------------   

! ---------------------------------------------------------------------------    
subroutine sub_lc_call(agg,demo,grid,lc,pol,t0,t1)

implicit none
type(t_agg),intent(in)::agg 
type(t_demo),intent(in)::demo
type(t_grid),intent(in)::grid
type(t_lc),intent(inout)::lc
type(t_pol),intent(in)::pol
integer,intent(in)::t0,t1

integer::tc,tcp1,sc,kc,jc,ic,gc
integer:: sc1,sc2,kc1,kc2
real(dp)::frac,wght_j,wght_ijs,temp,lab,betta_til,R_til_tp1
logical,parameter::opt_chk=.true.
character(1)::i_char_sc,i_char_ic
real(dp)::cons_gr(nj),cons_gr_det(nj)   ! consumption growth in deterministic model
real(dp),allocatable:: dc_vec(:)
integer:: dcl,dc_temp

do tc=t0,t1
    if (t0==t1) then
        tcp1=t0
    else
        tcp1=min(tc+1,nt)
    endif
        
    lc%ap_j(:,tc)=0.0_dp
    lc%cons_j(:,tc)=0.0_dp
    lc%m_j(:,tc)=0.0_dp
    lc%t_j(:,tc)=0.0_dp
    lc%lab_j(:,tc)=0.0_dp
    lc%lab1_j(:,tc)=0.0_dp
    lc%lab2_j(:,tc)=0.0_dp
    lc%coh_j(:,tc)=0.0_dp
    lc%indbc_j(:,tc)=0.0_dp
    
    ! first compute lc profiles for singles only (from js to nj)
    !!$OMP PARALLEL private(sc,kc)
    !!$OMP DO 
    do gc=1,ng
        do sc=1,ns
            do kc=1,nk
            
                call sub_lc(demo,grid,lc,pol,kc,sc,gc,tcp1,tc)
                
            enddo 
        enddo
    enddo
    !!$OMP END DO
    !!$OMP END PARALLEL
    !
    !! now compute lc profiles for couples and add everything up
    !do sc2=1,ns
    !    do sc1=1,ns
    !        do kc2=1,nk
    !            do kc1=1,nk
    !                
    !                call sub_lc_cpl(demo,grid,lc,pol,kc1,kc2,sc1,sc2,tcp1,tc)
    !                
    !            enddo
    !        enddo
    !    enddo
    !enddo
    !
    !
    !
    if (opt_chk) then
        
        print*, "lab", lc%lab_j(:,tc)
        pause
        
        print*, "indbc", lc%indbc_j(:,tc)
        print*, ' '
        
        call plot_y(lc%ap_j(:,tc),'ap','savings')
        call execplot()
        
        betta_til=betta*(1.0_dp+demo%lamt(tc))**(1.0_dp-tetta)
        R_til_tp1=(1.0_dp+agg%ret(tcp1))/(1.0_dp+demo%lamt(tc))
        cons_gr_det(:)=(betta_til*R_til_tp1)**(1.0_dp/tetta)
        !call plot_y(cons_gr_det(:),'cons-gr-det','consumption and consumption growth')
        !call plot_y(lc%cons_j(:,tc),'cons')
        !call execplot()
        
        call plot_y( lc%cons_j(:,tc),'cons','consumption')
        call execplot()
       
        cons_gr(js:nj-1)=lc%cons_j(js+1:nj,tc)/lc%cons_j(js:nj-1,tc)
        print*, "cons_gr", cons_gr(:)
        print*, ' '
        call plot_y(cons_gr_det(js:nj-1),'cons-gr-det','consumption growth')
        call plot_y(cons_gr(js:nj-1),'cons-gr')
        call execplot()
        
        !call plot_y(lc%indbc_j(:,tc),'frac-bc','fraction at borrowing constraint')
        !call execplot()

        call plot_y(lc%coh_j(:,tc),'coh','cash on hand')
        call execplot()

        call plot_y(lc%lab_j(:,tc),'lab','labor')
        call execplot()
        
        call plot_y(lc%m_j(:,tc),'m','mon inv')
        call execplot()
        
        call plot_y(lc%t_j(:,tc),'t','time inv')
        call execplot()
        
        
    endif
enddo   

end subroutine sub_lc_call
! ---------------------------------------------------------------------------    
    

! ---------------------------------------------------------------------------    
subroutine sub_lc(demo,grid,lc,pol,kc,sc,gc,tcp1,tc)
! computation of life-cycle profiles, respectively cross-sectional profiles

implicit none
type(t_demo),intent(in)::demo
type(t_grid),intent(in)::grid
type(t_lc),intent(inout)::lc
type(t_pol),intent(in)::pol
integer,intent(in)::kc,sc,tcp1,tc,gc

integer::jc,ic,ec,yc,pc,xc,dc,qc,ickid,gckid,hc
real(dp)::frac,wght_j,wght_ijs,wght_gijs,temp,lab
logical,parameter::opt_chk=.true.
real(dp)::cons_gr(nj),cons_gr_det(nj)   ! consumption growth in deterministic model
real(dp),allocatable:: dc_vec(:)
integer:: dcl,dc_temp


do jc=js,nj
    
    do gckid = 1,2
        do ickid = 1,ni
            
            do hc=1,np
   
                do ic=1,ni    
                    do ec=1,ne
                       
                        do yc=1,ny
                            do pc=1,np
                                do xc=1,nx
                       
                                    
                                    do dc=1,nd
                            
                                        frac=grid%Phi(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)    
                            
                            
                                        !if (jc<js .and. hc>1)then
                                        !    print*, "should not be"
                                        !    pause
                                        !endif
                                                      
                                        if (frac==0.0) cycle
                                        ! if (jc<jm)then
                                        wght_j=frac/sum(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc))
                                        wght_ijs=frac/sum(grid%Phi(:,:,:,:,:,ic,:,:,:,sc,:,:,jc,tc))
                                        wght_gijs=frac/sum(grid%Phi(:,:,:,:,:,ic,gc,:,:,sc,:,:,jc,tc))
                                        
                                        temp=grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)/(1.0+demo%lamt(tc))
                                       
                                        lc%ap_j(jc,tc)=lc%ap_j(jc,tc)+wght_j*temp
                                        
                                        lab=pol%lab(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        lc%lab_j(jc,tc)=lc%lab_j(jc,tc)+wght_j*lab
                                        
                                        lc%cons_j(jc,tc)=lc%cons_j(jc,tc)+wght_j*pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                       
                                        lc%coh_j(jc,tc)=lc%coh_j(jc,tc)+wght_j*grid%coh(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        
                                        lc%indbc_j(jc,tc)=lc%indbc_j(jc,tc)+wght_j*real(pol%indbc(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc))
                                        
                                        lc%m_j(jc,tc)=lc%m_j(jc,tc)+wght_j*pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        lc%t_j(jc,tc)=lc%t_j(jc,tc)+wght_j*pol%t1(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                            
                            
                                    enddo
                                   
                                end do
                            end do
                        end do
            
                    end do
                end do
            enddo
        enddo
    enddo
    
end do

end subroutine sub_lc
! ---------------------------------------------------------------------------    

    
    
end module aggrpar_full_mod
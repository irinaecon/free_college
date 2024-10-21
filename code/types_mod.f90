module types_mod
! -----------------------------------------------
! This module defines types as allocatable arrays
! -----------------------------------------------
    
use nrtype
use params_mod
  
implicit  none    
  
! aggregate variables  
type t_agg
    real(dp),allocatable,dimension(:):: grwage,pens_base,debtss,inc_thrs,igov,igov_kg,igov_hs,igov_hs_base,clsubs,privclsubs,clcost,clcosty,cap,gdp,gdpn,cons,inv,gcons,gcons_net,lab,lab1,lab2,ret,ret_soe,hrs,hrs1,hrs2,hrs1_calib,hrs2_calib,pens,bq,bqr,grsav,negass,gconsr,traclp,lambdapr,tau_pr,edu_spend_fac,avtax,avtax_av,margtaxbot,avtaxbot,avtaxbot_frac,avtaxrat
    real(dp),allocatable,dimension(:):: ben_h,tau_h,tau_k,tau_c,uben_param,aben_param,tau_rem,lambda_pr, gyr, byr,debt,totrev,def,dyr, labinctaxrvyr, psi_lambda,avhrs,nu,asswp,hc_foc_error_j, rv_pdv, & 
        pccontr_pdv, probcl,lsra,ass,sav,lsra_j,regretcl, valuedrop, & 
        clshr_tot, hk_tot, moninv_tot, tinv_tot, pop_tot, ivt_tot, h0delta
    real(dp),allocatable,dimension(:):: emprate,fracpens, emppop, allpopstock, popfull, welf_t, welf_j, welf_j_nokid, phi_hc
    real(dp),allocatable,dimension(:,:):: frac_gs, frishela,pop_frisch,ksi, pop_kid_s, net_trr_j, wage_hc, phi_shc, indbc_j,indbcexact_j,pop_j,pop_j_temp,cev_s,hkja_s,hkjacol_s,popja_s,popjacol_s,  & 
        avprem,marprem,marprem_pop,avprem_pop, probcl_h, wage_inc_s, fe_s, pop_s, tinv_tot_g, hk_tot_j, phi_s_tot, v_kid,v_delta
    real(dp),allocatable,dimension(:,:,:):: earn_gs,pop_gs,emppopref_all_gjt,emprateref_all_gjt,allpopstock_gjt,lab_sj,cons_sj,ass_sj,frac_bc_sj, frac_nettr_sj,labinc_sj,totinc_sj,clshr_jasp, hk_jasp, phi_s_tot_j
    real(dp),allocatable,dimension(:):: totrv, labinctaxrv,hsvtmp1,hsvtmp2,capinctaxrv,constaxrv,penstaxrv, At,earn, earn_ft, hrs_ft, hr_sh
    real(dp),allocatable,dimension(:):: wage,avwage,avearn,ben_u,ben_p,hcontr,pcontr,wage_inc,wage_inc_hh,net_wage_inc  ! wages,unemplyoment, pension contributions and benefits
    real(dp),allocatable,dimension(:):: tot_inc,tot_inc_hh,disp_inc     ! income measures
    real(dp),allocatable,dimension(:):: tau_p,rho_p,kappafee,varrho,sf,abar,abar_hs,kappah,kappap4,kappah8,kappat,h0norm,kappai1,kappai2,kappah1,kappah2,fi1,hk,fh0_no, fh0_cl , iota, betta1, betta2, betta0 , betta1_str, betta2_str,metta0, metta1, metta2, metta1_str, metta2_str, clcosty_str
    real(dp),allocatable,dimension(:):: lhsfrac,dropfrac,clfrac,varwage,lhsfracbase,clfracbase,clkid_lhs,clkid_cl,tgrad,alpha0,alpha1,omega_fin,gamdelta,psi_ls
    real(dp),allocatable,dimension(:,:)::h0distr, lab_s,lab1_s,lab2_s,wage_s,m_j, t_j, ivt_s, ivt_skid, h_j, poppar_j, t_s,m_s, poppar_s, cons_j,avtax_j,avtax_av_j,ass_j,lab_j,zeta_cl, hr_sh_sp,inv_j,tot_inc_j,labinctaxrv_j,capinctaxrv_j,constaxrv_j,pccontr_j
    real(dp),allocatable,dimension(:,:,:):: poppar_sj,pop_sj,poppar_skidj,t_sj ,m_sj,h_sj, ivt_sg, ivt_sg_cpl ,hr_sh_gp_sp   
    real(dp),allocatable,dimension(:,:,:,:):: pia0,poppar_sgj,poppar_sgj_cpl,t_sgj ,m_sgj, h_sgj,t_sgj_cpl ,m_sgj_cpl, h_sgj_cpl, educ_matrix_g, pop_jasp
    real(dp),allocatable,dimension(:,:,:,:,:):: educ_matrix_mg,t_ssgj_cpl ,m_ssgj_cpl, h_ssgj_cpl,poppar_ssgj_cpl
    real(dp),allocatable,dimension(:,:,:):: tr_sj, educ_matrix
    real(dp),allocatable,dimension(:):: bhlev ! bhlev is eq object 
    real(dp),allocatable,dimension(:):: beta,ky,ky_str,ivty,clsubsy,ivty_str,hrs_str,gyr_str,byr_str,moninv_str,tinv_str,tinv0_str,tinv_slope_str,moninv_slope_str,tgrad_str,lhsfrac_str,clfrac_str,dropfrac_str,varwage_str,clkid_cl_str,clkid_lhs_str,hk_str,hk_hs_str,scalfac,scalfac_wp,scalfac_old_wp,ky_av,hrs_fem_av,hrs_mal_av,gyr_av
    real(dp),allocatable,dimension(:):: poppar,poppar_inv, poppar_ivt, moninv, tinv, moninvratio, tinv_slope,moninv_slope, ivt,ivt_pkid,rhoedu0,rhoearn0, amin_param
    real(dp),allocatable,dimension(:,:):: ky_j,net_wage_inc_j,asswp_j,wage_inc_j
    real(dp),allocatable,dimension(:)::hmed,inv_mean,earn3040,earnLT,earn3040net,earnLTnet,phi
    real(dp),allocatable,dimension(:,:):: earnLT_j, earnLTnet_j, earnHS_j, earnLHS_jearnLHS_j_LEV, earnHS_j_LEV, Phi_s,totnetres_j
    real(dp),allocatable,dimension(:,:):: gammah_s,meanwage_s ,meanwage_s_str,slopewage_s,abgrad_s,hk_cutoff, earn3040_spar,earnLT_spar,earn3040_gpar,earnLT_gpar, earn3040net_spar,earnLTnet_spar,earn3040net_gpar,earnLTnet_gpar
    real(dp),allocatable,dimension(:,:,:):: earnLT_spar_j, earnLT_gpar_j, earnLTnet_spar_j, earnLTnet_gpar_j
    real(dp),allocatable,dimension(:,:,:):: earnLT_gspar,earn3040_gspar,earn3040net_gspar,earnLTnet_gspar, m_yj,h_yj,t_yj, conspdv_gs, conspdvbeta_gs, restotpdv_gs, restotpdvnet_gs
    real(dp),allocatable,dimension(:,:,:,:):: earnLT_gspar_j, earnLTnet_gspar_j
    integer,allocatable,dimension(:,:,:):: hquant_s
end type t_agg

! demographics (including productivity)
type t_demo
    real(dp),allocatable,dimension(:,:,:)::ageprod
    real(dp),allocatable,dimension(:,:,:)::ageprod_dat
    real(dp),allocatable,dimension(:,:,:,:):: numchild_gsj 
    real(dp),allocatable,dimension(:,:):: frac_prod ,sr            ! fraction with productivity kc [prod,edu]
    real(dp),allocatable,dimension(:):: popgr
    real(dp),allocatable,dimension(:):: grid_y,grid_epsi,pini,pini_dat,prob_epsi                 ! income states: [yc], initial distr-n
    real(dp),allocatable,dimension(:,:):: prob_y  ,prob_y_dat                  ! probability of income states [yc]
    real(dp),allocatable,dimension(:):: prob_gc,frac_s, abgrad                     ! population shares by gender [gc]
    real(dp),allocatable,dimension(:,:,:,:)::probmatch
    real(dp),allocatable,dimension(:)::  lamt,popgrt,mc_input,betta1_dat, betta2_dat , betta0_dat, metta0_dat,metta1_dat, metta2_dat, time_j
    real(dp),allocatable,dimension(:,:):: sc_input,nkids_input,frac_edu,h0prop,pini_ld ,frac_jt
    real(dp),allocatable,dimension(:,:,:)::ass_input,ass_input_dat
    real(dp),allocatable,dimension(:,:,:,:)::hc0_input,prob_y_ld
end type t_demo

! grids
type t_grid
    ! singles
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:):: coh,cohd,coh_cpl,cohd_cpl,coh_old,coh_old_cpl,coh_exo,coh_exo_cpl,coh_labld,sav_labld,ass,ass_cpl,assmin,assmin_cpl,assmax,assmax_cpl,pstk,grpstk,Phi_cpl,Phi,sav,sav_cpl,Phi_int,coh_child,ass_child
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:):: Phi_guess,Phi_child, Phi_childja
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:):: coeff_sh_child,coeff_sh_child_sh
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:):: coh_guess,coh_guessm
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:):: ass_glb !,Phi_guess
    real(dp),allocatable,dimension(:,:,:,:,:,:):: savp
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:):: Phi2guess, sav2guess
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:):: Phi2guess_s
    real(dp),allocatable,dimension(:):: hk_grid,h0_grid,minv_grid,minv_grid_dat,tinv_grid,phi_edu_long,phi_inv_long,phi_earn_long
    
    real(dp),allocatable,dimension(:,:)::pc_edu_long, p_inv_long, pc_earn_long
    real(dp),allocatable,dimension(:,:,:)::  lab, Phi_s
    real(dp),allocatable,dimension(:,:,:)::lab_ge
    real(dp),allocatable,dimension(:,:,:,:):: amin_age
   
    !! couples
    !real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:):: coh_cpl,coh_base_cpl,ass_cpl,assmin_cpl,pstk_cpl,grpstk_cpl,Phi_cpl,Phi_cpl_base,sav_cpl,Phi_cpl_int,Phi_cpl_int_base
    !real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:):: ass_cpl_glob
    !real(dp),allocatable,dimension(:,:,:,:,:,:,:,:):: savp_cpl
    !real(dp),allocatable,dimension(:):: lab_cpl
    
end type t_grid

! life-cycle variables
type t_lc
    real(dp),allocatable,dimension(:,:):: cons_j,lab_j,lab1_j,lab2_j,ap_j,coh_j,indbc_j,  m_j, t_j
end type t_lc


! policy and value functions
type t_pol
    ! singles
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:):: v,v_cpl,vd,vd_cpl,dc,dc_cpl,v_nokid,v_nokid_cpl,inv_vp_x,inv_vp_x_cpl,evtp1_coh,evtp1_coh_cpl,inv_vp_pgr,cons,cons_cpl,leis,leis1_cpl,leis2_cpl, & 
        lab,labd,labtot_cpl,lab1_cpl,lab2_cpl,lab_eff,lab1_eff_cpl,lab2_eff_cpl,intp_inv_vp_x,intp_vp_pgr,t1,t1_cpl,t2_cpl,m,m_cpl,consd,consd_cpl,lab1d_cpl,lab2d_cpl, &    
        grwageinc,grwageinc_cpl,grwage,grwage_cpl,grinc,grinc_cpl,netinc,netinc_cpl,netsav,netsav_cpl,labinctaxrv,labinctaxrv_cpl,hsvtmp1,hsvtmp1_cpl,hsvtmp2,hsvtmp2_cpl,capinctaxrv,capinctaxrv_cpl,constaxrv,constaxrv_cpl,penstaxrv,penstaxrv_cpl, vder_child,v_child,inv_vp_x_child,cons_child
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:):: v_guess,v_guessm, b_cpl
    real(dp),allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:):: b,b_mod
    integer,allocatable,dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:):: indbc,indbc_cpl, net_trr,net_trr_cpl
   
end type t_pol


type t_pol_st
    real(dp),allocatable,dimension(:):: cons, sav, v, vp_x, inv_vp_x
end type t_pol_st

! summary statistics
type t_stat
    real(dp),allocatable,dimension(:)::gini_ass,gini_cons,gini_labrv,gini_inc,varlog_ass,varlog_cons,varlog_inc,deltacons_t,gainsstat
    real(dp),allocatable,dimension(:):: incmed, grincmed,fracpov
    real(dp),allocatable,dimension(:,:):: win_frac,lose_frac
    real(dp),allocatable,dimension(:,:,:,:):: deltacons,gbig,gain,loss
end type t_stat


    contains
    
    ! ----------------------------------------------------------------------------------------------------
    subroutine sub_alloc(agg,demo,grid,lc,pol,pol_st,stat)
    
    implicit none
    
    type(t_agg),intent(inout)::agg
    type(t_demo),intent(inout)::demo
    type(t_grid),intent(inout)::grid
    type(t_lc),intent(inout)::lc
    type(t_pol),intent(inout)::pol
    type(t_pol_st),intent(inout)::pol_st
    type(t_stat),intent(inout)::stat
   
    ! statistics
    allocate(stat%win_frac(nj,nt))
    allocate(stat%lose_frac(nj,nt))
    allocate(stat%gbig(4,ns,nj,nt))
    allocate(stat%gain(4,ns,nj,nt))
    allocate(stat%loss(4,ns,nj,nt))
    allocate(stat%deltacons(4,ns,nj,nt))
    allocate(stat%deltacons_t(nt))
    allocate(stat%gainsstat(nt))
    allocate(stat%incmed(nt))
    allocate(stat%grincmed(nt))
    allocate(stat%fracpov(nt))
   
    ! aggregate variables
    
    allocate(agg%phi_hc(np))
    allocate(agg%phi_shc(ns,np))
    allocate(agg%wage_hc(ns,np))
    
    allocate(agg%pens_base(nt))
    
    allocate(agg%debtss(nt))
    allocate(agg%clshr_jasp(ns,ns,nt))
    allocate(agg%hk_jasp(ns,ns,nt))
    allocate(agg%pop_jasp(np,ns,ns,nt))
    allocate(agg%pop_j(nj,nt))
    allocate(agg%pop_j_temp(nj,nt))
    allocate(agg%pop_sj(ns,nj,nt))
    
    allocate(agg%gyr(nt))
    allocate(agg%byr(nt))
    allocate(agg%debt(nt))
    allocate(agg%totrev(nt))
    allocate(agg%def(nt))
    allocate(agg%dyr(nt))
    allocate(agg%labinctaxrvyr(nt))
    allocate(agg%totrv(nt))
    allocate(agg%psi_lambda(nt))
    allocate(agg%ksi(2,nt))
    allocate(agg%frishela(2,nt))
    allocate(agg%pop_frisch(2,nt))

    allocate(agg%frac_gs(2,ns))
    
    allocate(agg%welf_t(nt))
    allocate(agg%welf_j(nj))
    allocate(agg%welf_j_nokid(nj))
    
    allocate(agg%earn_gs(2,ns,nt))
    allocate(agg%pop_gs(2,ns,nt))
    
    allocate(agg%hk_tot(nt))
    allocate(agg%hk_tot_j(nj,nt))
    allocate(agg%phi_s_tot(ns,nt))
    allocate(agg%phi_s_tot_j(ns,nj,nt))
    allocate(agg%moninv_tot(nt))
    allocate(agg%moninvratio(nt))
    allocate(agg%tinv_tot(nt))
    allocate(agg%tinv_tot_g(2,nt))
    allocate(agg%clshr_tot(nt))
    allocate(agg%pop_tot(nt))
    allocate(agg%ivt_tot(nt))
    
    allocate(agg%wage_inc_s(ns,nt))
    allocate(agg%fe_s(ns,nt))
    allocate(agg%pop_s(ns,nt))
    
    allocate(agg%edu_spend_fac(nt))
    
    allocate(agg%avhrs(nt))
    
    allocate(agg%emprate(nt))
   
    allocate(agg%emppop(nt))

    allocate(agg%lsra(nt))
    allocate(agg%lsra_j(nj))
    allocate(agg%ass(nt))
    allocate(agg%sav(nt))
    
    allocate(agg%cev_s(ns,nt))
    allocate(agg%hkja_s(ns,nt))
    allocate(agg%hkjacol_s(ns,nt))
    allocate(agg%popja_s(ns,nt))
    allocate(agg%popjacol_s(ns,nt))


    allocate(agg%pop_kid_s(ns,nt))
   
    allocate(agg%probcl_h(np,ndl * nx  * ny * nw * nk * ns))
    allocate(agg%probcl(np)) ! ,ndl * nx  * ny * nw * nk * ns))
    
    allocate(agg%conspdv_gs(2,ns,nj))
    allocate(agg%conspdvbeta_gs(2,ns,nj))
    allocate(agg%restotpdv_gs(2,ns,nj))
    allocate(agg%restotpdvnet_gs(2,ns,nj))
   
    allocate(agg%allpopstock(nt)) ! all working age population
    allocate(agg%popfull(nt))
    allocate(agg%allpopstock_gjt(2,nj,nt)) 
   
    allocate(agg%avtax(nt))
    allocate(agg%avtax_av(nt))
    allocate(agg%avtaxbot(nt))
    allocate(agg%margtaxbot(nt))
    allocate(agg%avtaxbot_frac(nt))
    allocate(agg%avtaxrat(nt))

    allocate(agg%fracpens(nt)) 
    allocate(agg%ky_av(1))
    allocate(agg%hrs_fem_av(1))
    allocate(agg%hrs_mal_av(1))
    allocate(agg%gyr_av(1))
    allocate(agg%scalfac(nprm_clb))
    allocate(agg%scalfac_wp(ns))
    allocate(agg%scalfac_old_wp(ns))
    allocate(agg%beta(nt))
    allocate(agg%ky(nt))
    allocate(agg%ky_j(nj,nt))
    allocate(agg%ky_str(nt))
    allocate(agg%ivty(nt))
    allocate(agg%ivty_str(nt))
    allocate(agg%clcosty_str(nt))
    allocate(agg%clsubsy(nt)) 
    allocate(agg%clcost(nt))
    allocate(agg%clcosty(nt))
    allocate(agg%gyr_str(nt))
    allocate(agg%byr_str(nt))
    
    allocate(agg%v_delta(nj,nt))
    allocate(agg%v_kid(nj,nt))
   
    !allocate(agg%hrs_mal_str(nt))
    allocate(agg%hrs_str(nt))
    allocate(agg%phi(nt))
    
    allocate(agg%lab_sj(ns,nj,nt))
    allocate(agg%cons_sj(ns,nj,nt))
    allocate(agg%frac_bc_sj(ns,nj,nt))
    allocate(agg%frac_nettr_sj(ns,nj,nt))
    allocate(agg%ass_sj(ns,nj,nt))
    allocate(agg%labinc_sj(ns,nj,nt))
    allocate(agg%totinc_sj(ns,nj,nt))

    allocate(demo%sr(nj,nt))
    
    allocate(agg%moninv_str(nt))
    allocate(agg%tinv_str(nt))
    allocate(agg%tinv0_str(nt))
    allocate(agg%tinv_slope_str(nt))
    allocate(agg%moninv_slope_str(nt))
    allocate(agg%tgrad_str(nt))
    allocate(agg%lhsfrac_str(nt))
    allocate(agg%clfrac_str(nt))
    allocate(agg%dropfrac_str(nt))
    allocate(agg%clkid_lhs_str(nt))
    allocate(agg%clkid_cl_str(nt))
    
    allocate(agg%varwage_str(nt))
    
    allocate(agg%hc_foc_error_j(nj))
    
    allocate(agg%hk_str(nt))
    allocate(agg%hk_hs_str(nt))

    allocate(agg%indbc_j(nj,nt))
    allocate(agg%indbcexact_j(nj,nt))
    
    allocate(agg%educ_matrix_mg(2,2,ns,ns,nt))
    allocate(agg%educ_matrix_g(2,ns,ns,nt))
    allocate(agg%educ_matrix(ns,ns,nt))
    
    allocate(agg%zeta_cl(2,nt))
    allocate(agg%omega_fin(nt))
    allocate(agg%gamdelta(nt))
    allocate(agg%psi_ls(nt))
    allocate(agg%fh0_cl(nt))
    allocate(agg%fh0_no(nt))
    
    allocate(agg%varwage(nt))
    
    allocate(agg%igov(nt))
    allocate(agg%igov_kg(nt))
    allocate(agg%igov_hs(nt))
    allocate(agg%igov_hs_base(nt))
    allocate(agg%clsubs(nt))
    allocate(agg%privclsubs(nt))
    allocate(agg%avprem(ns,nt))
    allocate(agg%avprem_pop(ns,nt))
    allocate(agg%marprem(ns,nt))
    allocate(agg%marprem_pop(ns,nt))
    allocate(agg%cap(nt))
    allocate(agg%gdp(nt))
    allocate(agg%gdpn(nt))
    allocate(agg%cons(nt))
    allocate(agg%cons_j(nj,nt))
    allocate(agg%ass_j(nj,nt))
    allocate(agg%lab_j(nj,nt))
    
    allocate(agg%avtax_j(nj,nt))
    allocate(agg%avtax_av_j(nj,nt))
  
    allocate(agg%inv(nt))
    allocate(agg%gcons(nt))
    allocate(agg%gcons_net(nt))
    allocate(agg%gconsr(nt))
    allocate(agg%ret(nt))
    allocate(agg%ret_soe(nt))
    allocate(agg%hrs(nt))
    allocate(agg%hrs1(nt))
    allocate(agg%hrs2(nt))
    allocate(agg%hrs1_calib(nt))
    allocate(agg%hrs2_calib(nt))
    allocate(agg%pens(nt))
    allocate(agg%bq(nt))
    allocate(agg%bqr(nt))
    allocate(agg%traclp(nt))
    allocate(agg%grsav(nt))
    allocate(agg%negass(nt))        ! test size of negative asset position  
    allocate(agg%labinctaxrv(nt)) 
    allocate(agg%hsvtmp1(nt)) 
    allocate(agg%hsvtmp2(nt))
    allocate(agg%capinctaxrv(nt))  
    allocate(agg%constaxrv(nt))  
    allocate(agg%labinctaxrv_j(nj,nt))  
    allocate(agg%capinctaxrv_j(nj,nt))  
    allocate(agg%constaxrv_j(nj,nt))  
    allocate(agg%pccontr_j(nj,nt))
    allocate(agg%penstaxrv(nt))
    
    allocate(agg%grwage(nt))
    
    
    allocate(agg%rv_pdv(nj))

    allocate(agg%pccontr_pdv(nj))
    
    allocate(agg%tau_p(nt))
    allocate(agg%rho_p(nt))
    allocate(agg%sf(10+ns+4))
    allocate(agg%lambdapr(15))
    
    allocate(agg%wage_inc(nt))
    allocate(agg%wage_inc_hh(nt))
    allocate(agg%net_wage_inc(nt))
    allocate(agg%net_wage_inc_j(nj,nt))
    allocate(agg%wage_inc_j(nj,nt))
    allocate(agg%tot_inc(nt))
    allocate(agg%tot_inc_j(nj,nt))
    allocate(agg%tot_inc_hh(nt))
    allocate(agg%disp_inc(nt))
    allocate(agg%ben_u(nt))
    allocate(agg%hcontr(nt))
    allocate(agg%ben_p(nt))
    allocate(agg%pcontr(nt))
    allocate(agg%tr_sj(ns,nj,nt))
    
    allocate(agg%bhlev(nt))
    
    allocate(agg%hmed(nt))
    allocate(agg%hk_cutoff(2,nt))
    allocate(agg%gammah_s(ns,nt))
    allocate(agg%meanwage_s(ns,nt))
    allocate(agg%slopewage_s(ns,nt))
    allocate(agg%meanwage_s_str(ns,nt))
    allocate(agg%abgrad_s(3,ns))
    allocate(agg%hquant_s(2,ns,nt))   
     
    allocate(agg%lab(nt))
    allocate(agg%lab_s(ns,nt))
    allocate(agg%lab1(nt))
    allocate(agg%lab1_s(ns,nt)) 
    allocate(agg%lab2(nt))
    allocate(agg%lab2_s(ns,nt))
   
    allocate(agg%wage(nt))
    allocate(agg%avwage(nt))
    allocate(agg%avearn(nt))
    allocate(agg%hrs_ft(nt))
    allocate(agg%earn_ft(nt))
    allocate(agg%wage_s(ns,nt))
    allocate(agg%earn(nt))
    
    allocate(agg%regretcl(nt))
    allocate(agg%valuedrop(nt))
    
    allocate(agg%earnHS_j(nj,nt))
    
    allocate(agg%Phi_s(ns,nt))
    
    allocate(agg%earn3040(nt))
    allocate(agg%earnLT(nt))
    allocate(agg%earnLT_j(nj,nt))
    allocate(agg%totnetres_j(nj,nt))
   
    allocate(agg%inc_thrs(nt))
    
    allocate(agg%net_trr_j(nj,nt))
    
    allocate(agg%earn3040_spar(ns,nt))
    allocate(agg%earnLT_spar(ns,nt))
    allocate(agg%earnLT_spar_j(ns,nj,nt))
    
    allocate(agg%earn3040_gpar(2,nt))
    allocate(agg%earnLT_gpar(2,nt))
    allocate(agg%earnLT_gpar_j(2,nj,nt))
    
    allocate(agg%earn3040_gspar(2,ns,nt))
    allocate(agg%earnLT_gspar(2,ns,nt))
    allocate(agg%earnLT_gspar_j(2,ns,nj,nt))
    
    allocate(agg%earn3040net(nt))
    allocate(agg%earnLTnet(nt))
    allocate(agg%earnLTnet_j(nj,nt))
    
    allocate(agg%earn3040net_spar(ns,nt))
    allocate(agg%earnLTnet_spar(ns,nt))
    allocate(agg%earnLTnet_spar_j(ns,nj,nt))
    
    allocate(agg%earn3040net_gpar(2,nt))
    allocate(agg%earnLTnet_gpar(2,nt))
    allocate(agg%earnLTnet_gpar_j(2,nj,nt))
    
    allocate(agg%earn3040net_gspar(2,ns,nt))
    allocate(agg%earnLTnet_gspar(2,ns,nt))  
    allocate(agg%earnLTnet_gspar_j(2,ns,nj,nt))  
  
    allocate(agg%At(nt))
    
    allocate(agg%uben_param(nt))
    allocate(agg%aben_param(nt))
    allocate(agg%tau_c(nt))
    allocate(agg%tau_k(nt))
    allocate(agg%lambda_pr(nt))
    allocate(agg%tau_h(nt))
    allocate(agg%tau_pr(nt))
    allocate(agg%tau_rem(nt))
  
    allocate(agg%nu(nt))
    
    allocate(agg%abar(nt))
    allocate(agg%abar_hs(nt))
    allocate(agg%kappafee(nt))
    allocate(agg%varrho(nt)) 
    allocate(agg%kappah(nt))
    allocate(agg%kappap4(nt))
    allocate(agg%kappah8(nt))
    allocate(agg%kappat(nt))
    allocate(agg%fi1(nt))
    allocate(agg%kappai1(nt))
    allocate(agg%kappai2(nt))
    allocate(agg%kappah1(nt))
    allocate(agg%kappah2(nt))
    allocate(agg%h0norm(nt))
    
    
    allocate(agg%h0distr(2,ns))
    
    allocate(agg%h0delta(nt))
    
    allocate(agg%betta0(nt))
    allocate(agg%betta1(nt))
    allocate(agg%betta2(nt))
    allocate(agg%metta0(nt))
    allocate(agg%metta1(nt))
    allocate(agg%metta2(nt))
    
    allocate(agg%iota(nt))
    
    allocate(agg%betta1_str(1))
    allocate(agg%betta2_str(1))
    allocate(agg%metta1_str(1))
    allocate(agg%metta2_str(1))
    
    allocate(agg%hk(nt))
    
    allocate(agg%alpha0(nt))
    allocate(agg%alpha1(nt))
    
    allocate(agg%lhsfrac(nt))
    allocate(agg%clfrac(nt))
    allocate(agg%dropfrac(nt))
    allocate(agg%lhsfracbase(nt))
    allocate(agg%clfracbase(nt))
    allocate(agg%clkid_cl(nt))
    allocate(agg%clkid_lhs(nt))
    allocate(agg%tgrad(nt))
    
    allocate(agg%ben_h(nt)) ! aggregate health expenditures
    
    allocate(agg%asswp(nt))
    allocate(agg%asswp_j(nj,nt))
    
    allocate(agg%pia0(ng,nk,ns,nt))

    allocate(agg%rhoedu0(nt))
    allocate(agg%rhoearn0(nt))
    
    allocate(agg%hr_sh(nt))
    allocate(agg%hr_sh_sp(ns,nt))
    allocate(agg%hr_sh_gp_sp(2,ns,nt))
    
    allocate(agg%poppar(nt)) ! population of investing parents
    allocate(agg%poppar_ivt(nt)) ! population of parents last period of childhood
    allocate(agg%poppar_inv(nt)) 
    allocate(agg%moninv(nt)) ! aggregate mon inv
    allocate(agg%tinv(nt)) ! aggregate time inv
    allocate(agg%tinv_slope(nt)) 
    allocate(agg%moninv_slope(nt))
    allocate(agg%ivt(nt)) ! aggregate ivt transfer
    allocate(agg%ivt_pkid(nt))
    allocate(agg%ivt_s(ns,nt))
    allocate(agg%ivt_skid(ns,nt))
    allocate(agg%ivt_sg(ns,2,nt))
    allocate(agg%ivt_sg_cpl(ns,2,nt))
    allocate(agg%t_s(ns,nt))
    allocate(agg%m_s(ns,nt))
    allocate(agg%poppar_s(ns,nt))
    allocate(agg%poppar_sj(ns,nj,nt))
    allocate(agg%poppar_skidj(ns,nj,nt))
    allocate(agg%poppar_sgj(ns,2,nj,nt))
    allocate(agg%poppar_sgj_cpl(ns,2,nj,nt))
    allocate(agg%poppar_ssgj_cpl(ns,ns,2,nj,nt))
    allocate(agg%t_sj(ns,nj,nt))
    allocate(agg%m_sj(ns,nj,nt))
    allocate(agg%h_sj(ns,nj,nt))
    allocate(agg%t_sgj(ns,2,nj,nt))
    allocate(agg%m_sgj(ns,2,nj,nt))
    allocate(agg%h_sgj(ns,2,nj,nt))
    allocate(agg%t_sgj_cpl(ns,2,nj,nt))
    allocate(agg%m_sgj_cpl(ns,2,nj,nt))
    allocate(agg%h_sgj_cpl(ns,2,nj,nt))
    
    allocate(agg%t_ssgj_cpl(ns,ns,2,nj,nt))
    allocate(agg%m_ssgj_cpl(ns,ns,2,nj,nt))
    allocate(agg%h_ssgj_cpl(ns,ns,2,nj,nt))
    
    allocate(agg%amin_param(nt))
    
    allocate(agg%m_j(nj,nt))
    allocate(agg%t_j(nj,nt))
    allocate(agg%h_j(nj,nt))
    allocate(agg%m_yj(ny,nj,nt))
    allocate(agg%t_yj(ny,nj,nt))
    allocate(agg%h_yj(ny,nj,nt))
    allocate(agg%inv_j(nj,nt))
    allocate(agg%poppar_j(nj,nt))
    
    allocate(agg%inv_mean(nt))
    
    ! demographic objects
    allocate(demo%ageprod(2,nj,ns))
    allocate(demo%ageprod_dat(2,nj,ns))
    allocate(demo%numchild_gsj(2,ns,nj,nt))
    
    allocate(demo%popgr(nt))
   
    allocate(demo%grid_y(ny))
    allocate(demo%grid_epsi(nw))
    allocate(demo%prob_y(ny,ny))
    allocate(demo%prob_y_ld(ny,ny,ns,nj))
    allocate(demo%prob_y_dat(ny,ny))
    allocate(demo%pini(ny))
    allocate(demo%pini_ld(ny,ns))
    allocate(demo%pini_dat(ny)) 
    allocate(demo%prob_epsi(nw))
  
    allocate(demo%frac_edu(2,ns))
    
    allocate(demo%frac_prod(nk,ns))                        
    allocate(demo%prob_gc(ng))
    
    allocate(demo%frac_s(ns))
   
    allocate(demo%lamt(nt))
    allocate(demo%popgrt(nt))
    allocate(demo%frac_jt(nj,nt))
    
    allocate(demo%mc_input(2))
    allocate(demo%sc_input(2,4))
    allocate(demo%nkids_input(2,4))
    allocate(demo%ass_input(2,4,5))
    allocate(demo%ass_input_dat(2,4,5))
    allocate(demo%hc0_input(2,4,5,ni))
    
    allocate(demo%h0prop(2,4))
   
    allocate(demo%time_j(nj))
    allocate(demo%betta0_dat(1))
    allocate(demo%betta1_dat(1))
    allocate(demo%betta2_dat(1))
    allocate(demo%metta0_dat(1))
    allocate(demo%metta1_dat(1)) 
    allocate(demo%metta2_dat(1))
    
    allocate(demo%abgrad(ns))
    
    
    ! grids singles    
    allocate(grid%coh(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))           ! cash-on-hand 
    allocate(grid%cohd(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))  
    allocate(grid%coh_old(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    allocate(grid%coh_exo(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    allocate(grid%coh_guess(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nt)) 
    allocate(grid%coh_guessm(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nt))
   
    allocate(grid%ass(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))         ! assets
  
    !allocate(grid%assmax(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    !allocate(grid%assmin(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))  
    !allocate(grid%grpstk(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))      ! gross pension stock
    !allocate(grid%pstk(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! pension stock
    allocate(grid%sav(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))          ! saving
    
    !allocate(grid%savp(np,ni,nj,nk,ns,nt))                 ! saving in pension stock
    
    allocate(grid%Phi(nd,nx,np,ny,ne,nk,ng,nw,np,ns,ni,ns,nj,nt))          ! cross-sectional distribution
    allocate(grid%Phi_s(ns,ng,nt))
    
    allocate(grid%Phi2guess(nd,nx,ny,nk,ng,nw,ns,nt))
    allocate(grid%Phi2guess_s(nd,nx,ny,nk,ng,nw,ns,nt))
    allocate(grid%sav2guess(nd,nx,ny,nw,ng,nk,ns,nt))
 
    allocate(grid%Phi_guess(nd,nx,np,ny,ne,nk,ng,nw,np,ns,ni,ns,nj,nt))
   
    allocate(grid%Phi_child(nd,nx,np,ny,ne,nk,ng,nw,np,ns,ni,ns,nj,nt))
    allocate(grid%Phi_childja(nd,nx,np,ny,ne,nk,ng,nw,np,ns,ni,ns,2,nt))
    allocate(grid%lab(2,ndl,ns))
   
    allocate(grid%phi_edu_long(nu_long))
    allocate(grid%phi_earn_long(nv_long))
    
    !allocate(grid%phi_inv_long(nu_long))
    !allocate(grid%p_inv_long(nu_long,2))
    allocate(grid%pc_edu_long(nu_long,2))
    allocate(grid%pc_earn_long(nv_long,2))
    
    ! grids couples   
    allocate(grid%coh_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))           ! cash-on-hand 
    allocate(grid%cohd_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))  
    allocate(grid%coh_old_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))  
    allocate(grid%coh_exo_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
   
    allocate(grid%ass_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))          ! assets
   
    !allocate(grid%assmax_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    !allocate(grid%assmin_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))   
  
    allocate(grid%sav_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))           ! saving
   
    allocate(grid%Phi_cpl(nd_cpl,nx,np,ny,ny,ne,nk,nk,nw,nw,ns,ns,nj,nt))          ! cross-sectional distribution
 
    allocate(grid%hk_grid(np))
    allocate(grid%h0_grid(ni))
    allocate(grid%minv_grid(nminv))
    allocate(grid%minv_grid_dat(nminv))
    allocate(grid%tinv_grid(ntinv))
   
    allocate(grid%amin_age(2,4,2,nj))
     
    ! life-cycle objects:
    allocate(lc%cons_j(nj,nt))
    allocate(lc%m_j(nj,nt))
    allocate(lc%t_j(nj,nt))
    allocate(lc%lab_j(nj,nt))
    allocate(lc%lab1_j(nj,nt))
    allocate(lc%lab2_j(nj,nt))
    allocate(lc%ap_j(nj,nt))
    allocate(lc%coh_j(nj,nt))
    allocate(lc%indbc_j(nj,nt))
   
    ! policies singles
    allocate(pol%v(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))           ! value function
    allocate(pol%vd(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(pol%dc(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))   
    
    allocate(pol%v_nokid(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))  
    allocate(pol%v_guess(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nt))  
    allocate(pol%v_guessm(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nt)) 
    allocate(pol%t1(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))  
  
    allocate(pol%m(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))  
 
    allocate(pol%b(nd,nx,np,ny,ne,nw,ng,nk,ns,np,ns,nt))  
   
    allocate(pol%inv_vp_x(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))    ! inverse of derivative of value function w.r.t cash on hand      
    allocate(pol%evtp1_coh(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))   
    !allocate(pol%inv_vp_pgr(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))      ! derivative of value function w.r.t gross pension stock
    allocate(pol%cons(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))           ! consumption
    allocate(pol%consd(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
  
    allocate(pol%leis(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! leisure
    allocate(pol%lab(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! lab
    allocate(pol%labd(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(pol%lab_eff(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))  
 
    allocate(pol%grwageinc(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! gross wage income
    allocate(pol%grwage(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    allocate(pol%grinc(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! gross income
    allocate(pol%netinc(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! net income
    allocate(pol%netsav(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! net saving
    allocate(pol%indbc(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))        ! index for borrowing constraint
    allocate(pol%net_trr(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    allocate(pol%labinctaxrv(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(pol%hsvtmp1(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(pol%hsvtmp2(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    allocate(pol%capinctaxrv(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(pol%constaxrv(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(pol%penstaxrv(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt)) 
    allocate(grid%coh_child(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
   
    allocate(pol%v_child(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ni,ns,nj,nt))
    allocate(pol%vder_child(nd,nx,np,ny,ne,nw,ng,nk,np,ns,ng,ns,nj,nt))

    
    ! policies couples
    allocate(pol%v_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))           ! value function
    allocate(pol%vd_cpl(4,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))
    allocate(pol%v_nokid_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))    
    
    allocate(pol%dc_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))  
  
    allocate(pol%t1_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))    
    allocate(pol%t2_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))  
  
    allocate(pol%m_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))    
 
    allocate(pol%b_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nt))  
   
    allocate(pol%inv_vp_x_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))    ! inverse of derivative of value function w.r.t cash on hand      
    allocate(pol%evtp1_coh_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))    
  
    allocate(pol%cons_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))            ! consumption
    allocate(pol%consd_cpl(4,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))     
  
    allocate(pol%leis1_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))         ! leisure
    allocate(pol%leis2_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%labtot_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))         ! lab
    allocate(pol%lab1_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%lab2_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%lab1d_cpl(4,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%lab2d_cpl(4,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%lab1_eff_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%lab2_eff_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
 
    allocate(pol%grwageinc_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))         ! gross wage income
    allocate(pol%grwage_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%grinc_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))        ! gross income
    allocate(pol%netinc_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))         ! net income
    allocate(pol%netsav_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))        ! net saving
    allocate(pol%indbc_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))         ! index for borrowing constraint
    allocate(pol%net_trr_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%labinctaxrv_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%hsvtmp1_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%hsvtmp2_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%capinctaxrv_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt))  
    allocate(pol%constaxrv_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    allocate(pol%penstaxrv_cpl(nd_cpl,nx,np,ny,ny,ne,nw,nw,nk,nk,ns,ns,nj,nt)) 
    
    end subroutine sub_alloc
    ! ----------------------------------------------------------------------------------------------------
    
end module types_mod
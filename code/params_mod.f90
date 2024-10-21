module params_mod
    
use nrtype
implicit none
!-------------------------------------------------------------------------------------------------
! The following are set and explained in the calibration file (see select_calibration_here.txt)
!-------------------------------------------------------------------------------------------------
integer, protected :: opt_corr,opt_pause,opt_warn,opt_accbq,opt_alg_stst,opt_alg_trans,opt_calib,opt_readin_trans,&
    opt_trans,opt_constval,opt_test_trans,opt_test_flap,opt_sumstats,opt_test_nosr,opt_twogen,opt_ivt,opt_inv, & ! ,opt_flex_grid
    opt_incr_ra,opt_test_noer,opt_fss,opt_add_point,opt_prod_cb,opt_taup_const,opt_gconst,opt_bconst,opt_index,  opt_soe,  &
    opt_igov_ps,opt_arttrg,opt_ageindep,opt_asymm_shk,opt_shk,opt_igov,opt_cd,opt_pchip, opt_ige_compute,opt_mt,opt_curver  ! general options
integer(kind=8),protected:: nu_long, nv_long
integer:: ind_ext_bound
integer:: opt_flex_grid,opt_pe_in,opt_debt, opt_sumstats_loc,opt_ext_ls,opt_ext_compute
real(dp),protected :: alph,tetta,ksi(2),lamb,popgr,techgr,rp_for,zetta,zetta1,delt,fr_grad         ! preferences, technology, productivity
real(dp),protected :: se_kl,se_skills                               ! substitution elasticities
real(dp) :: tau_pr !lambda_pr                                     ! tax function parameters
real(dp),protected :: tau_p, tau_k, tr_param, tr_param_ref, sort_param
real(dp):: tau_c
integer, protected :: np,ne,ni,ns,ny,nw,nk,ndl,ndusr,ngkid,ns_cpl,nk_cpl                     ! state space
real(dp),protected :: max_x,max_x_fac                                   ! size of coh grid (last period)
real(dp),protected :: max_h, min_h, max_h0, min_h0
real(dp),protected :: biggam
integer,protected  :: nb, nminv, ntinv
integer, protected :: maxits_stst,rst_stst,maxln_stst                   ! numerical settings for steady state algorithm
real(dp),protected :: wght_stst,tol_stst,stpmx_stst                     ! numerical settings for steady state algorithm
integer, protected :: maxits_trans,rst_trans,maxln_trans                ! numerical settings for transition algorithm
real(dp),protected :: wght_trans,tol_trans,stpmx_trans                  ! numerical settings for transition algorithm
integer, protected :: jsp,njp,jrp,t1p,t2p,tr_yrs,t1gp,t2gp,tg_delt,tbp,t11p,tclb1,jc_lvas,tc1_transcalib,tc2_transcalib,jfp,jtp,jhsp,jclp     ! age and time bounds
real(dp),protected :: sigma_emp, sigma_ret, sigma_stig, sigma ! scale param of taste shocks
real(dp),protected:: ky_str,hrs_fem_str,hrs_mal_str,frish_fem_str,frish_mal_str,hk_str,h0_str,ivty_str,lhsfrac_str,dropfrac_str,varwage_str,clfrac_str,clkid_lhs_str,clkid_cl_str,alpha0_str, alpha1_str
real(dp):: moninv_str,tinv_str,tgrad_str,mgrad_str,t1_str,tinv_slope_str,moninv_slope_str
real(dp),protected:: earnref_data, bclimit, fee_flow_frac, colsubs_pub, colsubs_priv, igov_frac, stud_lab_frac(2)
real(dp),protected:: lglv,lvl_ref
real(dp):: lambda_pr, avearn, avearn_base, avearn_tp1, avearn_tp1_base, adj_fac_tr, ksi_frisch_fem,ksi_frisch_mal, avwage_tclb, scl_hs, scl_cl, inc_thrs_param
real(dp),protected:: er_pen,lat_rew
real(dp):: col_subs_param_ref, igov_ref, igov_ref_upd, igov_ref_old, aggpdvcol,aggpdvig,aggpdvfc
real(dp):: priv_subs_param
integer,protected:: jer ! early retirement age 60-to 65, 65 not included
integer,protected:: jrr ! regular retirement, statutory retirement age is 70
integer,parameter:: jm=12 !0
! retirement steps are: EARLY:jer to jrr, REG: jrr to jr, LATE: >=jr 
integer,protected:: ng  ! number of genders (by this means nest a subversion for "bachelor" hhs 
integer,protected:: opt_cpl ! =1: couples are present, =0: only singles
integer,parameter:: opt_captax=0
integer,protected:: nprm_clb ! beta plus two leisure weights
integer,protected:: atr ! number of additional outer loop vars in trnas
real(dp),protected:: emprate_str, gyr_str, byr_str
real(dp),protected:: frac_loss, omegac
real(dp):: ft_lab, pt_lab
real(dp),protected:: frischela_str 
real(dp),protected:: scalfac_ref, scalfac_fem
integer,protected:: opt_test_project
integer,protected:: opt_test_laptop
integer,protected:: j_ac_min,j_ac_max
integer,protected:: opt_reintp 
integer:: it_vf,vf_init_flg,meas_init_flg
logical:: opt_ige
integer:: opt_lambda, opt_cpl_aggr, it_meas_mar

real(dp),dimension(4):: ab_grad ! ability gradient

!-------------------------------------------------------------------------------------------------
! These are derived parameters
!-------------------------------------------------------------------------------------------------
real(dp):: zeta_cl,taste_cl,abar, kappah_param, fitm_param, nu, mean_moninv, mean_tinv,gamma_s(4), kappai_j_param(8),kappah_j_param(8),kappai_b_param,kappai_c_param, kappah_b_param,kappah_c_param,earn_si_lo
real(dp):: h0norm_param, iota_param, wght_abgrad,avearn3040,avearn3040net,avearn3040_s(4),avearn3040_gs(2,4),avearnLT,avearnLTgr,avearnLT_s(4),avearnLTgr_s(4),avearnLT_gs(2,4),avearnLTgr_gs(2,4),avearn3040_g(2), avearnLT_g(2),inv_mean, kappap4, kappah8, zeta
real(dp):: avearnLT_gsx(2,4,5), avearnLTgr_gsx(2,4,5)
real(dp):: jstud(4)
logical:: opt_gamma_update 
real(dp) :: igov_j(4), igovbase_j(4),kappat_param, fee_flow, col_subs_flow,col_subs_param,fee_flow_net,h0delta_param, ass_cutoff(4)
integer:: opt_calib_loc, opt_init_ss, opt_fin_ss, opt_trref, opt_trref_hk, opt_invconst, opt_vf_loop, opt_ge_upd, opt_meas_loop, opt_suits
integer::nj,js,jr0,nt,tr,t1g,t2g,tb,trfg,tclb,jt,jf,ndl_cpl                  ! age bounds, time indices
real(dp)::rhho_s,rhho_ni(4),rhho_ni_f(4),rhho_kl 
real(dp)::tech_scale                        ! scaling factor in aggregate production function: scales wages such that steady state aggregate wage equals one 
integer,allocatable::jr(:)                  ! time dependent retirement age
real(dp)::af_tech                           ! adjustment factor to aggregate technology
real(dp):: gamma_adj,betta,pheye(2,2),pheye_t(2)                        ! another adjustment factor to aggregate technology
integer:: nd,nd_cpl,nx,np1_aux ! set in calib_mod
real(dp):: lambda_tilde,ksi_calib(2)
real(dp):: norm_param(4), nu_param, abar_param,abar_hs_param,kappai_param 
real(dp),dimension(5):: igov_loss_vec = (/ 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp  /)
integer:: opt_lockdown, jc_lockdown, opt_vf_init, opt_tauc, opt_margprg 
real(dp):: gov_def_parent, gcons_base, gconst, totinc_base, totinc_base_child, gcons_base_child,gov_def_child,gammafe(4,2),gamdelta(4),scaledelta(4),gamdelta_hs,meandelta_s(4),gdp_base,subs_param_frac,subs_param,col_subs_cost,edu_spend_fac
real(dp):: omega_fin, omega_fin_hs, psi_ls, fee_flow_base,col_subs_flow_base,col_subs_cost_base,grincmed, pvec_ass(9), pvec_inc(9), qvec_inc(4), qvec_inc_base(4),pvec_ass_frac(10),pvec_inc_frac(10),qvec_ass_frac(5),qvec_inc_frac(5),toty_ass,toty_inc
   
!-------------------------------------------------------------------------------------------------
! Assign values to some parameters directly here (these parameters are NOT supposed to be (frequently) adjusted
!-------------------------------------------------------------------------------------------------
integer,parameter:: maxit_vf=7 
! age-dependent hc parameters
real(dp),parameter,dimension(2):: zi2_age = (/0.3_dp, 0.5_dp /) ! t1 share (recall, t1 is Female!!)
real(dp),parameter,dimension(2):: fi2_age = (/0.1_dp, 0.6_dp/) ! subst t1 and t2 if not perf subst CHECK THIS!!!!
real(dp),parameter,dimension(2):: xi = (/1.25_dp, 2.5_dp /)
real(dp),dimension(2),parameter:: kkappa = (/0.7484375_dp, 0.949714286_dp/)
real(dp),dimension(2),parameter:: ssigma = (/1.457_dp, 0.446_dp/)
real(dp):: kappa_j_param(6), sigma_j_param(6), fi0_j_param(6)  
real(dp),parameter:: schweek = 22.9_dp, kappap = 0.13_dp, rho_g = 0.589_dp
real(dp),dimension(4),parameter:: deltaprob_s = (/2.44_dp, 2.18_dp, 1.0_dp, 1.0_dp  /) 
real(dp),dimension(4),parameter:: quintcoh = (/0.427502085465331_dp,       0.724534422796410_dp,        1.07365226037735_dp, 1.66009285982060_dp   /)  ! 0.406785349649908       0.862791788997847        1.28576512518605     

real(dp),dimension(4),parameter:: quintcohpar = (/0.409037737037617_dp,       0.880666483357432_dp,        1.31603318163276_dp, 1.93370139617162_dp   /)  ! 0.406785349649908       0.862791788997847        1.28576512518605     

!real(dp),dimension(4),parameter:: deltaprob_s = (/1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp  /)
real(dp),parameter:: subs_param_base_frac = 0.6438_dp 
real(dp),parameter:: xprod = 1.0_dp  - 0.9268_dp
real(dp),parameter:: g=0.0302_dp
real(dp),parameter:: varrho_pr = 0.166_dp
logical:: opt_mean_update
! Bendpoint formula of pension system
real(dp),dimension(3),parameter::bp=(/0.24_dp, 1.35_dp, 1.99_dp /)
real(dp),dimension(3),parameter::slp_bp=(/ 0.9_dp, 0.32_dp, 0.15_dp /)

real(dp),parameter,dimension(2):: pssi = (/1.0_dp/0.6_dp,1.0_dp/0.6_dp/)

integer,parameter:: delta_j=4  
  
real(dp),parameter:: infty = 9000000.0_dp ! INFINITY
real(dp):: NaN

integer:: opt_pe, opt_cpl_comp
real(dp):: probmar_s(2,2,4),sav_min(2,47),sav_max(2,47),probmar(2)

!real(dp),parameter:: tr_param =0.0_dp !  0.0294_dp


!integer(kind=8),parameter::nu_long= 10000000  ! (ne*ny)*(ns*nk*nx*ny)*(ns*2)     ! # of entries for "education" persistence 
!integer(kind=8),parameter::nv_long= 10000000000

contains

    ! ----------------------------------------------------------------------------------------------------------------
    subroutine ReadCalibration(calib_name)
        
        character(len=*) ,intent(in) :: calib_name 
        character(len=80) :: parname, parval, parval2
        integer           :: istat, line, pos
        
        open (unit=21, file='input/calibr/'//calib_name, status='OLD', action='READ', iostat=istat)
        openif: if (istat==0) then
            line=0
            do
                line=line+1 ! blank lines in txt-file are not counted
                read (21,*,iostat=istat) parname, parval, parval2
                if (istat/=0) exit
                if (scan(parname,'!')>0) cycle

                select case (parname)
                 
                case ('opt_twogen')
                    read (parval,*) opt_twogen
                case ('opt_pchip')
                    read (parval,*) opt_pchip    
                case ('opt_cd')
                    read (parval,*) opt_cd    
                case ('opt_ageindep')
                    read (parval,*) opt_ageindep    
                case ('opt_asymm_shk')
                    read (parval,*) opt_asymm_shk 
                case ('opt_shk')
                    read (parval,*) opt_shk    
                case ('opt_igov')
                    read (parval,*) opt_igov    
                case ('opt_pe')
                    read (parval,*) opt_pe_in 
                case ('opt_igov_ps')
                    read (parval,*) opt_igov_ps    
                case ('opt_soe')
                    read (parval,*) opt_soe      
                case ('opt_taup_const')
                    read (parval,*) opt_taup_const  
                case ('opt_gconst')
                    read (parval,*) opt_gconst 
                case ('opt_bconst')
                    read (parval,*) opt_bconst    
                case ('opt_cpl')
                    read (parval,*) opt_cpl
                case ('opt_ivt')
                    read (parval,*) opt_ivt
                case ('opt_inv')
                    read (parval,*) opt_inv    
                case ('opt_warn')
                    read (parval,*) opt_warn
                case ('opt_pause')
                    read (parval,*) opt_pause
                case ('opt_corr')
                    read (parval,*) opt_corr    
                case ('opt_accbq')
                    read (parval,*) opt_accbq
                case ('opt_alg_stst')
                    read (parval,*) opt_alg_stst
                case ('opt_alg_trans')
                    read (parval,*) opt_alg_trans
                case ('opt_readin_trans')
                    read (parval,*) opt_readin_trans    
                case ('opt_trans')
                    read (parval,*) opt_trans
                case ('opt_flex_grid')
                    read (parval,*) opt_flex_grid
                case ('opt_add_point')
                    read (parval,*) opt_add_point
                case ('opt_constval')
                    read (parval,*) opt_constval
                case ('opt_fss')
                    read (parval,*) opt_fss             ! final steady state computation
                case ('opt_sumstats')
                    read (parval,*) opt_sumstats        ! compute summary statistics
                case ('opt_prod_cb')
                    read ( parval,*) opt_prod_cb
                case ('opt_calib')
                    read ( parval,*) opt_calib 
                    
                case ('opt_arttrg')
                    read ( parval,*) opt_arttrg
                 
                case ('nu_long')
                    read ( parval,*) nu_long
                case ('nv_long')
                    read ( parval,*) nv_long    
                
                case ('opt_ige_compute')
                    read ( parval,*) opt_ige_compute
                
                case ('opt_mt')
                    read ( parval,*) opt_mt     
                case ('opt_curver')
                    read ( parval,*) opt_curver    
                    
                case ('opt_test_trans')
                    read (parval,*) opt_test_trans
                case ('opt_test_flap')
                    read (parval,*) opt_test_flap       ! flat age productivity
                case ('opt_test_nosr')
                    read (parval,*) opt_test_nosr       ! no survival risk
        
                case ('opt_test_noer')
                    read (parval,*) opt_test_noer       ! no endogenous retirement
                case ('opt_index')
                    read (parval,*) opt_index       
                    
                case ('opt_test_laptop')
                    read (parval,*) opt_test_laptop 
                case ('opt_test_project')
                    read (parval,*) opt_test_project 
                    
                case ('opt_reintp')
                    read (parval,*) opt_reintp    
                    
                case ('ivty_str')
                    read ( parval,*) ivty_str  
                case ('moninv_str')
                    read ( parval,*) moninv_str
                case ('tinv_str')
                    read ( parval,*) tinv_str  
                case ('tgrad_str')
                    read ( parval,*) tgrad_str   
                case ('lhsfrac_str')
                    read ( parval,*) lhsfrac_str  
                case ('dropfrac_str')
                    read ( parval,*) dropfrac_str    
                case ('clfrac_str')
                    read ( parval,*) clfrac_str 
                case ('varwage_str')
                    read ( parval,*) varwage_str    
                case ('clkid_cl_str')
                    read ( parval,*) clkid_cl_str 
                case ('clkid_lhs_str')
                    read ( parval,*) clkid_lhs_str    
                case ('ky_str')
                    read ( parval,*) ky_str
                case ('hrs_fem_str')
                    read ( parval,*) hrs_fem_str  
                case ('hrs_mal_str')
                    read ( parval,*) hrs_mal_str 
                case ('alpha0_str')
                    read ( parval,*) alpha0_str 
                case ('alpha1_str')
                    read ( parval,*) alpha1_str     
                case ('earnref_data')
                    read ( parval,*) earnref_data  
                case ('bclimit')
                    read ( parval,*) bclimit  
                case ('sort_param')
                    read ( parval,*) sort_param     
                case ('fee_flow_frac')
                    read ( parval,*) fee_flow_frac
                case ('colsubs_pub')
                    read ( parval,*) colsubs_pub
                case ('col_subs_param_ref')
                    read ( parval,*) col_subs_param_ref 
                case ('igov_ref')
                    read ( parval,*) igov_ref    
                case ('colsubs_priv') 
                    read ( parval,*) colsubs_priv  
                case ('igov_frac')
                    read ( parval,*) igov_frac  
                case ('stud_lab_frac_hs')
                    read ( parval,*) stud_lab_frac(1) 
                case ('stud_lab_frac_cl')
                    read ( parval,*) stud_lab_frac(2)     
                
                case ('frish_fem_str')
                    read ( parval,*) frish_fem_str    
                case ('frish_mal_str')
                    read ( parval,*) frish_mal_str     
                case ('emprate_str')
                    read ( parval,*) emprate_str
                    
                case ('hk_str')
                    read ( parval,*) hk_str
                case ('h0_str')
                    read ( parval,*) h0_str    
                
                case ('gyr_str')
                    read ( parval,*) gyr_str
                case ('byr_str')
                    read ( parval,*) byr_str    
                case ('frischela_str')
                    read ( parval,*) frischela_str    
                
                case ('lglv')
                    read (parval,*) lglv
                case ('lvl_ref')
                    read (parval,*) lvl_ref
                case ('tau_k')
                    read (parval,*) tau_k
                case ('tau_c')
                    read (parval,*) tau_c
                case ('tr_param')
                    read (parval,*) tr_param
                case ('tr_param_ref')
                    read (parval,*) tr_param_ref    
                
                case ('alpha')
                    read (parval,*) alph
                case ('delta')
                    read (parval,*) delt
                case ('betha')
                    read (parval,*) betta
                case ('theta')
                    read (parval,*) tetta
                case ('ksi_fem')
                    read (parval,*) ksi(1)
                case ('ksi_mal')
                    read (parval,*) ksi(2)
                case ('phi_fem')
                    read (parval,*) pheye(1,1)
                case ('phi_mal')
                    read (parval,*) pheye(1,2)
                case ('lam')
                    read (parval,*) lamb
                case ('popgr')
                    read (parval,*) popgr
                case ('techgr')
                    read (parval,*) techgr
                case ('fr_grad')
                    read (parval,*) fr_grad    
                case ('rp_for')
                    read (parval,*) rp_for
                case ('zeta')
                    read (parval,*) zetta
                case ('zeta1')
                    read (parval,*) zetta1  
              
                case ('se_kl')
                    read (parval,*) se_kl
                case ('se_skills')
                    read (parval,*) se_skills
                
                !case ('lambda')
                !    read (parval,*) lambda_pr
                case ('tau_pr')
                    read (parval,*) tau_pr
                case ('tau_p')
                    read (parval,*) tau_p
                    
                    
                case ('scalfac_ref')
                    read (parval,*) scalfac_ref
                case ('scalfac_fem')
                    read (parval,*) scalfac_fem     
                
                case ('frac_loss')
                    read (parval,*) frac_loss 
                case ('ft_lab')
                    read (parval,*) ft_lab   
                case ('pt_lab')
                    read (parval,*) pt_lab     
                case ('omegac')
                    read (parval,*) omegac     
                    
                
                case ('er_pen')
                    read (parval,*) er_pen
                case ('lat_rew')
                    read (parval,*) lat_rew  
                 
                case ('opt_incr_ra')
                    read (parval,*) opt_incr_ra     ! oncrease retirement age
                 
                case ('nx')
                    read (parval,*) nx    
                case ('np1_aux')
                    read (parval,*) np1_aux
                case ('ny')
                    read (parval,*) ny
                case ('nw')
                    read (parval,*) nw    
                case ('np')
                    read (parval,*) np
                case ('ne')
                    read (parval,*) ne
                case ('ni')
                    read (parval,*) ni
                case ('ns')
                    read (parval,*) ns
                case ('nk')
                    read (parval,*) nk
                case ('ns_cpl')
                    read (parval,*) ns_cpl
                case ('nk_cpl')
                    read (parval,*) nk_cpl    
                case ('ng')
                    read (parval,*) ng
                 
                case ('ndl')
                    read (parval,*) ndl
                    
                case ('ngkid')
                    read (parval,*) ngkid    
               
                !case ('ndl_cpl')
                !    read (parval,*) ndl_cpl
               
                case ('ndusr')
                    read (parval,*) ndusr
                
                case ('nprm_clb')
                    read (parval,*) nprm_clb
                case ('atr')
                    read (parval,*) atr    
                    
                    
                case ('jsp')
                    read (parval,*) jsp
                case ('jhsp')
                    read (parval,*) jhsp
                case ('jclp')
                    read (parval,*) jclp    
                
                case ('jrp')
                    read (parval,*) jrp
                case ('jfp')
                    read (parval,*) jfp
                case ('jtp')
                    read (parval,*) jtp    
                case ('jer')
                    read (parval,*) jer
                case ('jrr')
                    read (parval,*) jrr  
                case ('jc_lvas')
                    read (parval,*) jc_lvas      
                    
                case ('njp')
                    read (parval,*) njp
                case ('j_ac_min')
                    read (parval,*) j_ac_min 
                case ('j_ac_max')
                    read (parval,*) j_ac_max     
                    
                    
                case ('t1p')
                    read (parval,*) t1p
                case ('t2p')
                    read (parval,*) t2p
            
                case ('t11p')
                    read (parval,*) t11p
                case ('tclb1')
                    read (parval,*) tclb1    
                case ('tr_yrs')
                    read (parval,*) tr_yrs
                case ('t1gp')
                    read (parval,*) t1gp            ! first year of Gini computation
                case ('t2gp')
                    read (parval,*) t2gp            ! second year of Gini computation
                case ('tg_delt')
                    read (parval,*) tg_delt         ! Gini computation every tg_delt years
                case ('tbp')
                    read (parval,*) tbp             ! base year for welfare evaluation
                case ('max_x')
                    read (parval,*) max_x
                case ('max_x_fac')
                    read (parval,*) max_x_fac
                case ('max_h')
                    read (parval,*) max_h
                case ('min_h')
                    read (parval,*) min_h
                case ('max_h0')
                    read (parval,*) max_h0
                case ('min_h0')
                    read (parval,*) min_h0    
                case ('maxits_stst')
                    read (parval,*) maxits_stst
                case ('rst_stst')
                    read (parval,*) rst_stst
                case ('maxln_stst')
                    read (parval,*) maxln_stst
                case ('wght_stst')
                    read (parval,*) wght_stst
                case ('tol_stst')
                    read (parval,*) tol_stst
                case ('stpmx_stst')
                    read (parval,*) stpmx_stst
                case ('maxits_trans')
                    read (parval,*) maxits_trans
                case ('rst_trans')
                    read (parval,*) rst_trans
                case ('maxln_trans')
                    read (parval,*) maxln_trans
                case ('wght_trans')
                    read (parval,*) wght_trans
                case ('tol_trans')
                    read (parval,*) tol_trans
                case ('stpmx_trans')
                    read (parval,*) stpmx_trans
                case ('sigma_emp')
                    read (parval,*) sigma_emp
                case ('sigma_ret')
                    read (parval,*) sigma_ret  
                case ('sigma_stig')
                    read (parval,*) sigma_stig
                case ('sigma')
                    read (parval,*) sigma   
                    
                case ('biggam')
                    read (parval,*) biggam       
                case ('nb')
                    read (parval,*) nb
                case ('nminv')
                    read (parval,*) nminv
                case ('ntinv')
                    read (parval,*) ntinv    
                    
                case default
                    print '(a,a)', 'Unknown parameter: ',parname
                end select
            end do
            if (istat>0) print '(a,i6)', 'An error occured reading line', line
        else openif
            print '(a,i6)', 'ERROR opening calibration file: IOSTAT=',istat
            stop '*********STOP********* in Params:ReadCalib'
        end if openif
        close (unit=21)
        

    end subroutine ReadCalibration
    ! ----------------------------------------------------------------------------------------------------------------
   

end module params_mod

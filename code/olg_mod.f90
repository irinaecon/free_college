module olg_mod
    
    ! this module organizes solution of the OLG model / in PE setup organizes calibration and gvt budget clearing
    use nrtype
    use params_mod
    use types_mod
    use solve_hh_main
    use aggrpar_full_mod
    use newuoa_module
 
    
	implicit none
    
    integer:: opt_pe_ind
    
    contains  

! ---------------------------------------------------------------------------    
subroutine sub_olg(stat,agg,demo,grid,lc,pol) 
! organization of solution of OLG model
use nrutil  
implicit none

type(t_stat),intent(inout)::stat 
type(t_agg),intent(inout)::agg 
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_lc),intent(inout)::lc
type(t_pol),intent(inout)::pol
type(t_pol_st)::pol_st 

logical,parameter::opt_check=.false.
integer::tc,sc,ic,ac,jc,zc,tc_loc

! variables needed for solution algorithm
real(dp),allocatable,dimension(:)::fgs,xgs,fgstr,xgstr,diagRss,diagQss,sfagg
real(dp),allocatable,dimension(:,:)::Rss(:,:),Qss(:,:),Rtr(:,:),Qtr(:,:)
logical::intlj,reevalj,check
integer::ngs,ngtr     ! size of outerloop variables
integer::t0,t1,it_calib,maxit_clb,nx_loc,ic_ksi,it_clb
integer,parameter:: maxit_calib=10,maxit_clbloop = 40
real(dp):: wap(nt),wap_fem(nt),wap_mal(nt), hrs_fem_pp(nt),hrs_mal_pp(nt),scalfac_old(nprm_clb),scalfac_old_wp(ns),dist_calib,dist_calib_wp,wp_trans,wp_ss,aw_ref,aw_zis,wp_trans_t(nt)
real(dp):: totrevpdv, avpdv
real(dp),parameter::epsi=1.0e-03,df_scl = 0.0_dp
integer:: gc,ind_param,ind_param_loc,kc,optcalib_id,opt_pe_min,opt_pe_max,idmin,idmax
integer :: I,  NPT
real(dp) :: RHOEND, RHOBEG
real(8), dimension(:), allocatable :: X
real(dp) ::b(ns,nj),b_til_tp1(ns,nj),dist_trg
real(dp):: df  ! dampening factor for outer calibration loop
real(dp),parameter:: df_incr = 0.025_dp
integer:: it_lambda
real(dp):: lambda_old, dist_lambda
integer:: pe_ind_arr(3)

opt_trref = 0
opt_trref_hk = opt_trref

opt_vf_loop = 1
opt_meas_loop = 1

af_tech = 1.0_dp 
 
!opt_inv_loc = 1

df=0.5_dp 

opt_ge_upd = 0

opt_invconst = 0

opt_tauc = 0
opt_margprg = 0
opt_suits = 0

opt_lambda =0

! input of staring values
call sub_inoutloop_run(agg,1) 

vf_init_flg = 1
meas_init_flg = 1
 
opt_lockdown = 0

wght_abgrad = 1.0_dp

opt_mean_update = .false.

! initialize stuff for marr market loop
grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,jf-2,:) = 0.0_dp
grid%Phi(1,1,1,1,1,1,2,1,1,1,1,1,jf-2,:) = 0.5_dp !grid%Phi(1,xc2,1,ycp1_p,1,kc_p,gc_p,wcp1_p,1,sc_p,1,1,jf-2,tc)
grid%Phi(1,1,1,1,1,1,1,1,1,1,1,1,jf-2,:) = 0.5_dp
grid%Phi2guess = 0.0_dp
grid%Phi2guess(1,1,1,1,1,1,1,:) = 0.5_dp
grid%Phi2guess(1,1,1,1,2,1,1,:) = 0.5_dp
grid%Phi2guess_s = 0.0_dp
grid%Phi2guess_s(1,1,1,1,1,1,:,:) = 0.5_dp
grid%Phi2guess_s(1,1,1,1,2,1,:,:) = 0.5_dp
grid%sav(:,:,:,:,:,:,:,:,:,:,:,:,jf-2,:) = 0.0_dp
grid%Phi_s(1,1,:) = 1.0_dp !ns**(-1.0_dp) 
grid%Phi_s(1,2,:) = 1.0_dp !ns**(-1.0_dp)
grid%sav2guess = 0.0_dp
sav_min = 0.0_dp
sav_max = 0.0_dp

do tc = 1,nt
    agg%pop_j(:,tc) = demo%frac_jt(:,tc)
enddo

! wages differ only by education, in PE doesn't matter, all are set to lvl_ref

if (opt_calib>1)then ! calibration
   
    print*, ' '
    print*, '----------------------------------------------'
    print*, 'CALIBRATION steady state chosen year ', tclb1  
    print*, '----------------------------------------------'
    print*, ' '
    
    ! GROUP 1
    ! asset income ratio
    agg%ky_str(tclb) = ky_str
    ! g / y           
    agg%gyr_str(tclb) = gyr_str
    
    ! b/ y
    agg%byr_str(tclb) = byr_str
    
    ! GROUP 2
    ! ivt as a fraction of tot assets 
    agg%ivty_str(tclb) = ivty_str
    
    ! GROUP 3
    ! mean HK at age jt
    agg%hk_str(tclb) = hk_str
    agg%hk_hs_str(tclb) = hk_str * 1.0594_dp
    
    ! GROUP 4
    ! fraction of hs
    agg%lhsfrac_str(tclb) = lhsfrac_str
    ! fraction of cl    
    agg%clfrac_str(tclb) = clfrac_str
    
    
    agg%dropfrac_str(tclb) = dropfrac_str
    
    agg%hrs_str(tclb) = 1.0_dp/3.0_dp
    
    agg%varwage_str(tclb) = varwage_str
    
    ! GROUP 5
    ! annual per child inv TIME / ft_lab    
    agg%tinv_str(tclb) = tinv_str
    agg%tinv_slope_str(tclb) = tinv_slope_str
    ! annual per child inv MON / earn_ref
    agg%moninv_str(tclb) = moninv_str
    ! slope of money profile
    agg%moninv_slope_str(tclb) = moninv_slope_str
    
    ! GROUP 6
    ! time inv at child age 1
    agg%tinv0_str(tclb) = t1_str
    
    agg%clkid_cl_str(tclb) = clkid_cl_str
    agg%clkid_lhs_str(tclb) = clkid_lhs_str
    
    ! GROUP 7
    ! education gradient time [take weighted average of two]
    agg%tgrad_str(tclb) = tgrad_str
    
    agg%betta1_str(tclb) = demo%betta1_dat(1) 
    agg%betta2_str(tclb) = demo%betta2_dat(1) 
    
    agg%metta1_str(tclb) = demo%metta1_dat(1) 
    agg%metta2_str(tclb) = demo%metta2_dat(1)
    
    agg%meanwage_s_str(:,tclb) = 1.0_dp
    
    print*, ' '
    print*, '----------------------------------------'
    print*, 'Compute all Distance Functions and setup artificial targets'
    print*, '----------------------------------------'
    print*, ' '
    
    opt_calib_loc=1
    opt_init_ss=1
    opt_fin_ss = 0
    t0=tclb
    t1=tclb
    opt_sumstats_loc = 1
    
    opt_vf_init = 1
    
    call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    
    call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    
    opt_vf_init = 0
    opt_sumstats_loc = 0
    
    !print*, "bASELINE"
    !call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    !
    !call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    !
    !
    !opt_trref=1
    !print*, "reform"
    !call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    !
    !call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    !pause
    !! to test 
    !call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    !call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    
    if (opt_arttrg) call sub_arttargets(df,dist_trg)    
       
    do it_clb = 1,maxit_clbloop

        opt_trref =0
        opt_trref_hk = opt_trref
        
        print*, 'CALIB LOOP iteration # ', it_clb
        print*, ' '     
        
        print*, "MOMENTS"
        print*, "kyr: ", agg%ky(tclb)
        print*, "gyr: ", agg%gconsr(tclb)
        print*, "byr: ", agg%byr(tclb)
        print*, "ivt/ass: ", agg%ivty(tclb) 
        print*, "av HK", agg%h_j(jt,tclb)
        print*, "CL share", agg%clfrac(tclb)
        print*, "LHS share", agg%lhsfrac(tclb)  
        print*, "mon inv", agg%moninv(tclb)
        print*, "tinv", agg%tinv(tclb)
        print*, "tinv slope", agg%tinv_slope(tclb)
        print*, "tinv0 ", agg%t_j(jf,tclb)
        print*, "trgad", agg%tgrad(tclb)
        
        
        print*, "PARAMS"
        print*, "beta: ", agg%beta(tclb)
        print*, "lamtil: ", agg%psi_lambda(tclb)
        print*, "nu: ", agg%nu(tclb)
        print*, "Abar", agg%abar(tclb)
        print*, "phi", agg%phi(tclb)
        print*, "zeta_cl", agg%zeta_cl(1,tclb)
        print*, "kappah", agg%kappah(tclb)
        print*, "kappa", agg%kappat(tclb)
        print*, "kappai1", agg%kappai1(tclb)
        print*, "fitm", agg%fi1(tclb)
        
        opt_calib_loc=1
            
        opt_ge_upd = 1
        
        do optcalib_id = 0,0 !0,0 !1
            
            if (optcalib_id==0)then
                opt_lambda = 0
            else
                opt_lambda = 2
            endif
            
        
            ! allocate objects for organization of solvers
            ngs=f_ng(ngs,1,0,optcalib_id)
            allocate(fgs(ngs))
            allocate(xgs(ngs))
            allocate(X(ngs))
            allocate(Rss(ngs,ngs))
            allocate(Qss(ngs,ngs))
            allocate(diagRss(ngs))
            allocate(diagQss(ngs))
            allocate(sfagg(ngs+atr))
            
            ! Initial diagonal iteration matrix
            Rss=0.0
            diagRss(:)=wght_stst
            diagRss=1.0/diagRss
            call put_diag(diagRss,Rss)        
        
            ! ---------------------
           
            t0=tclb
            t1=tclb
        
            opt_vf_loop = 0
            
            ! compute scaling factors:
            call sub_makex(xgs,agg,t0,t1,ngs+atr,sfagg,0,optcalib_id)
            
            call sub_makex(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)       
        
            ! solution for equilibrium
            !if (opt_alg_stst==1 .and. optcalib_id==1) then    ! solution with Quasi-Newton method opt_alg_stst==1 
            !    intlj=.true.   
            !    reevalj=.true.
            !    call alg_qn(sub_solve_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
            !else    ! solution with Gauss-Seidel method
                call alg_gs(sub_solve_olg,fgs,xgs,ngs,check,wght_stst,6,tol_stst)
            !endif
        
            ! get values for x-variabless
            call sub_getx(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)
        
            ! test of aggregation
            call sub_testagg(agg,grid,t0,t1)
        
            print*, ' '
            print*, 'I am done with first gorup of SS calibration, nice! => SAVE'
            print*, ' '
            !pause
            
            open(unit=83,file='input/outerloop/params1.txt')
        
            write(83,'(29f50.16)') adj_fac_tr, avwage_tclb, agg%psi_lambda(tclb),agg%phi(tclb),agg%phi(tclb),  &
                agg%beta(tclb),agg%nu(tclb),agg%abar(tclb),agg%kappah(tclb),agg%kappat(tclb),agg%kappai1(tclb),agg%kappai2(tclb),agg%zeta_cl(1,tclb),agg%zeta_cl(2,tclb),agg%fi1(tclb),agg%h0norm(tclb),agg%omega_fin(tclb), &
                agg%kappah1(tclb),agg%kappah2(tclb),agg%tau_c(tclb),agg%gamdelta(tclb), agg%gammah_s(1,tclb),agg%gammah_s(2,tclb),agg%gammah_s(3,tclb),agg%gammah_s(4,tclb),agg%inv_mean(tclb),agg%kappap4(tclb),agg%iota(tclb), agg%h0delta(tclb)
        
            close (83)  
        
            ! open files
            open(unit=99,file='input/outerloop/outputSSouterloop.txt')
        
            do tc=t0,t1
                ! Outer loop variables	
	            write(99,'(12f50.16)') agg%ret(tclb), &
                    agg%wage(tclb),agg%wage_s(2,tclb),agg%wage_s(3,tclb),agg%wage_s(4,tclb), &
                    agg%rho_p(tc),agg%bqr(tclb),agg%avearn(tclb),agg%avwage(tclb),agg%psi_lambda(tc), agg%tau_c(tc),agg%tau_p(tclb)
                
                

            end do
            ! close files
            close(99)
           
            
            print*, ' '
            print*, 'SAVED SS CALIB1, move on to next '
            print*, ' '
        
            !pause
         
                    
            deallocate(fgs,xgs,Rss,Qss,diagRss,diagQss,sfagg,X)
            
        enddo
        
        
        
       ! pause
        
        opt_ge_upd = 0
        opt_vf_loop = 1
        opt_meas_loop = 1

        opt_pe = 1
        opt_pe_ind = 1
    
        print*, ' ' 
        print*, 'Uni to 3 param CALIB '
        print*, ' '
            
        opt_calib_loc=1
        opt_init_ss=1
    
        do opt_trref = 0,0 !1
            opt_trref_hk = opt_trref
            
            print*, "OPTTREF is ", opt_trref
            print*, ' '

            do ind_param=0,14 !7
            
                if (ind_param==0)then
                    opt_lambda = 2
                else
                    opt_lambda = 0
                endif
            
                if (ind_param==10)then
                    opt_sumstats_loc = 1
                else
                    opt_sumstats_loc = 0
                endif
            
                print*, ' '
                print*, 'Solving for ind_param ', ind_param
                print*, ' ' 
            
                ! UPDATE targets
                call sub_arttargets(df,dist_trg)
        
                !if (ind_param==4)then
                !    ngs = 2
                !elseif (ind_param==5)then
            
                if (ind_param==6)then
                    ngs = 4
                elseif (ind_param==2 .or. ind_param==11)then 
                    ngs = 2
                elseif (ind_param==12)then
                    ngs = 4
                elseif (ind_param==13)then
                    ngs = 4
                
                elseif (ind_param==14)then
                
                    ngs = 6
                
                else                
                    ngs=1
                endif
            
                allocate(fgs(ngs))
                allocate(xgs(ngs))
                allocate(Rss(ngs,ngs))
                allocate(Qss(ngs,ngs))
                allocate(diagRss(ngs))
                allocate(diagQss(ngs))
                allocate(sfagg(ngs))
            
                ! Initial diagonal iteration matrix
                Rss=0.0
                diagRss(:)=wght_stst
                diagRss=1.0/diagRss
                call put_diag(diagRss,Rss)         
        
                ! ---------------------
          
                t0=tclb
                t1=tclb
    
    
                ! compute scaling factors:
                !if (ind_param==4)then
                !    ind_param_loc = 1
                !    call sub_makex_2param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
                !
                !    call sub_makex_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )      
                !elseif (ind_param==5)then
                if (ind_param==6 )then
                    ind_param_loc = 1
                    call sub_makex_5param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
            
                    call sub_makex_5param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )
                
           
                elseif (ind_param==2)then
                
                    ind_param_loc = 6
                    call sub_makex_2param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
            
                    call sub_makex_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )                
                
                
                elseif (ind_param==11)then
                
                    ind_param_loc = 3
                    call sub_makex_2param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
            
                    call sub_makex_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )
                elseif (ind_param==12)then
                
                    ind_param_loc = 2
                    call sub_makex_2param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
            
                    call sub_makex_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )   
                elseif (ind_param==13)then
                
                    ind_param_loc = 5
                    call sub_makex_2param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
            
                    call sub_makex_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )       
                
                elseif (ind_param==14)then 
                
                    ind_param_loc = 2
                    call sub_makex_5param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param_loc )
            
                    call sub_makex_5param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )
                
                
                else           
            
                    call sub_makex_param(xgs,agg,t0,t1,ngs+atr,sfagg,0,ind_param)
            
                    call sub_makex_param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param)       
                endif
    
                if (ind_param==1)then
                    opt_mean_update=.true.
                else
                    opt_mean_update=.false.
                endif
                         
           
                ! solution for equilibrium
                if (opt_alg_stst==1) then    ! solution with Quasi-Newton method
                    intlj=.true.   
                    reevalj=.true.
                    !if (ind_param==4)then
                    !    call alg_qn(sub_calib2_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
                    !elseif (ind_param==5)then    
                    if (ind_param==6 .or. ind_param==14)then
                        call alg_qn(sub_calib5_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
    
    
                    !elseif (ind_param==5 .or. ind_param==8)then   
    
                    !     call alg_gs(sub_calib_olg,fgs,xgs,ngs,check,wght_stst,maxits_stst,tol_stst)
    
    
                
                    elseif (ind_param==9)then
                    
                        ! for the moment nothing, educ grad
                    
                    elseif (ind_param==2 .or. ind_param==11 .or. ind_param==13 )then
                    
                        print*, "here2", ind_param, ind_param_loc
                    
                        call alg_qn(sub_calib2_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
                    
                    elseif (ind_param==12 )then
                        call alg_qn(sub_calib2_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
                    
                        ! do nothing, money profile
                    
                    else
                    
                
                        call alg_qn(sub_calib_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
                    endif
            
                else    ! solution with Gauss-Seidel method
                    !if (ind_param==4)then
                    !    call alg_gs(sub_calib2_olg,fgs,xgs,ngs,check,wght_stst,maxits_stst,tol_stst)
                    !elseif (ind_param==5)then 
                    if (ind_param==6 .or. ind_param==14)then 
                        call alg_gs(sub_calib5_olg,fgs,xgs,ngs,check,wght_stst,maxits_stst,tol_stst)
               
                    elseif (ind_param==2 .or. ind_param==11 .or. ind_param==12 .or. ind_param==13)then
                    
                        call alg_gs(sub_calib2_olg,fgs,xgs,ngs,check,wght_stst,maxits_stst,tol_stst)
                    
                    elseif (ind_param == 9)then
                    
                        ! nothing
                    
                    else
                        call alg_gs(sub_calib_olg,fgs,xgs,ngs,check,wght_stst,maxits_stst,tol_stst)
                    endif
            
                
                endif
    
                ! get values for x-variabless
                !if (ind_param==4)then
                !    call sub_getx_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )
                !elseif (ind_param==5)then
                if (ind_param==6 .or. ind_param==14)then
                    call sub_getx_5param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )
                
                elseif (ind_param==9)then
                
                
                elseif (ind_param==2 .or. ind_param==11 .or. ind_param==12 .or. ind_param==13)then
                    call sub_getx_2param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param_loc )
                
                else
            
                    call sub_getx_param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param)
                endif
        
                !! test of aggregation
                !call sub_testagg(agg,grid,t0,t1)
            
                if (opt_trref==0)then
                    open(unit=83,file='input/outerloop/params1.txt')
                else
                    open(unit=83,file='input/outerloop/params1REF.txt')
                endif
            
        
                write(83,'(29f50.16)') adj_fac_tr, avwage_tclb, agg%psi_lambda(tclb),agg%phi(tclb),agg%phi(tclb),  &
                    agg%beta(tclb),agg%nu(tclb),agg%abar(tclb),agg%kappah(tclb),agg%kappat(tclb),agg%kappai1(tclb),agg%kappai2(tclb),agg%zeta_cl(1,tclb),agg%zeta_cl(2,tclb),agg%fi1(tclb),agg%h0norm(tclb),agg%omega_fin(tclb), &
                    agg%kappah1(tclb),agg%kappah2(tclb),agg%tau_c(tclb),agg%gamdelta(tclb), agg%gammah_s(1,tclb),agg%gammah_s(2,tclb),agg%gammah_s(3,tclb),agg%gammah_s(4,tclb),agg%inv_mean(tclb),agg%kappap4(tclb),agg%iota(tclb), agg%h0delta(tclb)
        
                close (83)  
            
                deallocate(fgs,xgs,Rss,Qss,diagRss,diagQss,sfagg)
             
            enddo ! ind_param

        enddo ! opt_trref
        
        !pause
    
    
        print*, "parameter now", agg%beta(t0), &
                agg%psi_lambda(t0), agg%nu(t0),agg%abar(t0), agg%kappah(t0) 
         
        call sub_arttargets(df,dist_trg)    
        
        if (dist_trg < epsi)then
            print*, "CALIB converged"
            exit
        else
            print*, "current max distance", dist_trg
            
        endif
        
        ! update dampening factor / increase weight on actual targets
        df =max(0.0_dp, df - df_incr )
        
        
    enddo
    
    
else
    opt_invconst = 0
    
    agg%gyr_str(:) = gyr_str
    agg%byr_str(:) = byr_str
    
    ! GE, pension, and TTS
    opt_calib_loc=1
    opt_init_ss=1
    opt_fin_ss = 0
  
    
    if (opt_trans==0)then ! ss comparison only
    
        opt_invconst = 0
    
        ! ------------------------------------------
        ! solution for steady state in period 1
        ! ------------------------------------------
        print*, ' '
        print*, '----------------------------------------'
        print*, 'Compute Model Solution BASELINE' 
        print*, '----------------------------------------'
        print*, ' '
    
        agg%gyr_str(tclb) = gyr_str
        
        agg%byr_str(tclb) = byr_str
    
        ! GE, pension, and TTS
        opt_calib_loc=1
        opt_init_ss=1
        opt_fin_ss = 0
        t0=tclb
        t1=tclb
    
        opt_vf_init = 1
    
        call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    
        call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    
        opt_invconst = 0
    
        opt_vf_init = 0
    
        opt_trref =0
        opt_trref_hk = opt_trref
    
        opt_ge_upd = 1
    
        do opt_trref=0,1
            
            opt_trref_hk = opt_trref
        
            if (opt_trref==0)then
                opt_pe_min = 0
                opt_pe_max = 0
            
            else
                opt_pe_min = 0
                opt_pe_max = 1
          
            endif
                
        
            do opt_pe_ind = opt_pe_max,opt_pe_min,-1
            
                opt_pe = opt_pe_ind
                
                if (opt_trref==1 .and. opt_pe==1)then
                    idmin = 1
                    idmax = 1
                else
                    if (opt_trref==0)then
                        idmin = 0
                        idmax = 0
                    else
                        idmin = 0
                        idmax = 1
                    endif
                
                endif
                    
    
            do optcalib_id = idmin,idmax !1,1 !1
            
            
            
                print*, ''
                print*, "calib_id, opt_pe, opt_trref ", optcalib_id, opt_pe, opt_trref
                print*, ''
            
                if (optcalib_id==1)then
                    opt_lambda=1
                else
                    opt_lambda=0
                endif
            
        
                ! allocate objects for organization of solvers
                ngs=f_ng(ngs,1,0,optcalib_id)
        
                allocate(fgs(ngs))
                allocate(xgs(ngs))
                allocate(X(ngs))
                allocate(Rss(ngs,ngs))
                allocate(Qss(ngs,ngs))
                allocate(diagRss(ngs))
                allocate(diagQss(ngs))
                allocate(sfagg(ngs+atr))
            
                ! Initial diagonal iteration matrix
                Rss=0.0
                diagRss(:)=wght_stst
                diagRss=1.0/diagRss
                call put_diag(diagRss,Rss)        
        
                ! ---------------------
           
                t0=tclb
                t1=tclb
        
                opt_vf_loop = 0
            
                ! compute scaling factors:
                call sub_makex(xgs,agg,t0,t1,ngs+atr,sfagg,0,optcalib_id)
            
                call sub_makex(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)       
        
                ! solution for equilibrium
                if (opt_alg_stst==0) then ! .and. optcalib_id==1) then    ! solution with Quasi-Newton method opt_alg_stst==1 
                    intlj=.true.   
                    reevalj=.true.
                    call alg_qn(sub_solve_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
                else    ! solution with Gauss-Seidel method
                    call alg_gs(sub_solve_olg,fgs,xgs,ngs,check,wght_stst,14,tol_stst)
                endif
        
                ! get values for x-variabless
                call sub_getx(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)
        
                ! test of aggregation
                call sub_testagg(agg,grid,t0,t1)
        
                print*, ' '
                print*, 'I am done with first gorup of SS calibration, nice! => SAVE'
                print*, ' '
                !pause
            
                if (opt_trref==0)then
                    open(unit=83,file='input/outerloop/params1.txt')
                else
                    open(unit=83,file='input/outerloop/params1REF.txt')
                endif
            
        
                write(83,'(29f50.16)') adj_fac_tr, avwage_tclb, agg%psi_lambda(tclb),agg%phi(tclb),agg%phi(tclb),  &
                    agg%beta(tclb),agg%nu(tclb),agg%abar(tclb),agg%kappah(tclb),agg%kappat(tclb),agg%kappai1(tclb),agg%kappai2(tclb),agg%zeta_cl(1,tclb),agg%zeta_cl(2,tclb),agg%fi1(tclb),agg%h0norm(tclb),agg%omega_fin(tclb), &
                    agg%kappah1(tclb),agg%kappah2(tclb),agg%tau_c(tclb),agg%gamdelta(tclb), agg%gammah_s(1,tclb),agg%gammah_s(2,tclb),agg%gammah_s(3,tclb),agg%gammah_s(4,tclb),agg%inv_mean(tclb),agg%kappap4(tclb),agg%iota(tclb), agg%h0delta(tclb)
        
                close (83)  
        
                ! open files
                if (opt_trref==0)then
                    open(unit=99,file='input/outerloop/outputSSouterloop.txt')
                else
                    open(unit=99,file='input/outerloop/outputSSouterloopREF.txt')
                endif
            
        
                do tc=t0,t1
                    ! Outer loop variables	
	                write(99,'(12f50.16)') agg%ret(tclb), &
                        agg%wage(tclb),agg%wage_s(2,tclb),agg%wage_s(3,tclb),agg%wage_s(4,tclb), &
                        agg%rho_p(tc),agg%bqr(tclb),agg%avearn(tclb),agg%avwage(tclb),agg%psi_lambda(tc), agg%tau_c(tc),agg%tau_p(tclb)
                end do
                ! close files
                close(99)
                    
                deallocate(fgs,xgs,Rss,Qss,diagRss,diagQss,sfagg,X)
        
        
                ! compute reform first in PE
            
            enddo
        
            enddo
        
        
        enddo
    
    
    
        print*, ' '
        print*, 'I am done with INITIAL SS, nice! => SAVE'
        print*, ' '
    
   
    
    else
    
        ! transition
        
        ! initial ss
        
        ! ------------------------------------------
        ! solution for steady state in period 1
        ! ------------------------------------------
        print*, ' '
        print*, '----------------------------------------'
        print*, 'Compute Model Solution BASELINE' 
        print*, '----------------------------------------'
        print*, ' '
    
        agg%gyr_str(:) = gyr_str
        
        agg%byr_str(:) = byr_str
    
        ! GE, pension, and TTS
        opt_calib_loc=1
        opt_init_ss=1
        opt_fin_ss = 0
        t0=1
        t1=1
    
        opt_vf_init = 1
        opt_ge_upd = 0
        
        opt_trref = 0
        opt_trref_hk = opt_trref
        opt_lambda = 0
        !if (opt_debt==1)then
        !    opt_lambda = 2
        !else
        !    opt_lambda = 1
        !endif
        
        !print*, "here", opt_inv_loc
        !pause
        call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    
        call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
        
        !opt_inv_loc = 0
    
        opt_invconst = 0
    
        opt_vf_init = 0
    
        opt_trref =0
        opt_trref_hk = opt_trref
    
        opt_ge_upd = 1 
        
        pe_ind_arr = (/-1,1,0  /)
    
        do opt_pe_ind =0,0 !opt_pe_in,opt_pe_in ! 0,0 !size(pe_ind_arr)
    
            opt_pe = opt_pe_ind !pe_ind_arr(opt_pe_ind)
            
            if (opt_trref==1 .and. opt_pe==1)then
                idmin = 1
                idmax = 1
            else
                if (opt_trref==0)then
                    idmin = 0
                    idmax = 0
                else
                    idmin = 1
                    idmax = 1
                endif
                
            endif
        
            print*, ''
            print*, "calib_id, opt_pe, opt_trref ", optcalib_id, opt_pe, opt_trref
            print*, ''
            
            optcalib_id = 1
        
            if (optcalib_id==1)then
                opt_lambda=2
            else
                opt_lambda = 0
            endif
                
            ! allocate objects for organization of solvers
            ngs=f_ng(ngs,1,0,optcalib_id)
        
            allocate(fgs(ngs))
            allocate(xgs(ngs))
            allocate(X(ngs))
            allocate(Rss(ngs,ngs))
            allocate(Qss(ngs,ngs))
            allocate(diagRss(ngs))
            allocate(diagQss(ngs))
            allocate(sfagg(ngs+atr))
            
            ! Initial diagonal iteration matrix
            Rss=0.0
            diagRss(:)=wght_stst
            diagRss=1.0/diagRss
            call put_diag(diagRss,Rss)        
        
            ! ---------------------
           
            t0=tclb
            t1=tclb
        
            opt_vf_loop = 0
            
            ! compute scaling factors:
            call sub_makex(xgs,agg,t0,t1,ngs+atr,sfagg,0,optcalib_id)
            
            call sub_makex(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)       
        
            ! solution for equilibrium
            if (opt_alg_stst==0) then ! .and. optcalib_id==1) then    ! solution with Quasi-Newton method opt_alg_stst==1 
                intlj=.true.   
                reevalj=.true.
                call alg_qn(sub_solve_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
            else    ! solution with Gauss-Seidel method
                call alg_gs(sub_solve_olg,fgs,xgs,ngs,check,wght_stst,0,tol_stst)
            endif
         
            ! get values for x-variabless
            call sub_getx(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)
        
            ! test of aggregation
            call sub_testagg(agg,grid,t0,t1)
        
            print*, ' '
            print*, 'I am done with INITIAL SS, nice! => SAVE', opt_pe
            print*, ' '
            !pause
            
            if (opt_trref==0)then
                open(unit=83,file='input/outerloop/params1.txt')
            else
                open(unit=83,file='input/outerloop/params1REF.txt')
            endif
            
        
            write(83,'(29f50.16)') adj_fac_tr, avwage_tclb, agg%psi_lambda(tclb),agg%phi(tclb),agg%phi(tclb),  &
                agg%beta(tclb),agg%nu(tclb),agg%abar(tclb),agg%kappah(tclb),agg%kappat(tclb),agg%kappai1(tclb),agg%kappai2(tclb),agg%zeta_cl(1,tclb),agg%zeta_cl(2,tclb),agg%fi1(tclb),agg%h0norm(tclb),agg%omega_fin(tclb), &
                agg%kappah1(tclb),agg%kappah2(tclb),agg%tau_c(tclb),agg%gamdelta(tclb), agg%gammah_s(1,tclb),agg%gammah_s(2,tclb),agg%gammah_s(3,tclb),agg%gammah_s(4,tclb),agg%inv_mean(tclb),agg%kappap4(tclb),agg%iota(tclb), agg%h0delta(tclb)
        
            close (83)  
        
            ! open files
            if (opt_trref==0)then
                open(unit=99,file='input/outerloop/outputSSouterloop.txt')
            else
                open(unit=99,file='input/outerloop/outputSSouterloopREF.txt')
            endif
            
        
            do tc=t0,t1
                ! Outer loop variables	
	            write(99,'(12f50.16)') agg%ret(tclb), &
                    agg%wage(tclb),agg%wage_s(2,tclb),agg%wage_s(3,tclb),agg%wage_s(4,tclb), &
                    agg%rho_p(tc),agg%bqr(tclb),agg%avearn(tclb),agg%avwage(tclb),agg%psi_lambda(tc), agg%tau_c(tc),agg%tau_p(tclb)
            end do
            ! close files
            close(99) 
                     
            deallocate(fgs,xgs,Rss,Qss,diagRss,diagQss,sfagg,X)
        
        enddo ! opt_pe
        
        do tc =nt,nt
            grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,tc) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,1)
            grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,tc) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,1)
            grid%Phi2guess(:,:,:,:,:,:,:,tc) = grid%Phi2guess(:,:,:,:,:,:,:,1)
            grid%Phi2guess_s(:,:,:,:,:,:,:,tc) = grid%Phi2guess_s(:,:,:,:,:,:,:,1)
            grid%sav2guess(:,:,:,:,:,:,:,tc) =grid%sav2guess(:,:,:,:,:,:,:,1)
            grid%Phi_s(:,:,tc) = grid%Phi_s(:,:,1)
            
            agg%pop_j(:,tc) = agg%pop_j(:,1)
            
            grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
            grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%Phi_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
            
            agg%bq(tc) = agg%bq(1) 
            agg%bqr(tc) = agg%bqr(1) 
            agg%clcost(tc) =agg%clcost(1) 
            agg%avwage(tc) = agg%avwage(1)
        enddo 
       


        ! input of staring values
        call sub_inoutloop_run(agg,1)    
        
        !agg%psi_lambda(2:nt) = 0.7319975197988334_dp
        if (opt_readin_trans==1)then
            ! read in previously computed solution
            if (opt_curver==1)then
                call sub_inoutloop_trans_cur(agg)
            else
                call sub_inoutloop_trans(agg)
            endif
        else
            agg%debt = agg%debt(1)
        endif
        
        !agg%psi_lambda(2:nt) = 0.799832482655951_dp !agg%psi_lambda(1)  * 0.98_dp !0.97_dp !0.7189322659029489_dp !0.5949939112536221_dp
        !agg%ret(2:nt) = 0.13_dp

        ! agg%tau_c(nt) = 0.080_dp 
            
         
        
         
        ! here would be the outer-outer loop for tau with debt, though probably it is better to do it as two sequential loops
        
        ! initialize debt path
        !agg%debt(2:nt-1) = agg%debt(1)
        
        it_lambda = 1
        do while (it_lambda < 5 ) 
            
            print*, "in the outer outer loop iteration number", it_lambda, agg%psi_lambda(nt)
            
            lambda_old = agg%psi_lambda(nt)
        
        ! 9 SS
        if (opt_fss) then   ! if computation of final steady state switched on
            ! ------------------------------------------
            ! solution for steady state in period nt
            ! ------------------------------------------
            print*, ' '
            print*, '----------------------------------------'
            print*, 'Compute Final Steady State BASELINE'
            print*, '----------------------------------------'
            print*, ' '
                
            ! GE, pension, and TTS
            opt_calib_loc=1
            opt_init_ss=0
            opt_fin_ss = 1
            t0=nt
            t1=nt
    
            opt_vf_init = 1
            if (opt_curver==0)then
                
                opt_trref=1 !ACHTUNG1
            else
                opt_trref=0
            endif
            
            opt_trref_hk = opt_trref
            
            if (opt_margprg==1) tau_pr = 0.104_dp
                
            opt_ge_upd = 0
                
            opt_lambda = 0
    
            call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
    
            call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    
            opt_vf_init = 0
            opt_ge_upd = 1
            
            !pause
    
            !opt_pe = 0
            do opt_pe_ind = 1,1,-1 ! first PE, then GE
    
                opt_pe = opt_pe_ind
                
                if (opt_trref==1 .and. opt_pe==1)then
                    idmin = 1
                    idmax = 1
                else
                    if (opt_trref==0)then
                        idmin = 0
                        idmax = 0
                    else
                        idmin = 1
                        idmax = 1
                    endif
                
                endif
              
                do optcalib_id = 0,0 !idmin,idmax !1,1 !1
        !    
        !    
        !    
                    print*, ''
                    print*, "calib_id, opt_pe, opt_trref ", optcalib_id, opt_pe, opt_trref
                    print*, ''
            
                    if (optcalib_id==1)then
                        !if (opt_debt==1)then
                        !    opt_lambda=2
                        !else
                            opt_lambda=2
                        !endif
                        
                    else
                        opt_lambda=0
                    endif
            
                    opt_lambda=2        

                    ! allocate objects for organization of solvers
                    ngs=f_ng(ngs,1,0,optcalib_id)
                    
                    allocate(fgs(ngs))
                    allocate(xgs(ngs))
                    allocate(X(ngs))
                    allocate(Rss(ngs,ngs))
                    allocate(Qss(ngs,ngs))
                    allocate(diagRss(ngs))
                    allocate(diagQss(ngs))
                    allocate(sfagg(ngs+atr))
                    
                    ! Initial diagonal iteration matrix
                    Rss=0.0
                    diagRss(:)=wght_stst
                    diagRss=1.0/diagRss
                    call put_diag(diagRss,Rss)        
        
                    ! compute scaling factors:
                    call sub_makex(xgs,agg,t0,t1,ngs+atr,sfagg,0,optcalib_id)
            
                    call sub_makex(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)       
        
                    !! solution for equilibrium
                    !if (opt_alg_stst==0) then ! .and. optcalib_id==1) then    ! solution with Quasi-Newton method opt_alg_stst==1 
                    !    intlj=.true.   
                    !    reevalj=.true.
                    !    call alg_qn(sub_solve_olg,fgs,xgs,ngs,Qss,Rss,intlj,reevalj,check,rst_stst,maxln_stst,maxits_stst,stpmx_stst,tol_stst,.true.)
                    !else    ! solution with Gauss-Seidel method
                        call alg_gs(sub_solve_olg,fgs,xgs,ngs,check,wght_stst,0,tol_stst)
                    !endif
        
                    ! get values for x-variabless
                    call sub_getx(xgs,agg,t0,t1,ngs,sfagg,1,optcalib_id)
        
                    ! test of aggregation
                    call sub_testagg(agg,grid,t0,t1)
        
                    print*, ' '
                    print*, 'I am done with FINAL SS, nice! => SAVE'
                    print*, ' '
                    !pause
                    
                    if (opt_trref==0)then
                        open(unit=83,file='input/outerloop/params1.txt')
                    else
                        open(unit=83,file='input/outerloop/params1REF.txt')
                    endif
            
        
                    
                    write(83,'(29f50.16)') adj_fac_tr, agg%avwage(t0), agg%psi_lambda(t0),agg%phi(tclb),agg%phi(tclb),  &
                        agg%beta(tclb),agg%nu(tclb),agg%abar(tclb),agg%kappah(t0),agg%kappat(tclb),agg%kappai1(tclb),agg%kappai2(tclb),agg%zeta_cl(1,tclb),agg%zeta_cl(2,tclb),agg%fi1(tclb),agg%h0norm(tclb),agg%omega_fin(tclb), &
                        agg%kappah1(tclb),agg%kappah2(tclb),agg%tau_c(t0),agg%gamdelta(tclb), agg%gammah_s(1,tclb),agg%gammah_s(2,tclb),agg%gammah_s(3,tclb),agg%gammah_s(4,tclb),agg%inv_mean(tclb),agg%kappap4(tclb),agg%iota(tclb), agg%h0delta(tclb)
        
                    close (83)  
            
                    ! open files
                    if (opt_trref==0)then
                        open(unit=99,file='input/outerloop/outputSSouterloop.txt')
                    else
                        open(unit=99,file='input/outerloop/outputSSouterloopREF.txt')
                    endif
            
        
                    do tc=t0,t1
                        ! Outer loop variables	
	                    write(99,'(12f50.16)') agg%ret(t0), &
                            agg%wage(t0),agg%wage_s(2,t0),agg%wage_s(3,t0),agg%wage_s(4,tclb), &
                            agg%rho_p(t0),agg%bqr(t0),agg%avearn(t0),agg%avwage(t0),agg%psi_lambda(tc), agg%tau_c(tc),agg%tau_p(t0)
                    end do
                    ! close files
                    close(99)                    
                    deallocate(fgs,xgs,Rss,Qss,diagRss,diagQss,sfagg,X)
        
           
            
                enddo
            
            enddo
        
        do tc = 2,nt
            grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,tc) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,nt)
            grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,tc) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,nt)
            grid%Phi2guess(:,:,:,:,:,:,:,tc) = grid%Phi2guess(:,:,:,:,:,:,:,nt)
            grid%Phi2guess_s(:,:,:,:,:,:,:,tc) = grid%Phi2guess_s(:,:,:,:,:,:,:,nt)
            grid%sav2guess(:,:,:,:,:,:,:,tc) =grid%sav2guess(:,:,:,:,:,:,:,nt)
            grid%Phi_s(:,:,tc) = grid%Phi_s(:,:,nt)
        enddo

        
            
        endif


        

  
            
            ! TRANSITION
            ! ------------------------------------------
            ! solution for transition
            ! ------------------------------------------
            print*, ' '
            print*, '----------------------------------------'
            print*, 'Compute Transition BASELINE'
            print*, '----------------------------------------'
            print*, ' '
            
            ! for the debt path: impose convergence to SS
            if (opt_bconst==0)then
                agg%debt(nt-2:nt) = agg%debt(nt)     
            else
               
            endif   
            opt_calib_loc=1
            opt_init_ss=0
            opt_fin_ss = 0
            t0=2
            !if (opt_debt==1)then
                t1=nt ! ss is function of transition, resolve for byr
            !else
            !    t1=nt-1
            !endif
            
    
            opt_trref=1 ! ACHTUNG
            opt_trref_hk = opt_trref
            
            opt_ge_upd = 0
    
            !call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
            !
            !call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
    
            opt_ge_upd = 1
    
           ! opt_pe = 1
            do opt_pe_ind = opt_pe_in,opt_pe_in,-1 ! first PE, then GE
            
                opt_pe = opt_pe_ind
                
                !print*, "ARRIVED here"
                !pause 
    
                if (opt_trref==1 .and. opt_pe==1)then
                    idmin = 1
                    idmax = 1
                else
                    if (opt_trref==0)then
                        idmin = 0
                        idmax = 0
                    else
                        idmin = 1
                        idmax = 1
                    endif
                
                endif
                    
    
                do optcalib_id = idmin,idmax !1,1 !1
            
            
            
                    print*, ''
                    print*, "calib_id, opt_pe, opt_trref ", optcalib_id, opt_pe, opt_trref
                    print*, ''
             
                    if (optcalib_id==1)then
                        !if (opt_debt==1)then
                        !    opt_lambda=2
                        !else
                            opt_lambda=2
                        !endif
                        
                    else
                        if (opt_bconst==0)then
                            opt_lambda=0
                        else
                            opt_lambda=2 
                        endif         
                    endif
                
                    ! allocate objects for organization of solvers
                    ngs=f_ng(ngs,1,0,optcalib_id)
                    
                    allocate(fgs(ngs))
                    allocate(xgs(ngs))
                    allocate(X(ngs))
                    allocate(Rss(ngs,ngs))
                    allocate(Qss(ngs,ngs))
                    allocate(diagRss(ngs))
                    allocate(diagQss(ngs))
                    allocate(sfagg(ngs+atr))
                    
                    ! Initial diagonal iteration matrix
                    Rss=0.0
                    diagRss(:)=wght_stst
                    diagRss=1.0/diagRss
                    call put_diag(diagRss,Rss)        
            
            
                    ! allocate objects for organization of solvers
                   
                    ngtr=f_ng(ngs,t1-t0+1,1,1)
                   
                    
                    print*, "here"
    
                    allocate(fgstr(ngtr))
                    allocate(xgstr(ngtr))
        
                    print*, "allocated"
   
                    ! compute scaling factors:
                    call sub_makex(xgs,agg,t0,t1,ngs+atr,sfagg,0,optcalib_id) 
                
                    ! set starting values and make xgstr:
                    call sub_makex(xgstr,agg,t0,t1,ngs+atr,sfagg,3,1) ! do this only once, of cours
            
        
                    print*, "made"

                    !call sub_solvehh_main(agg,demo,grid,pol,t0,t1,1)
                    !
                    !call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) 
                    !
                    !print*, "tested"
            
                    ! iteration matrix:
                    if (opt_alg_trans==1) then
                        Rtr=Rss
                        Qtr=Qss
                    elseif (opt_alg_trans==2) then
                        Rtr=0.0 
                        diagRss(:)=wght_trans
                        diagRss=1.0/diagRss
                        call put_diag(diagRss,Rtr)
        
                        Qtr=0.0
                        diagQss(:)=1.0_dp
                        diagQss=1.0/diagQss
                        call put_diag(diagQss,Qtr)
                    endif
                
                    print*, 'compute transition' !, FULL KSI'
                    
                    if (opt_bconst==0)then

                        if (it_lambda==1) then
                            if (opt_alg_trans<=2) then
                                call alg_gsqn(sub_solve_olg,fgstr,xgstr,ngtr,Qtr,Rtr,check,rst_trans,maxln_trans,maxits_trans,.true.,stpmx_trans,tol_trans,t1-t0+1)
                            else
                                call alg_gs(sub_solve_olg,fgstr,xgstr,ngtr,check,wght_trans,0,tol_trans)
                            endif   
                        	! get x-variables
                    		call sub_getx(xgstr,agg,t0,t1,ngs+atr,sfagg,2,1) 
                        endif
                    
                        call sub_eval_debt(agg%psi_lambda(2),agg%debt(2),agg,demo,grid,pol,stat,lc,t0,t1)

                    endif
                    ! set starting values and make xgstr:
                    call sub_makex(xgstr,agg,t0,t1,ngs+atr,sfagg,3,1) ! do this only once, of cours
            
  
                    
                    
                    if (opt_alg_trans<=2) then
                        call alg_gsqn(sub_solve_olg,fgstr,xgstr,ngtr,Qtr,Rtr,check,rst_trans,maxln_trans,maxits_trans,.true.,stpmx_trans,tol_trans,t1-t0+1)
                    else
                        call alg_gs(sub_solve_olg,fgstr,xgstr,ngtr,check,wght_trans,4,tol_trans)
                    endif                
            
                    ! test of aggregation: 
                    ! here only up to period t1-1 bec this is last period of transition
                    ! reason: if computation stops before steady state in population is reached (e.g., if ntp<2500), 
                    ! then resource constraints in tc=nt do not hold (bec. there is not steady state)
                    call sub_testagg(agg,grid,t0+1,t1-1)
    
                    ! get x-variables
                    call sub_getx(xgstr,agg,t0,t1,ngs+atr,sfagg,2,1) 
            
                    
            
                    print*, ' '
                    print*, 'I am done with TRANSITION, nice! => SAVE'
                    print*, ' '
                    !pause
            
                    
                    
                    deallocate(fgs,xgs,X,Rss,Qss,diagRss,diagQss,sfagg,fgstr,xgstr)
        
            
            
            enddo
            
            enddo



            
            
                        
            ! open files
                    open(unit=99,file='input/outerloop/outputouterloopT.txt')
                    open(unit=991,file='output/aggvarsT.txt')
                    open(unit=992,file='output/aggvarsT2.txt')
        
                    do tc=1,nt
                        ! Outer loop variables TRANSITION	
	                    write(99,'(12f50.16)') agg%ret(tc), &
                            agg%wage(tc),agg%wage_s(2,tc),agg%wage_s(3,tc),agg%wage_s(4,tc), &
                            agg%rho_p(tc),agg%bqr(tc),agg%avearn(tc),agg%avwage(tc),agg%psi_lambda(tc), agg%tau_c(tc), agg%tau_p(tc)
                        
                        ! aggregate variables	
	                    write(991,'(39f50.16)') agg%cap(tc), agg%lab(tc), agg%hrs(tc), agg%gdp(tc), agg%clshr_tot(tc),  & 
                            agg%hk_tot(tc), agg%moninv_tot(tc), agg%tinv_tot(tc), agg%debt(tc), agg%byr(tc), agg%totrev(tc), &
                            agg%clshr_jasp(3,1,tc),agg%clshr_jasp(3,2,tc) ,agg%clshr_jasp(3,3,tc), agg%clshr_jasp(2,1,tc),agg%clshr_jasp(2,2,tc) ,agg%clshr_jasp(2,3,tc), &
                            agg%hk_jasp(3,1,tc),agg%hk_jasp(3,2,tc) ,agg%hk_jasp(3,3,tc), agg%hk_jasp(2,1,tc),agg%hk_jasp(2,2,tc) ,agg%hk_jasp(2,3,tc), & 
                            agg%hk_jasp(1,1,tc),agg%hk_jasp(1,2,tc) ,agg%hk_jasp(1,3,tc), agg%wage_inc_s(1,tc), agg%wage_inc_s(2,tc),agg%wage_inc_s(3,tc), & 
                            agg%fe_s(1,tc), agg%fe_s(2,tc),agg%fe_s(3,tc), agg%h_j(jt,tc),agg%ivt_tot(tc),agg%cons(tc),sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,3,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,2,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                        
                        write(992,'(15f50.16)') agg%hk_tot(tc), agg%phi_s_tot(1,tc), agg%phi_s_tot(2,tc), agg%phi_s_tot(3,tc), agg%phi_s_tot(4,tc), agg%moninv(tc), agg%tinv(tc), agg%tot_inc(tc), &
                            agg%wage_inc(tc), agg%labinctaxrv(tc)/agg%wage_inc(tc),agg%avtax(tc),agg%avtax_av(tc), agg%gcons(tc), agg%igov(tc), agg%clsubs(tc)
                        ! compute pdv of totrev
                        avpdv = (1.0_dp + agg%ret(1)  * (1.0_dp - agg%tau_k(1)) )**(tc - 1) * earn_si_lo
                        
                        totrevpdv = totrevpdv + agg%totrev(tc) / avpdv
                   
                    end do
                    ! close files
                    close(99)
                    close(991)
                    close(992)
                    
                    
                    

            dist_lambda = abs(agg%psi_lambda(2) - lambda_old)
            
            print*, "dist in lambda loop", dist_lambda,agg%psi_lambda(2), totrevpdv
            
            if (it_lambda>1 .and. dist_lambda < epsi)then
                exit
            else
                lambda_old = agg%psi_lambda(2)
                it_lambda = it_lambda + 1
            endif
            
            
            enddo

        if (opt_check) then
            tc=1
            print*, 'returns: ', agg%ret(tc)
            print*, 'wages: '
            do sc=1,ns
        
                print*, agg%wage_s(sc,tc), ' '
        
            end do
            print*, ' '
        
            print*, 'taxes: ', agg%tau_p(tc)
        endif

            endif
    
        endif


contains

    ! ---------------------------------------------------------------------------  
    subroutine sub_arttargets(df,dist)
    
    implicit none
    real(dp),intent(in):: df
    real(dp),intent(out):: dist
    real(dp):: dist_vec(19)
    
    dist_vec = 0.0_dp
    
    dist_vec(1) = abs( ky_str - agg%ky(tclb) )
    !dist_vec(2) = abs( gyr_str - agg%gconsr(tclb) )
    dist_vec(2) = abs( byr_str - agg%byr(tclb) )
    dist_vec(3) = abs( ivty_str - agg%ivty(tclb) )
    dist_vec(4) = abs( hk_str - agg%h_j(jt,tclb) )
    dist_vec(5) = abs( clfrac_str - agg%clfrac(tclb) )
    dist_vec(6) = abs( dropfrac_str - agg%dropfrac(tclb) )
    dist_vec(7) = abs( tinv_str - agg%tinv(tclb) )
    dist_vec(8) =abs(demo%betta1_dat(1) - agg%betta1(tclb))     ! abs( tinv_slope_str - agg%tinv_slope(tclb) )
    dist_vec(9) =abs(demo%betta2_dat(1) - agg%betta2(tclb)) 
   ! dist_vec(10) = abs( moninv_str - agg%moninv(tclb) )
    dist_vec(10) = abs( t1_str - agg%t_j(jf,tclb) )
  !  dist_vec(11) = abs( tgrad_str - agg%tgrad(tclb) )
    
    !dist_vec(12) = abs( clkid_lhs_str - agg%clkid_lhs_str(tclb) )
    dist_vec(12) = abs( clkid_cl_str - agg%clkid_cl_str(tclb) )
    dist_vec(14) = abs( agg%hk_hs_str(tclb) - agg%h_j(jt+1,tclb) )
    
    dist_vec(15) =abs(demo%metta1_dat(1) - agg%metta1(tclb))     ! abs( tinv_slope_str - agg%tinv_slope(tclb) )
    dist_vec(16) =abs(demo%metta2_dat(1) - agg%metta2(tclb)) 
    
    dist_vec(17:19) = abs( agg%meanwage_s(:,tclb) -1.0_dp )
    
    dist = maxval(dist_vec)
    
    if (dist > epsi) then
    
        ! GROUP 1
        ! asset income ratio
        agg%ky_str(tclb) = ky_str * (1.0_dp - df) + agg%ky(tclb) * df
        ! g / y           
        agg%gyr_str(tclb) = gyr_str !* (1.0_dp - df) + agg%gconsr(tclb) * df
        
        ! b / y 
        agg%byr_str(tclb) = byr_str
    
        ! GROUP 2
        ! ivt as a fraction of tot assets 
        agg%ivty_str(tclb) = ivty_str* (1.0_dp - df) + agg%ivty(tclb) * df
    
        ! GROUP 3
        ! mean HK at age jt
        agg%hk_str(tclb) = hk_str* (1.0_dp - df) + agg%h_j(jt,tclb) * df
    
        !print*, "here", hsfrac_str, agg%hsfrac(:)
        !pause
        
        ! GROUP 4
        ! fraction of hs
        agg%dropfrac_str(tclb) = dropfrac_str* (1.0_dp - df) + agg%dropfrac(tclb) * df
        ! fraction of cl    
        agg%clfrac_str(tclb) = clfrac_str* (1.0_dp - df) +agg%clfrac(tclb) * df
        
        ! GROUP 5
        ! annual per child inv TIME / ft_lab    
        agg%tinv_str(tclb) = tinv_str* (1.0_dp - df) + agg%tinv(tclb) * df
        agg%tinv_slope_str(tclb) = tinv_slope_str * (1.0_dp - df) + agg%tinv_slope(tclb) * df
        
        agg%betta1_str(tclb) = demo%betta1_dat(1) * (1.0_dp - df) + agg%betta1(tclb) * df
        agg%betta2_str(tclb) = demo%betta2_dat(1) * (1.0_dp - df) + agg%betta2(tclb) * df
        
        agg%metta1_str(tclb) = demo%metta1_dat(1) * (1.0_dp - df) + agg%metta1(tclb) * df
        agg%metta2_str(tclb) = demo%metta2_dat(1) * (1.0_dp - df) + agg%metta2(tclb) * df
        
        ! annual per child inv MON / earn_ref
        agg%moninv_str(tclb) = moninv_str * (1.0_dp - df) +  agg%moninv(tclb) * df
    
        ! GROUP 6
        ! time inv at child age 1
        agg%tinv0_str(tclb) = t1_str* (1.0_dp - df) + agg%t_j(jf,tclb) * df
    
        ! GROUP 7
        ! education gradient time [take weighted average of two]
        agg%tgrad_str(tclb) = tgrad_str* (1.0_dp - df) + agg%tgrad(tclb) * df
        
        agg%clcosty_str(tclb) = fee_flow_frac* (1.0_dp - df) + agg%clcosty_str(tclb) * df
        
        ! GROUP 8
        ! slope of money profile, NOT now
        agg%moninv_slope_str(tclb) = moninv_slope_str * (1.0_dp - df) +  agg%moninv_slope(tclb) * df
        
        agg%clkid_cl_str(tclb) = clkid_cl_str * (1.0_dp - df) +  agg%clkid_cl(tclb) * df
        agg%clkid_lhs_str(tclb) = clkid_lhs_str * (1.0_dp - df) +  agg%clkid_lhs(tclb) * df
        
        agg%meanwage_s_str(:,tclb) = 1.0_dp * (1.0_dp - df) + agg%meanwage_s(:,tclb) * df
        
    else
        
        print*, "calibration converged!"
        
        
        pause
        
    endif
        
    
    end subroutine sub_arttargets
    ! ---------------------------------------------------------------------------  

    
    

    ! ---------------------------------------------------------------------------    
    subroutine sub_solve_olg(fg,xg,ng)
    ! function called from solver for solution of olg model
    implicit none  
    integer,intent(in)::ng
    real(dp),intent(in)::xg(ng)
    real(dp),intent(out)::fg(ng)
    
    real(dp),allocatable::xg_new(:)
    logical,parameter::opt_check=.false.

    ! get values for x-variables
    if (t1==t0) then    ! steady state
        call sub_getx(xg,agg,t0,t1,ngs,sfagg,1,optcalib_id)
    else                ! transition
        call sub_getx(xg,agg,t0,t1,ngs+atr,sfagg,2,optcalib_id)
    endif
    
    ! solution for OLG equilibrium, given settings
    call sub_olg_equil(stat,agg,demo,grid,lc,pol,t0,t1)
    
    !if (t0<t1 .and. opt_debt == 1)then ! transition, here the debt loop goes
    !    call sub_solve_debt(agg%psi_lambda(nt),agg,demo,grid,pol,stat,lc,t0,t1)
    !    print*, "solved debt loop", agg%psi_lambda(nt)
    !endif
    
        
    ! make values of x-variables:
    if (t1==t0) then    ! steady state
        call sub_makex(xg_new,agg,t0,t1,ngs,sfagg,1,optcalib_id)
    else                ! transition
        call sub_makex(xg_new,agg,t0,t1,ngs+atr,sfagg,2,optcalib_id)
    endif
    
    if (optcalib_id==1)then
        
        fg(:)=xg(:)-xg_new(:) 
        
       ! fg(1:ngs-nprm_clb)=xg(1:ngs-nprm_clb)-xg_new(1:ngs-nprm_clb) ! not for calib parameters
       ! !!fg(ngs-nprm_clb+1)=agg%ky(tclb) - agg%ky_str(tclb)
       ! !
       !! if (opt_trref ==0)then
       !     fg(ngs-nprm_clb+nprm_clb) = agg%gconsr(tclb) - agg%gyr_str(tclb)
       !! else
       !!     fg(ngs-nprm_clb+nprm_clb) = agg%gcons(tclb) - agg%gcons_base(tclb)
       !! endif
        
        print*, "distance1", xg(1),xg_new(1) 
        print*, "distance2", xg(2),xg_new(2) 
        print*, "distance3", xg(3),xg_new(3) 
        print*, "distance4", xg(4),xg_new(4) 
        print*, "distance5", xg(5),xg_new(5) 
        print*, "distance6", xg(6),xg_new(6) 
        print*, "distance7", xg(7),xg_new(7)
        print*, "distance8", xg(8),xg_new(8)
        
    else    
        fg(:)=xg(:)-xg_new(:) 
        
        print*, "distance1", xg(1),xg_new(1) 
        print*, "distance2", xg(2),xg_new(2) 
        print*, "distance3", xg(3),xg_new(3) 
        print*, "distance4", xg(4),xg_new(4) 
        print*, "distance5", xg(5),xg_new(5) 
        print*, "distance6", xg(6),xg_new(6) 
        print*, "distance7", xg(7),xg_new(7)
        
    endif
    
    
    if (opt_check ) then
        ! print*, 'fgs is: ', fgs(:)
        ! print*, 'xgs_new is: ', xgs_new(:)  
        print*, 'taxes: ',  agg%tau_p(t0:t1)
        print*, 'return: ', agg%ret(t0:t1)  
        if ( (t1-t0+1)>1 ) pause
    endif
    
    if (t1>t0)then
        open(unit=99,file='input/outerloop/outputouterloopT.txt')
        open(unit=991,file='output/aggvarsT.txt')
        open(unit=992,file='output/aggvarsT2.txt')
        do tc=1,t1
            ! Outer loop variables TRANSITION	
	        write(99,'(12f50.16)') agg%ret(tc), &
                agg%wage(tc),agg%wage_s(2,tc),agg%wage_s(3,tc),agg%wage_s(4,tc), &
                agg%rho_p(tc),agg%bqr(tc),agg%avearn(tc),agg%avwage(tc),agg%psi_lambda(tc), agg%tau_c(tc), agg%tau_p(tc)
                        
        
            ! aggregate variables	
	        write(991,'(39f50.16)') agg%cap(tc), agg%lab(tc), agg%hrs(tc), agg%gdp(tc), agg%clshr_tot(tc),  & 
                agg%hk_tot(tc), agg%moninv_tot(tc), agg%tinv_tot(tc), agg%debt(tc), agg%byr(tc), agg%totrev(tc), &
                agg%clshr_jasp(3,1,tc),agg%clshr_jasp(3,2,tc) ,agg%clshr_jasp(3,3,tc), agg%clshr_jasp(2,1,tc),agg%clshr_jasp(2,2,tc) ,agg%clshr_jasp(2,3,tc), &
                agg%hk_jasp(3,1,tc),agg%hk_jasp(3,2,tc) ,agg%hk_jasp(3,3,tc), agg%hk_jasp(2,1,tc),agg%hk_jasp(2,2,tc) ,agg%hk_jasp(2,3,tc), & 
                agg%hk_jasp(1,1,tc),agg%hk_jasp(1,2,tc) ,agg%hk_jasp(1,3,tc), agg%wage_inc_s(1,tc), agg%wage_inc_s(2,tc),agg%wage_inc_s(3,tc), & 
                agg%fe_s(1,tc), agg%fe_s(2,tc),agg%fe_s(3,tc), agg%h_j(jt,tc),agg%ivt_tot(tc),agg%cons(tc),sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,3,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,2,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                   
            
           write(992,'(15f50.16)') agg%hk_tot(tc), agg%phi_s_tot(1,tc), agg%phi_s_tot(2,tc), agg%phi_s_tot(3,tc), agg%phi_s_tot(4,tc), agg%moninv(tc), agg%tinv(tc), agg%tot_inc(tc), &
                            agg%wage_inc(tc), agg%labinctaxrv(tc)/agg%wage_inc(tc),agg%avtax(tc),agg%avtax_av(tc), agg%gcons(tc), agg%igov(tc), agg%clsubs(tc)        
            
        end do
        ! close files 
        close(99)
        close(991)
        close(992)
    endif
    end subroutine sub_solve_olg
    ! ---------------------------------------------------------------------------    
    
    
    ! ---------------------------------------------------------------------------    
    subroutine sub_solve_olg_debt(fg,xg,ng)
    ! function called from solver for solution of olg model
    implicit none  
    integer,intent(in)::ng
    real(dp),intent(in)::xg(ng)
    real(dp),intent(out)::fg(ng)
    
    real(dp),allocatable::xg_new(:)
    logical,parameter::opt_check=.false.
    integer:: t0_loc, t1_loc
    
    t0_loc = 2
    t1_loc = nt

    call sub_getx_param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param)
  
    
    ! solution for OLG equilibrium, given settings
    agg%psi_lambda(2:nt)  =agg%psi_lambda(t0)
    print*, "current input lambda", agg%psi_lambda(t0)
    opt_debt= 1
    call sub_olg_equil(stat,agg,demo,grid,lc,pol,t0_loc,t1_loc)
    opt_debt = 0
    ! make values of x-variables:
    call sub_makex_param(xgs,agg,t0,t1,ngs,sfagg,1,ind_param)
   
    
    print*, "inside olg_debt", ng, agg%debt(1),agg%debtss(1)
    fg(1)=agg%debtss(1)-agg%debt(1) 
    
    
    open(unit=99,file='input/outerloop/outputouterloopT.txt')
    open(unit=991,file='output/aggvarsT.txt')
    open(unit=992,file='output/aggvarsT2.txt')    
    do tc=1,nt
        ! Outer loop variables TRANSITION	
	    write(99,'(11f50.16)') agg%ret(tc), &
            agg%wage_s(2,tc),agg%wage_s(3,tc),agg%wage_s(4,tc), &
            agg%rho_p(tc),agg%bqr(tc),agg%avearn(tc),agg%avwage(tc),agg%psi_lambda(tc), agg%tau_c(tc), agg%tau_p(tc)
                        
        
        ! aggregate variables	
	    write(991,'(39f50.16)') agg%cap(tc), agg%lab(tc), agg%hrs(tc), agg%gdp(tc), agg%clshr_tot(tc),  & 
            agg%hk_tot(tc), agg%moninv_tot(tc), agg%tinv_tot(tc), agg%debt(tc), agg%byr(tc), agg%totrev(tc), &
            agg%clshr_jasp(3,1,tc),agg%clshr_jasp(3,2,tc) ,agg%clshr_jasp(3,3,tc), agg%clshr_jasp(2,1,tc),agg%clshr_jasp(2,2,tc) ,agg%clshr_jasp(2,3,tc), &
            agg%hk_jasp(3,1,tc),agg%hk_jasp(3,2,tc) ,agg%hk_jasp(3,3,tc), agg%hk_jasp(2,1,tc),agg%hk_jasp(2,2,tc) ,agg%hk_jasp(2,3,tc), & 
            agg%hk_jasp(1,1,tc),agg%hk_jasp(1,2,tc) ,agg%hk_jasp(1,3,tc), agg%wage_inc_s(1,tc), agg%wage_inc_s(2,tc),agg%wage_inc_s(3,tc), & 
            agg%fe_s(1,tc), agg%fe_s(2,tc),agg%fe_s(3,tc), agg%h_j(jt,tc),agg%ivt_tot(tc),agg%cons(tc),sum(grid%Phi(:,:,:,:,:,:,:,:,:,4,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,3,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,2,:,:,js,tc)), sum(grid%Phi(:,:,:,:,:,:,:,:,:,1,:,:,js,tc))
                    
        write(992,'(15f50.16)') agg%hk_tot(tc), agg%phi_s_tot(1,tc), agg%phi_s_tot(2,tc), agg%phi_s_tot(3,tc), agg%phi_s_tot(4,tc), agg%moninv(tc), agg%tinv(tc), agg%tot_inc(tc), &
                            agg%wage_inc(tc), agg%labinctaxrv(tc)/agg%wage_inc(tc),agg%avtax(tc),agg%avtax_av(tc), agg%gcons(tc), agg%igov(tc), agg%clsubs(tc)                
        
    end do
    ! close files 
    close(99)
    close(991)
    close(992)
        
    end subroutine sub_solve_olg_debt
    ! ---------------------------------------------------------------------------
   
    ! ---------------------------------------------------------------------------    
    subroutine sub_calib_olg(fg,xg,ng)
    ! function called from solver for solution of olg model
    implicit none  
    integer,intent(in)::ng
    real(dp),intent(in)::xg(ng)
    real(dp),intent(out)::fg(ng)
    
    real(dp),allocatable::xg_new(:)
    logical,parameter::opt_check=.false.

    ! get values for x-variables
    if (t1==t0) then    ! steady state
        call sub_getx_param(xg,agg,t0,t1,ngs,sfagg,1,ind_param)
    else                ! transition
        call sub_getx_param(xg,agg,t0,t1,ngs+atr,sfagg,2,ind_param)
    endif
    
    !print*, agg%fh0_no(tclb), sfagg
    !pause
    
    ! solution for OLG equilibrium, given settings
    call sub_olg_equil(stat,agg,demo,grid,lc,pol,t0,t1)
    
    ! make values of x-variables:
    if (t1==t0) then    ! steady state
        call sub_makex_param(xg_new,agg,t0,t1,ngs,sfagg,1,ind_param)
    else                ! transition
        call sub_makex_param(xg_new,agg,t0,t1,ngs+atr,sfagg,2,ind_param)
    endif
   
    if (ind_param==0)then
        
        !fg(1)=agg%gconsr(tclb) - agg%gyr_str(tclb)
        fg(1)=agg%byr(tclb) - agg%byr_str(tclb)
    
    elseif (ind_param==1)then
        
        fg(1)=xg(1)-xg_new(1) 
    
    elseif  (ind_param==2)then
        ! ivt transfers to assets
        fg(1)=agg%ivty(tclb) - agg%ivty_str(tclb)
    elseif (ind_param==3)then
        ! normalization of acuqired HK
        fg(1)=agg%h_j(jt,tclb) -hk_str! agg%hk_str(tclb)   
    elseif (ind_param==4)then
        ! dropout RATE
        fg(1)=agg%dropfrac(tclb)/agg%clfrac(tclb) - agg%dropfrac_str(tclb)/agg%clfrac_str(tclb)   
           
    elseif (ind_param==5)then
        ! college completion fraction / gradudate fraction
        fg(1)=agg%clfrac(tclb) - agg%clfrac_str(tclb)    
    !elseif (ind_param==6)then    
    !    fg(1)=agg%t_j(jf,tclb) - agg%tinv0_str(tclb) 
    elseif (ind_param==7)then 
        fg(1)=agg%hrs(tclb) - agg%hrs_str(tclb)
        
    elseif (ind_param==8)then 
        fg(1)=agg%clkid_cl(tclb) - agg%clkid_cl_str(tclb)    
    elseif (ind_param==9)then
        ! education gradient in time 
        fg(1)=agg%clcosty(tclb) - agg%clcosty_str(tclb)
    elseif (ind_param==10)then
        ! normalization of acuqired HK
        fg(1)=agg%h_j(jt+1,tclb) -agg%hk_hs_str(tclb)! agg%hk_str(tclb)    
     
    endif
    
    end subroutine sub_calib_olg
    ! ---------------------------------------------------------------------------    
    
    ! ---------------------------------------------------------------------------    
    subroutine sub_calib2_olg(fg,xg,ng)
    ! function called from solver for solution of olg model
    implicit none  
    integer,intent(in)::ng
    real(dp),intent(in)::xg(ng)
    real(dp),intent(out)::fg(ng)
    
    real(dp),allocatable::xg_new(:)
    logical,parameter::opt_check=.false.

    ! get values for x-variables
    if (t1==t0) then    ! steady state
        call sub_getx_2param(xg,agg,t0,t1,ngs,sfagg,1,ind_param_loc)
    else                ! transition
        call sub_getx_2param(xg,agg,t0,t1,ngs+atr,sfagg,2,ind_param_loc)
    endif
    
    ! solution for OLG equilibrium, given settings
    call sub_olg_equil(stat,agg,demo,grid,lc,pol,t0,t1)
    
    ! make values of x-variables:
    if (t1==t0) then    ! steady state
        call sub_makex_2param(xg_new,agg,t0,t1,ngs,sfagg,1,ind_param_loc)
    else                ! transition
        call sub_makex_2param(xg_new,agg,t0,t1,ngs+atr,sfagg,2,ind_param_loc)
    endif
   
    
    if  (ind_param_loc==1)then
        
        
        ! fraction of CL
        fg(1)=agg%clfrac(tclb) - agg%clfrac_str(tclb) 
        
        ! fraction of HS
        fg(2)=agg%lhsfrac(tclb) - agg%lhsfrac_str(tclb)    
        
        
        !print*, agg%hsfrac(tclb), agg%hsfrac_str(tclb) ,agg%clfrac(tclb) , agg%clfrac_str(tclb)
        !pause
    elseif (ind_param_loc==2)then
        
        fg(1)=agg%betta1(tclb) - agg%betta1_str(tclb) 
        
        fg(2)=agg%betta2(tclb) - agg%betta2_str(tclb) 
        
        fg(3)=agg%metta1(tclb) - agg%metta1_str(tclb) 
        
        fg(4)=agg%metta2(tclb) - agg%metta2_str(tclb)
        
          
        
        
    elseif (ind_param_loc==3)then
        
        fg(1)=agg%betta1(tclb) - agg%betta1_str(tclb) 
        
        fg(2)=agg%betta2(tclb) - agg%betta2_str(tclb)   
        
    elseif (ind_param_loc==4)then
        
        fg(1:2)=xg(1:2)-xg_new(1:2)       
        
    elseif (ind_param_loc==5)then
        
        fg(1:3)=agg%meanwage_s(1:3,tclb) - agg%meanwage_s_str(:,tclb) !1.0_dp
        
    elseif (ind_param_loc==6)then
        
        fg(1)=agg%ky(tclb) - agg%ky_str(tclb)     
        
        
        fg(2)=agg%ivty(tclb) - agg%ivty_str(tclb)
        
        
        
    endif
    
    end subroutine sub_calib2_olg 
    ! ---------------------------------------------------------------------------    

    ! ---------------------------------------------------------------------------    
    subroutine sub_calib5_olg(fg,xg,ng)
    ! function called from solver for solution of olg model
    implicit none  
    integer,intent(in)::ng
    real(dp),intent(in)::xg(ng)
    real(dp),intent(out)::fg(ng)
    
    real(dp),allocatable::xg_new(:)
    logical,parameter::opt_check=.false.

    ! get values for x-variables
    if (t1==t0) then    ! steady state
        call sub_getx_5param(xg,agg,t0,t1,ngs,sfagg,1,ind_param_loc)
    else                ! transition
        call sub_getx_5param(xg,agg,t0,t1,ngs+atr,sfagg,2,ind_param_loc)
    endif
    
    ! solution for OLG equilibrium, given settings
    call sub_olg_equil(stat,agg,demo,grid,lc,pol,t0,t1)
    
    ! make values of x-variables:
    if (t1==t0) then    ! steady state
        call sub_makex_5param(xg_new,agg,t0,t1,ngs,sfagg,1,ind_param_loc)
    else                ! transition
        call sub_makex_5param(xg_new,agg,t0,t1,ngs+atr,sfagg,2,ind_param_loc)
    endif
   
    
    if  (ind_param_loc==1)then
        
        ! per period mon inv as a share of reference earnings
        fg(1)=agg%moninv(tclb) - agg%moninv_str(tclb)
        
        ! time profile intercept 
        fg(2)=agg%tinv(tclb) - agg%tinv_str(tclb)
            
        ! slope coefficient
       ! fg(3)=agg%betta1(tclb) - agg%betta1_str(tclb)
        
       ! fg(4)=agg%betta2(tclb) - agg%betta2_str(tclb)
        
        fg(3)=agg%h_j(jt,tclb) -hk_str
        
        fg(4)=agg%t_j(jf,tclb) - agg%tinv0_str(tclb)
        
        !fg(6)=agg%metta1(tclb) - agg%metta1_str(tclb)
        !
        !fg(7)=agg%metta2(tclb) - agg%metta2_str(tclb)
        
    elseif (ind_param_loc==2)then
        
        fg(1:3)=agg%meanwage_s(1:3,tclb) - agg%meanwage_s_str(:,tclb) 
        
         ! fraction of CL
        fg(4)=agg%clfrac(tclb) - agg%clfrac_str(tclb) 
        
        ! fraction of dropouts
        fg(5)=agg%dropfrac(tclb)/agg%clfrac(tclb) - agg%dropfrac_str(tclb)/agg%clfrac_str(tclb)   
        
        
    endif
    
    end subroutine sub_calib5_olg
    ! ---------------------------------------------------------------------------    
    

end subroutine sub_olg
! ---------------------------------------------------------------------------    


! ---------------------------------------------------------------------------    
subroutine sub_olg_equil(stat,agg,demo,grid,lc,pol,t0,t1)
! solution: household model and aggregation 
implicit none

type(t_stat),intent(inout)::stat
type(t_agg),intent(inout)::agg 
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_lc),intent(inout)::lc
type(t_pol),intent(inout)::pol
integer,intent(in)::t0,t1
integer:: itm
real(dp):: dist_m, dist_m_vec(ny,1,ni,2,nk,ns),dist_m_vec_tr(ny,1,ni,2,nk,ns,nt)
real(dp),parameter::epsi=1.0e-05
real(dp):: pop_t(nt) ! population receiving transfers from acc bequests
integer:: tc,sc,kc,yc,ec,dc,xc,gc,ic,jc,itp,ii
real(dp):: Phi_new(nd,nx,np,ny,ne,ni,2,nk,ns),Phi_new_tr(nd,nx,np,ny,ne,ni,2,nk,ns,nt)
real(dp),parameter:: df=0.3_dp
real(dp),allocatable:: xold(:),xnew(:)
real(dp):: Phi2new(nd,nx,ny,nk,ng,nw,ns,nt),dist,PhiT(nd,nx,ny,ng,nk,nw,ns),Phi_max(nd,ny,nk,nw,ns), & 
    sav_vec(nd,nx,ny,ng,nk,nw,ns)
integer:: it
integer,parameter:: maxit_ss = 3, maxit_tr = 1
integer:: maxit



if (t0==t1)then ! ss => fixed point iter in distr
    
    maxit = maxit_ss
    
    ! guess for Phi_jf-1: initialized it at the beginning of the module
    grid%Phi2guess(:,:,:,:,:,:,:,t0:t1) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
    do gc=1,2
        do sc=1,ns
            grid%Phi2guess_s(:,:,:,:,gc,:,sc,t0:t1) = grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1) / sum(grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1))
        enddo
    enddo
    
    grid%sav2guess(:,:,:,:,:,:,:,t0:t1) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
    
    it = 0
     
    do while (it<maxit) 
        opt_ge_upd = 0
        opt_pe=1
        
        !do tc = t0,t1
        !    ! find indices of positive entries in Phi2guess
        !
        !    PhiT = reshape(grid%Phi2guess(:,:,:,:,:,:,:,tc),shape(PhiT)) 
        !
        !    where (PhiT(:,:,:,:,:,:,:)>0.0_dp) !grid%sav(1,xc2,1,ycp1_p,1,wcp1_p,gc_p,kc_p,1,sc_p,1,1,jf-2,tc)
        !        sav_vec(:,:,:,:,:,:,:) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,tc)
        !    
        !    end where
        !
        !    do gc = 1,ng
        !    
        !        sav_min(gc,tc) = minval(sav_vec(:,:,:,gc,:,:,:),PhiT(:,:,:,gc,:,:,:)>0)
        !        sav_max(gc,tc) = maxval(sav_vec(:,:,:,gc,:,:,:),PhiT(:,:,:,gc,:,:,:)>0)
        !   
        !    enddo
        !enddo
       
        call sub_solvehh_main(agg,demo,grid,pol,t0,t1,2)

        call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) ! here upon fpp interg. convergence, store Phi2 and sav2
    
        !update guessgrid%Phi(1,inds(2),1,yc,1,kc,gc,wc,1,sc,1,1,jf-2,tc)
        Phi2new(:,:,:,:,:,:,:,t0:t1) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1) !grid%Phi2(:,:,:,:,jf-2)
        dist = maxval(abs(Phi2new(:,:,:,:,:,:,:,t0:t1) - grid%Phi2guess(:,:,:,:,:,:,:,t0:t1)) )
        if (dist<epsi)then
            print*, "converged phi2" , it
            exit
        else
            grid%Phi2guess(:,:,:,:,:,:,:,t0:t1) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
            do gc=1,2
                do sc=1,ns
                    grid%Phi2guess_s(:,:,:,:,gc,:,sc,t0:t1) = grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1) / sum(grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1))
                enddo
            enddo
            
            grid%sav2guess(:,:,:,:,:,:,:,t0:t1) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
            it = it + 1
            print*, "dist mar phi2", dist, it
        endif
    enddo
    
    !opt_ge_upd = 1
    !if (t0==1)then
        opt_pe = opt_pe_ind
    !else
    !    opt_pe = opt_pe_in
    !endif

    if (opt_pe==0) opt_ge_upd = 1
    
    call sub_solvehh_main(agg,demo,grid,pol,t0,t1,2)

    call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) ! here upon fpp interg. convergence, store Phi2 and sav2
    

    
else ! transition
    
    maxit = maxit_tr
   
    ! guess for Phi_jf-1: initialized it at the beginning of the module
    grid%Phi2guess(:,:,:,:,:,:,:,t0:t1) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
    do gc=1,2
        do sc=1,ns
            grid%Phi2guess_s(:,:,:,:,gc,:,sc,t0:t1) = grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1) / sum(grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1))
        enddo
    enddo
    
    grid%sav2guess(:,:,:,:,:,:,:,t0:t1) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
    
    it = 0
     
    do while (it<maxit)
        opt_ge_upd = 0
        opt_pe=1
        
        do tc = t0,t1
            ! find indices of positive entries in Phi2guess
        
            PhiT = reshape(grid%Phi2guess(:,:,:,:,:,:,:,tc),shape(PhiT)) 
        
            where (PhiT(:,:,:,:,:,:,:)>0.0_dp) !grid%sav(1,xc2,1,ycp1_p,1,wcp1_p,gc_p,kc_p,1,sc_p,1,1,jf-2,tc)
                sav_vec(:,:,:,:,:,:,:) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,tc)
            
            end where
     
            do gc = 1,ng
            
                sav_min(gc,tc) = minval(sav_vec(:,:,:,gc,:,:,:),PhiT(:,:,:,gc,:,:,:)>0)
                sav_max(gc,tc) = maxval(sav_vec(:,:,:,gc,:,:,:),PhiT(:,:,:,gc,:,:,:)>0)
           
            enddo
        enddo
        
        call sub_solvehh_main(agg,demo,grid,pol,t0,t1,2)

        call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0) ! here upon fpp interg. convergence, store Phi2 and sav2
    
        !update guessgrid%Phi(1,inds(2),1,yc,1,kc,gc,wc,1,sc,1,1,jf-2,tc)
        Phi2new(:,:,:,:,:,:,:,t0:t1) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1) !grid%Phi2(:,:,:,:,jf-2)
        grid%sav2guess(:,:,:,:,:,:,:,t0:t1) = grid%sav(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
        dist = maxval(abs(Phi2new(:,:,:,:,:,:,:,t0:t1) - grid%Phi2guess(:,:,:,:,:,:,:,t0:t1)) )
        if (dist<epsi)then
            print*, "converged phi2" , it
            exit
        else
            grid%Phi2guess(:,:,:,:,:,:,:,t0:t1) = grid%Phi(:,:,1,:,1,:,:,:,1,:,1,1,jf-2,t0:t1)
            do gc=1,2
                do sc=1,ns
                    grid%Phi2guess_s(:,:,:,:,gc,:,sc,t0:t1) = grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1) / sum(grid%Phi(:,:,1,:,1,:,gc,:,1,sc,1,1,jf-2,t0:t1))
                enddo
            enddo
            
            
            it = it + 1
            print*, "dist mar phi2", dist, it
        endif
    enddo
    
    !opt_ge_upd = 1
    opt_pe = opt_pe_ind
    
    if (opt_pe==0) opt_ge_upd = 1

    ! solution of household model: 
    call sub_solvehh_main(agg,demo,grid,pol,t0,t1,2)

   
    call sub_meas_call(stat,agg,demo,grid,lc,pol,t0,t1,0)
    
    if (opt_debt==1)then
        call sub_eval_debt(agg%psi_lambda(2),agg%debt(2),agg,demo,grid,pol,stat,lc,t0,t1)
        print*, "in olg_eq, debt2", agg%debt(2)
    endif

endif
  
end subroutine sub_olg_equil 
! ---------------------------------------------------------------------------  

! ---------------------------------------------------------------------------    
subroutine sub_inoutloop_trans(agg)
! input of starting values for outer loop variables
use esplot
implicit none

type(t_agg),intent(inout)::agg
real(dp),dimension(12,nt)::dat_outloop
real(dp),dimension(39,nt)::aggvars
integer::sc,ic,oc,zc,tc,vc
character(1)::i_char_sc,i_char_ic
logical,parameter::opt_check=.false.

open(unit=99, file='input/outerloop/outputouterloopT.txt', status='OLD', action='READ')
read(99,*) dat_outloop
close(99)

open(unit=98, file='output/aggvarsT.txt', status='OLD', action='READ')
read(98,*) aggvars
close(98)

do vc = 1,size(dat_outloop,1)
    dat_outloop(vc,nt) = dat_outloop(vc,nt-1)
enddo

do vc = 1,size(aggvars,1)
    aggvars(vc,nt) = aggvars(vc,nt-1)
enddo

  
agg%debt(2:nt) = aggvars(9,2:nt)

oc = 1
    
if (opt_pe_in==0)then
    ! interest rate is fixed
    agg%ret(2:nt)=dat_outloop(oc,2:nt) !0.0816_dp !0.0858_dp !dat_outloop_ss(1,1) 
endif    
oc=oc+1
   
! wages are fixed, set them all to normalizattion level. Productivity profiles are read in as normalized, no further adjustments needed. 
agg%wage(2:nt) = dat_outloop(oc,2:nt)
oc=oc+1
do sc=2,ns      
    if (opt_pe_in==0)then
        agg%wage_s(sc,2:nt)= dat_outloop(oc,2:nt)
        if (sc==2) agg%wage_s(1,2:nt) = agg%wage_s(sc,2:nt)
    endif
    
    oc=oc+1
enddo
  
! pension repl rate
agg%rho_p(2:nt)= dat_outloop(oc,2:nt)
oc=oc+1



! not relevant now, for accid bequests
agg%bqr(2:nt)=dat_outloop(oc,2:nt)
oc=oc+1

! average earnings
agg%avearn(2:nt)= dat_outloop(oc,2:nt)*lvl_ref
oc=oc+1

! average wage
agg%avwage(2:nt)= dat_outloop(oc,2:nt)*lvl_ref
oc=oc+1
   
! for level param of tax function
agg%psi_lambda(2:nt) =dat_outloop(oc,2:nt)
oc=oc+1

! cons tax
agg%tau_c(2:nt) =dat_outloop(oc,2:nt)
oc=oc+1

! pens contr rate
agg%tau_p(2:nt) =dat_outloop(oc,2:nt)
oc=oc+1

end subroutine sub_inoutloop_trans
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------    
subroutine sub_inoutloop_trans_cur(agg)
! input of starting values for outer loop variables
use esplot
implicit none

type(t_agg),intent(inout)::agg 
real(dp),dimension(11,nt)::dat_outloop
integer::sc,ic,oc,zc,tc
character(1)::i_char_sc,i_char_ic
logical,parameter::opt_check=.false.

open(unit=99, file='input/outerloop/outputouterloopT.txt', status='OLD', action='READ')
read(99,*) dat_outloop
close(99)
  
oc = 1
    
! interest rate is fixed
agg%ret(2:2)=dat_outloop(oc,2:2) !0.0816_dp !0.0858_dp !dat_outloop_ss(1,1) 
    
oc=oc+1
   
! wages are fixed, set them all to normalizattion level. Productivity profiles are read in as normalized, no further adjustments needed. 
do sc=1,ns            
    agg%wage_s(sc,2:2)= dat_outloop(oc,2:2)
    oc=oc+1
enddo
  
! pension repl rate
agg%rho_p(2:2)= dat_outloop(oc,2:2)
oc=oc+1

! not relevant now, for accid bequests
agg%bqr(2:2)=dat_outloop(oc,2:2)
oc=oc+1

! average earnings
agg%avearn(2:2)= dat_outloop(oc,2:2)*lvl_ref
oc=oc+1

! average wage
agg%avwage(2:2)= dat_outloop(oc,2:2)*lvl_ref
oc=oc+1
   
! for level param of tax function
agg%psi_lambda(2:2) =dat_outloop(oc,2:2)
oc=oc+1

! cons tax
agg%tau_c(2:2) =dat_outloop(oc,2:2)
oc=oc+1

! pens contr rate
agg%tau_p(2:2) =dat_outloop(oc,2:2)
oc=oc+1

end subroutine sub_inoutloop_trans_cur
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------    
subroutine sub_inoutloop_run(agg,opt_clb)
! input of starting values for outer loop variables
use esplot
implicit none

type(t_agg),intent(inout)::agg
integer,intent(in):: opt_clb
real(dp),dimension(45,nt)::dat_outloop
real(dp),dimension(12,1)::dat_outloop_ss
real(dp):: sfac_dat(1,31+atr),params_dat(29,1) ! params_dat(9,1)
integer::sc,ic,oc,zc,tc
character(1)::i_char_sc,i_char_ic
logical,parameter::opt_check=.false.

if (opt_trref==0)then
    open(unit=99, file='input/outerloop/outputSSouterloop.txt', status='OLD', action='READ')
    read(99,*) dat_outloop_ss
    close(99)
    
    open(unit=96, file='input/outerloop/params1.txt', status='OLD', action='READ')
    read(96,*) params_dat
    close(96)
else
    open(unit=99, file='input/outerloop/outputSSouterloopREF.txt', status='OLD', action='READ')
    read(99,*) dat_outloop_ss
    close(99)
    
    open(unit=96, file='input/outerloop/params1REF.txt', status='OLD', action='READ')
    read(96,*) params_dat
    close(96)
endif 

    
    
    
if (opt_clb==1)then ! SS calib

    agg%meanwage_s(:,:) = 1.0_dp
    
    oc = 1
    
    ! interest rate is fixed
    agg%ret(:)=dat_outloop_ss(oc,1) !0.0816_dp !0.0858_dp !dat_outloop_ss(1,1) 
    
    oc=oc+1
   
    ! wages are fixed, set them all to normalizattion level. Productivity profiles are read in as normalized, no further adjustments needed. 
    agg%wage(tc) = dat_outloop_ss(oc,1)
    oc = oc+1
    do sc=2,ns            
        agg%wage_s(sc,:)= dat_outloop_ss(oc,1)
        oc=oc+1
    enddo
    agg%wage_s(1,:) = agg%wage_s(2,:)
    
    !agg%wage(:) = 0.643609658803749_dp!!738551228614606_dp !1.0_dp
   
    ! pension repl rate
    agg%rho_p(:)= dat_outloop_ss(oc,1)
    oc=oc+1
    ! not relevant now, for accid bequests
    agg%bqr(:)=dat_outloop_ss(oc,1)
    oc=oc+1
    ! average earnings
    agg%avearn(:)= dat_outloop_ss(oc,1)*lvl_ref
    oc=oc+1
    ! aveerage wage
    agg%avwage(:)= dat_outloop_ss(oc,1)*lvl_ref
    oc=oc+1
    
    oc=oc+1
    oc=oc+1
    
    ! pension contr rate
    agg%tau_p(:) = dat_outloop_ss(oc,1)
   
    adj_fac_tr = params_dat(1,1)
    oc=2
 
    oc=oc+1
    ! for level param of tax function
    agg%psi_lambda(:) =params_dat(3,1) !* 0.2_dp !* 1.0_dp
    oc=oc+1
    ! weights on labor disutiltiy for single and married, keep them equal
    agg%phi(:) =params_dat(oc,1) !0.65_dp 
    oc=oc+1
    !agg%phi_mal(2,:) = params_dat(oc,1) !0.65_dp
    !agg%phi_mal(1,:) = 1.0_dp
    oc=oc+1
   
    ! beta
    agg%beta(:) =params_dat(oc,1)
    oc=oc+1
 
    ! nu
    agg%nu(:) =params_dat(oc,1) 
    oc=oc+1
    
    ! norm param in HK production
    agg%abar(:) =params_dat(oc,1) ! 18.5_dp 
    oc=oc+1
    
    ! weight on HK in outer CES
    agg%kappah(:) =params_dat(oc,1)! 0.5_dp 
    oc=oc+1
    
    ! intercept kappa0
    agg%kappat(:) =params_dat(oc,1)  !0.53_dp !25_dp !params_dat(oc,1)
    oc=oc+1
    
    ! slope kappai1
    agg%kappai1(:) =params_dat(oc,1) !  0.1_dp !1_dp 
    oc=oc+1
    
    agg%kappai2(:) =params_dat(oc,1) !  0.1_dp !1_dp 
    oc=oc+1
    
    ! college utility cost / taste 
    agg%zeta_cl(1,:)= params_dat(oc,1)  !0.73_dp  
    oc=oc+1
    agg%zeta_cl(2,:)= params_dat(oc,1) !*agg%zeta_cl(1,:) !agg%zeta_cl(1,:)
    oc=oc+1
    
    ! s.e. time and money
    agg%fi1(:) =params_dat(oc,1) ! 0.6_dp
    oc=oc+1
    
    ! normalization h0
    agg%h0norm(:) = params_dat(oc,1)
    oc=oc+1
    agg%omega_fin(:) = params_dat(oc,1) 
    oc=oc+1
    
    agg%fh0_no(:) = log(0.9_dp) 
    agg%fh0_cl(:) = log(1.05_dp) 
    
    agg%kappah1(:) =params_dat(oc,1) !params_dat(oc,1) !  0.1_dp !1_dp 
    oc=oc+1
    
    agg%kappah2(:) =params_dat(oc,1) !params_dat(oc,1) !  0.1_dp !1_dp 
    oc=oc+1
    
    agg%tau_c(:) = params_dat(oc,1) !0.3_dp * hk_str
    oc=oc+1
    agg%gamdelta(:) = params_dat(oc,1) !0.6_dp * hk_str
    oc=oc+1
    
    agg%gammah_s(1,:) = params_dat(oc,1) !- (0.5_dp * agg%abgrad_s(2,1) + 0.5_dp * agg%abgrad_s(3,1) ) * 1.5_dp
    oc=oc+1
    agg%gammah_s(2,:) = params_dat(oc,1) !- (0.5_dp * agg%abgrad_s(2,1) + 0.5_dp * agg%abgrad_s(3,1) ) * 1.5_dp
    oc=oc+1
    agg%gammah_s(3,:) =params_dat(oc,1) !0.80_dp! 1.45_dp * 1.49_dp ! params_dat(oc,1) !- (0.5_dp * agg%abgrad_s(2,2) + 0.5_dp * agg%abgrad_s(3,2) )
    oc=oc+1
    agg%gammah_s(4,:) =params_dat(oc,1) !0.73_dp! 1.45_dp * 1.61_dp ! params_dat(oc,1) ! (0.5_dp * agg%abgrad_s(2,1) + 0.5_dp * agg%abgrad_s(3,1) ) * 0.1_dp
    oc=oc+1
    !agg%gammah_s(1,:) = agg%gammah_s(2,:)
    agg%inv_mean(:) =params_dat(oc,1) !  0.877312173318740_dp !1.0_dp
    oc=oc+1
    agg%kappap4(:) = params_dat(oc,1) !kappap
    oc = oc +1 
    
    agg%psi_ls(:) = 0.20_dp 
      
    ! in baseline edu spending
    agg%edu_spend_fac(:) = 1.0_dp
    
    agg%varrho(:) = 0.388_dp
    agg%kappafee(:) = 0.30_dp  
    
    grincmed = 0.5_dp
    
    agg%iota(:) =params_dat(oc,1) !0.37_dp ! 5_dp
    oc = oc +1 
    
    agg%h0delta(:)=params_dat(oc,1) ! 0.72_dp
    
    agg%inc_thrs(:) = 0.32_dp
    
    
    
endif

  
end subroutine sub_inoutloop_run 
! ---------------------------------------------------------------------------    




! -------------------------------------------------------
function f_ng(ng,tt,ind,optcalib)
! compute parameters for GS and GSQN implementation
! ind=0: steady state
! ind=1: transition
implicit none

integer::f_ng
integer,intent(in)::tt,ind,optcalib
integer,intent(in)::ng
integer:: nprm_base

if (opt_pe==0)then
    nprm_base = 4 + ns 
elseif (opt_pe==1)then
    nprm_base = 1 !3
elseif (opt_pe==-1)then ! GE prices ONLY
    nprm_base = 4

endif

if (ind==0) then	! settings for steady state  
    if (optcalib==1 .and. opt_pe>-1)then
        f_ng=nprm_base + nprm_clb  ! ret,rhop,aw,bhlev   
        
    else
        f_ng=nprm_base
      
    endif
    
    if (opt_accbq>1 .and. opt_pe>-1) then
	    f_ng=f_ng+1
    endif
else    ! now also loop on replacement rate, thus increase ng by 1
	! TEST: do not add replacement rate
    !f_ng=(ng)*tt
     f_ng=(atr+ng)*tt
endif

end function f_ng
! -------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_makex(x,agg,t0,t1,ng,sf,ind,optcalib)
! makes x-vector for GSQN implementation
! ind==0: scaling factors (in initial steady state)
! ind==1: steady state
! ind==2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(out),allocatable::x(:)         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(inout)::sf(:)     
integer,intent(in)::ind,optcalib

real(dp),allocatable::xr(:,:)
integer::sc,ic,ac,zc
real(dp)::adj_fac

if (ind<=1) then    ! steady state
    allocate(x(ng))
    allocate(xr(1,ng))
else
    allocate(x((ng)*(t1-t0+1)))
    allocate(xr(t1-t0+1,ng))
endif

if (ind==0) then    ! build sf and x in initial steady state
    ac=1
    
    if (opt_pe<1)then
    
        sf(ac)=1.0_dp/agg%ret(t0)
        ac=ac+1
   
        do sc=1,ns
            
            sf(ac)=1.0_dp/agg%wage_s(sc,t0)
            ac=ac+1               
            
           
        enddo
   
        if (opt_pe==0)then
    
            sf(ac)=1.0_dp / agg%avwage(t0)
            ac = ac+1
    
            sf(ac)=1.0_dp / agg%avearn(t0)
            ac = ac+1
            
        endif
        
    
    endif
    
    if (opt_pe>-1)then
    
        if (opt_trref==0)then
            sf(ac)=1.0_dp/agg%rho_p(t0)
        else
            sf(ac)=1.0_dp/agg%tau_p(t0)
        endif
    
        ac = ac+1
        
    
    
        if (opt_accbq>1)then
            sf(ac)=1.0_dp/0.02_dp !agg%bqr(t0)
            ac = ac+1
        endif
    
   
    
        if (optcalib==1 )then
            !sf(ac)=1.0_dp/agg%beta(t0)
            !ac = ac+1
            if (opt_tauc==1)then
                sf(ac)=1.0_dp/agg%tau_c(t0)
                ac = ac+1
            else
            
                sf(ac)=1.0_dp/agg%psi_lambda(t0)
                ac = ac+1
            endif
        
            
        
        endif

    endif
    
    
    x(:)=1.0_dp

elseif (ind>=1) then    ! build x in final steady state and transition
    
    ! set starting values in transition:
    if (ind==3) then
        if (opt_constval==1) then   ! start with constants as starting values
            
            agg%ret(t0:t1)=agg%ret(nt)
            
            do sc=1,ns
                agg%wage_s(sc,t0:t1)=agg%wage_s(sc,nt)
            end do
            
            agg%tau_p(t0:t1)=agg%tau_p(nt)
            agg%tau_c(t0:t1)=agg%tau_c(nt)
            agg%rho_p(t0:t1)=agg%rho_p(nt)
            agg%avwage(t0:t1)=agg%avwage(nt)
            agg%avearn(t0:t1)=agg%avearn(nt)
            agg%bqr(t0:t1)=agg%bqr(nt)
            
            agg%psi_lambda(t0:t1) = agg%psi_lambda(nt) 
        
        elseif (opt_constval==-1)then    ! scale paths using new steady state information (notice that period nt-1 is already in steady state)
            adj_fac=min(1.0_dp,agg%ret(nt)/agg%ret(nt-1) )
            
            !print*, "adj fact", adj_fac
            !pause
            
            agg%ret(t0:t1)=agg%ret(t0:t1)*adj_fac   
           
            do sc=1,ns
                    
                adj_fac=agg%wage_s(sc,nt)/agg%wage_s(sc,nt-1)
                agg%wage_s(sc,t0:t1)=agg%wage_s(sc,t0:t1)*adj_fac
                  
                
                    
            end do
           
            
            adj_fac=agg%tau_p(nt)/agg%tau_p(nt-1)
            agg%tau_p(trfg:t1)=agg%tau_p(trfg:t1)*adj_fac
            
            if (opt_accbq==2)then
                adj_fac=agg%bqr(nt)/agg%bqr(nt-1)
                agg%bqr(t0:t1)=agg%bqr(t0:t1)*adj_fac
            endif
            
            adj_fac=agg%rho_p(nt)/agg%rho_p(nt-1)
            agg%rho_p(t0:t1)=agg%rho_p(t0:t1)*adj_fac
            
            adj_fac=agg%avwage(nt)/agg%avwage(nt-1)
            agg%avwage(t0:t1)=agg%avwage(t0:t1)*adj_fac
            adj_fac=agg%avearn(nt)/agg%avearn(nt-1)
            agg%avearn(t0:t1)=agg%avearn(t0:t1)*adj_fac
      
        endif
    
            
     
    endif
    
    ac=1
    
    if (opt_pe<1)then
    
        xr(:,ac)=agg%ret(t0:t1)*sf(ac)
   
        ac=ac+1
    
        do sc=1,ns
           
            xr(:,ac)=log(agg%wage_s(sc,t0:t1)*sf(ac) ) + lglv
                            
            ac=ac+1
              
        end do
        
    
        if (opt_pe==0)then
            xr(:,ac)=log(agg%avwage(t0:t1)*sf(ac) ) + lglv         
            ac=ac+1
    
            xr(:,ac)=log(agg%avearn(t0:t1)*sf(ac) ) + lglv
            ac=ac+1
    
        endif
        
        
    endif
    
    if (opt_pe>-1)then
    
        if (opt_trref==0)then
            xr(:,ac)=agg%rho_p(t0:t1)*sf(ac)
        else
            xr(:,ac)=agg%tau_p(t0:t1)*sf(ac)
        endif
    
        ac=ac+1
    
    
        if (opt_accbq>1)then
       
            xr(:,ac)=agg%bqr(t0:t1)*sf(ac)   
        
            ac=ac+1 
        endif
    
         if (optcalib==1)then
            !xr(:,ac)=agg%beta(t0:t1)*sf(ac)
            !ac=ac+1
            if (opt_tauc==1)then
                xr(:,ac)=agg%tau_c(t0:t1)*sf(ac)
                ac=ac+1
            else
            
                xr(:,ac)=agg%psi_lambda(t0:t1)*sf(ac)
                ac=ac+1
            endif
        
         endif
         
    endif
    
    
    !if (ind>=2) then
    !    xr(:,ac)=agg%tau_p(t0:t1)*sf(ac) 
    !    ac=ac+1
    !    if (opt_captax==0)then
    !        xr(:,ac)=agg%tau_c(t0:t1)*sf(ac) 
    !        ac=ac+1
    !       
    !    else
    !        xr(:,ac)=agg%tau_k(t0:t1)*sf(ac) 
    !        ac=ac+1
    !    endif
    !  
    !endif
   
    x(:)=reshape(xr(:,:),(/size(x,1)/))
endif

end subroutine sub_makex
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_getx(x,agg,t0,t1,ng,sf,ind,optcalib)
! get x-vector for GSQN implementation
! ind=1: steady state
! ind=2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(in)::x(ng*(t1-t0+1))         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(in)::sf(:)     ! length in steady state
integer,intent(in)::ind,optcalib

real(dp)::xr(t1-t0+1,ng)
integer::sc,ic,ac,zc

xr(:,:)=reshape(x(:),(/(t1-t0+1),ng/))
ac=1

if (opt_pe<1)then

    agg%ret(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    do sc=1,ns
       
        agg%wage_s(sc,t0:t1)=(exp(xr(:,ac)-lglv))/sf(ac)       
               
        ac=ac+1
    
    enddo
    
    if (opt_pe==0)then
    

        agg%avwage(t0:t1)=(exp(xr(:,ac)-lglv))/sf(ac)
        ac = ac+1

        agg%avearn(t0:t1)=(exp(xr(:,ac)-lglv))/sf(ac)
        ac=ac+1

        
    endif
    
        
endif

if (opt_pe>-1)then

    if (opt_trref==0)then
        agg%rho_p(t0:t1)=xr(:,ac)/sf(ac)
    else
        agg%tau_p(t0:t1)=xr(:,ac)/sf(ac)
    endif

    ac=ac+1


    if (opt_accbq>1)then
  
        agg%bqr(t0:t1)=xr(:,ac)/sf(ac)
    
        ac=ac+1
    endif

    if (optcalib==1)then
        !agg%beta(t0:t1)=xr(:,ac)/sf(ac)
        !ac=ac+1
        if (opt_tauc==1)then
            agg%tau_c(t0:t1)=xr(:,ac)/sf(ac) 
            !ac=ac+1
        else
        
            agg%psi_lambda(t0:t1)=xr(:,ac)/sf(ac) 
            !ac=ac+1
          !  print*, " in get x", agg%psi_lambda(t0:t1)
        endif
    

    endif
    
endif


!if (ind==2) then
!    agg%tau_p(t0:t1)=xr(:,ac)/sf(ac)
!    ac=ac+1
!    if (opt_captax==0)then
!        agg%tau_c(t0:t1)=xr(:,ac)/sf(ac)
!        ac=ac+1
!       
!    else
!        agg%tau_k(t0:t1)=xr(:,ac)/sf(ac)
!        ac=ac+1
!    endif
!
!endif

end subroutine sub_getx
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_makex_param(x,agg,t0,t1,ng,sf,ind,ind_param)
! makes x-vector for GSQN implementation
! ind==0: scaling factors (in initial steady state)
! ind==1: steady state
! ind==2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(out),allocatable::x(:)         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(inout)::sf(:)     
integer,intent(in)::ind,ind_param

real(dp),allocatable::xr(:,:)
integer::sc,ic,ac,zc
real(dp)::adj_fac

if (ind<=1) then    ! steady state
    allocate(x(ng))
    allocate(xr(1,ng))
else
    allocate(x((ng)*(t1-t0+1)))
    allocate(xr(t1-t0+1,ng))
endif


if (ind==0) then    ! build sf and x in initial steady state
    ac=1
    
    if (ind_param==0 )then
        if (opt_tauc==1)then
            sf(ac)=1.0_dp/agg%tau_c(t0)
            ac = ac+1
        else
            
            sf(ac)=1.0_dp/agg%psi_lambda(t0)
            
            ac = ac+1
        endif
        
    
    elseif (ind_param==1 )then
        sf(ac)=1.0_dp/agg%inv_mean(t0)
        ac = ac+1
    elseif (ind_param==2 )then
        sf(ac)=1.0_dp/agg%nu(t0)
        ac = ac+1
    elseif (ind_param==3 )then
        sf(ac)=1.0_dp/agg%abar(t0)
        ac = ac+1   
    elseif (ind_param==4 )then
        sf(ac)=1.0_dp !/agg%phi_fem(2,t0)
        ac = ac+1 
       
    elseif (ind_param==5 )then
        sf(ac)=1.0_dp/0.2_dp !agg%zeta_cl(1,t0)
        !sf(ac) = 1
        ac = ac+1     
    !elseif (ind_param==6 )then
    !    sf(ac)=1.0_dp/agg%h0norm(t0)
    !    ac = ac+1
    elseif (ind_param==7 )then
        sf(ac)=1.0_dp/agg%phi(t0)
        ac = ac+1  
    elseif (ind_param==8 )then
        sf(ac)=1.0_dp/0.2_dp !agg%zeta_cl(2,t0)
        !sf(ac) = 1
        ac = ac+1     
    elseif (ind_param==9 )then
        sf(ac)=1.0_dp!/agg%fi1(t0)
        ac = ac+1  
   
    elseif (ind_param==10 )then
        sf(ac)=1.0_dp/agg%gamdelta(t0)
        ac = ac+1        
    
    endif 

   
    x(:)=1.0_dp

elseif (ind>=1) then    ! build x in final steady state and transition
    
    
    ac=1
    
    if (ind_param==0)then
        if (opt_tauc==1)then
            xr(:,ac)=agg%tau_c(t0:t1)*sf(ac)
            ac=ac+1
        else
            
            xr(:,ac)=agg%psi_lambda(t0:t1)*sf(ac)
            !xr(:,ac) =log(agg%wage_s(sc,t0:t1)*sf(ac) ) + lglv
            ac=ac+1
        endif
        
            
    
     elseif (ind_param==1)then
        xr(:,ac)=agg%inv_mean(t0:t1)*sf(ac)
        ac=ac+1
     elseif (ind_param==2)then
        xr(:,ac)=agg%nu(t0:t1)*sf(ac)
        ac=ac+1 
     elseif (ind_param==3)then
        xr(:,ac)=agg%abar(t0:t1)*sf(ac)
        ac=ac+1  
     elseif (ind_param==4)then
        xr(:,ac)=agg%omega_fin(t0:t1)*sf(ac)
        ac=ac+1
     
     elseif (ind_param==5)then
        xr(:,ac)=agg%zeta_cl(1,t0:t1)*sf(ac) 
        ac=ac+1      
     !elseif (ind_param==6)then
!        xr(:,ac)=agg%h0norm(t0:t1)*sf(ac)
!        ac=ac+1   
     elseif (ind_param==7)then
        xr(:,ac)=agg%phi(t0:t1)*sf(ac)
        ac=ac+1 
     elseif (ind_param==8)then
        xr(:,ac)=agg%zeta_cl(2,t0:t1)*sf(ac)
        ac=ac+1   
     elseif (ind_param==9)then
        xr(:,ac)=agg%iota(t0:t1)*sf(ac)
        ac=ac+1    
    elseif (ind_param==10)then
        xr(:,ac)=agg%gamdelta(t0:t1)*sf(ac)
        ac=ac+1 
     
    endif
    
    x(:)=reshape(xr(:,:),(/size(x,1)/))
endif

end subroutine sub_makex_param
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_getx_param(x,agg,t0,t1,ng,sf,ind,ind_param)
! get x-vector for GSQN implementation
! ind=1: steady state
! ind=2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(in)::x(ng*(t1-t0+1))         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(in)::sf(:)     ! length in steady state
integer,intent(in)::ind,ind_param

real(dp)::xr(t1-t0+1,ng)
integer::sc,ic,ac,zc

xr(:,:)=reshape(x(:),(/(t1-t0+1),ng/))
ac=1

if (ind_param==0)then
    if (opt_tauc==1)then
        agg%tau_c(t0:t1)=xr(:,ac)/sf(ac)
        ac=ac+1
    else
        
        agg%psi_lambda(t0:t1)=xr(:,ac)/sf(ac)
        !agg%psi_lambda(t0:t1)=(exp(xr(:,ac)-lglv))/sf(ac)
        ac=ac+1

    endif
    
elseif (ind_param==1)then
    agg%inv_mean(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
elseif (ind_param==2)then
    agg%nu(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
elseif (ind_param==3)then
    agg%abar(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
elseif (ind_param==4)then
    agg%omega_fin(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1 
    
    
elseif (ind_param==5)then
    agg%zeta_cl(1,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1     
!elseif (ind_param==6)then
!    agg%h0norm(t0:t1)=xr(:,ac)/sf(ac)
!    ac=ac+1  
elseif (ind_param==7)then
    agg%phi(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
elseif (ind_param==8)then
    agg%zeta_cl(2,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1    
elseif (ind_param==9)then
    agg%iota(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1    
elseif (ind_param==10)then
    agg%gamdelta(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1    
  
   
endif

 
end subroutine sub_getx_param
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_makex_2param(x,agg,t0,t1,ng,sf,ind,ind_param)
! makes x-vector for GSQN implementation
! ind==0: scaling factors (in initial steady state)
! ind==1: steady state
! ind==2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(out),allocatable::x(:)         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(inout)::sf(:)     
integer,intent(in)::ind,ind_param

real(dp),allocatable::xr(:,:)
integer::sc,ic,ac,zc
real(dp)::adj_fac

if (ind<=1) then    ! steady state
    allocate(x(ng))
    allocate(xr(1,ng))
else
    allocate(x((ng)*(t1-t0+1)))
    allocate(xr(t1-t0+1,ng))
endif


if (ind==0) then    ! build sf and x in initial steady state
    ac=1
    
    if (ind_param==1 )then
       
             
        sf(ac)=1.0_dp/agg%zeta_cl(1,t0)
        ac = ac+1
        
        sf(ac)=1.0_dp!/agg%phi_fem(2,t0)
        ac = ac+1
        
    elseif (ind_param==2)then
        
        sf(ac)=1.0_dp !/agg%kappah1(t0)
        ac = ac+1
        
        sf(ac)=1.0_dp !/agg%kappah2(t0)
        ac = ac+1
        
        sf(ac)=1.0_dp !/agg%kappai1(t0)
        ac = ac+1
        
        sf(ac)=1.0_dp !/agg%kappai2(t0)
        ac = ac+1
        
    elseif (ind_param==3)then
        
        sf(ac)=1.0_dp !/agg%kappai1(t0)
        ac = ac+1
        
        sf(ac)=1.0_dp !/agg%kappai2(t0)
        ac = ac+1
        
    elseif (ind_param==4)then
        
        sf(ac)=1.0_dp/agg%hk_cutoff(1,t0)
        ac = ac+1
         
        sf(ac)=1.0_dp/agg%hk_cutoff(2,t0)
        ac = ac+1  
        
    elseif (ind_param==5)then
        
        !sf(ac)=1.0_dp/agg%gammah_s(1,t0)
        !if (sf(ac) > 900.0_dp ) sf(ac) = 1.0_dp
        !ac = ac+1
        !
        !sf(ac)=1.0_dp/agg%gammah_s(2,t0)
        !if (sf(ac) > 900.0_dp ) sf(ac) = 1.0_dp
        !ac = ac+1 
        !
        !sf(ac)=1.0_dp/agg%gammah_s(3,t0)
        !if (sf(ac) > 900.0_dp ) sf(ac) = 1.0_dp
        !ac = ac+1
        sf = 1.0_dp 
        
    elseif (ind_param==6)then
        
        sf(ac)=1.0_dp/agg%beta(t0)
        ac = ac+1
         
        sf(ac)=1.0_dp/agg%nu(t0)
        ac = ac+1  
        
    endif
   
    x(:)=1.0_dp

elseif (ind>=1) then    ! build x in final steady state and transition
    
    
    ac=1
    
     if (ind_param==1)then
        
        xr(:,ac)=agg%zeta_cl(1,t0:t1)*sf(ac)
        ac=ac+1  
          
        xr(:,ac)=agg%phi(t0:t1)*sf(ac)
        ac=ac+1
     
     elseif (ind_param==2)then
         
        xr(:,ac)=agg%kappah1(t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%kappah2(t0:t1)*sf(ac)
        ac=ac+1
        
        xr(:,ac)=agg%kappai1(t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%kappai2(t0:t1)*sf(ac)
        ac=ac+1 
        
     elseif (ind_param==3)then
         
        xr(:,ac)=agg%kappah1(t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%kappah2(t0:t1)*sf(ac)
        ac=ac+1 
     elseif (ind_param==4)then
         
        xr(:,ac)=agg%hk_cutoff(1,t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%hk_cutoff(2,t0:t1)*sf(ac)
        ac=ac+1      
        
     elseif (ind_param==5)then
         
        xr(:,ac)=agg%gammah_s(1,t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%gammah_s(2,t0:t1)*sf(ac)
        ac=ac+1 
        
        xr(:,ac)=agg%gammah_s(3,t0:t1)*sf(ac)
        ac=ac+1 
        
         !print*, "makes", sf, xr(:,:),agg%hk_cutoff(1,t0:t1)
       
    elseif (ind_param==6)then
         
        xr(:,ac)=agg%beta(t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%nu(t0:t1)*sf(ac)
        ac=ac+1 
        
        print*, "ind6 makex",  xr(tclb,:)
    endif
    
    x(:)=reshape(xr(:,:),(/size(x,1)/))
endif

end subroutine sub_makex_2param
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_getx_2param(x,agg,t0,t1,ng,sf,ind,ind_param)
! get x-vector for GSQN implementation
! ind=1: steady state
! ind=2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(in)::x(ng*(t1-t0+1))         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(in)::sf(:)     ! length in steady state
integer,intent(in)::ind,ind_param

real(dp)::xr(t1-t0+1,ng)
integer::sc,ic,ac,zc

xr(:,:)=reshape(x(:),(/(t1-t0+1),ng/))
ac=1


if (ind_param==1)then
    
    
    agg%zeta_cl(1,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%phi(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
elseif (ind_param==2)then
    
    agg%kappah1(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%kappah2(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
    agg%kappai1(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%kappai2(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
elseif (ind_param==3)then
    
    agg%kappah1(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%kappah2(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1  
elseif (ind_param==4)then
    
    agg%hk_cutoff(1,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%hk_cutoff(2,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1     
    
elseif (ind_param==5)then
    
    agg%gammah_s(1,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%gammah_s(2,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1 
    
    agg%gammah_s(3,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
elseif (ind_param==6)then
    
    agg%beta(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%nu(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1     
    
    print*, "getx ind6", xr(tclb,1)/sf(1), xr(tclb,2)/sf(2), xr(tclb,1), xr(tclb,2), sf(1), sf(2)
   
endif


end subroutine sub_getx_2param
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
subroutine sub_makex_5param(x,agg,t0,t1,ng,sf,ind,ind_param)
! makes x-vector for GSQN implementation
! ind==0: scaling factors (in initial steady state)
! ind==1: steady state
! ind==2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(out),allocatable::x(:)         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(inout)::sf(:)     
integer,intent(in)::ind,ind_param

real(dp),allocatable::xr(:,:)
integer::sc,ic,ac,zc
real(dp)::adj_fac

if (ind<=1) then    ! steady state
    allocate(x(ng))
    allocate(xr(1,ng))
else
    allocate(x((ng)*(t1-t0+1)))
    allocate(xr(t1-t0+1,ng))
endif


if (ind==0) then    ! build sf and x in initial steady state
    ac=1
    
    if (ind_param==1 )then
       
        sf(ac)=1.0_dp/agg%kappah(t0)
        ac = ac+1
      
        sf(ac)=1.0_dp/agg%kappat(t0)
        ac = ac+1
        
        !sf(ac)=1.0_dp/agg%kappai1(t0)
        !ac = ac+1
        !
        !sf(ac)=1.0_dp/agg%kappai2(t0)
        !ac = ac+1
        
        sf(ac)=1.0_dp/agg%abar(t0)
        ac = ac+1
        
        sf(ac)=1.0_dp/agg%kappap4(t0)
        ac = ac+1
        
        !sf(ac)=1.0_dp/agg%kappah1(t0)
        !ac = ac+1
        !
        !sf(ac)=1.0_dp/agg%kappah2(t0)
        !ac = ac+1
    
        
    elseif (ind_param==2)then
        
        sf(1:3) = 1.0_dp
        ac = 3+1
        
        sf(ac)=1.0_dp/0.2_dp !agg%zeta_cl(1,t0)
        ac = ac+1
        
        sf(ac)=1.0_dp !/agg%phi_fem(2,t0)
        ac = ac+1
        
    endif
   
    x(:)=1.0_dp

elseif (ind>=1) then    ! build x in final steady state and transition
    
    
    ac=1
    
     if (ind_param==1)then
        xr(:,ac)=agg%kappah(t0:t1)*sf(ac)
        ac=ac+1
     
        xr(:,ac)=agg%kappat(t0:t1)*sf(ac)
        ac=ac+1  
        
        !xr(:,ac)=agg%kappai1(t0:t1)*sf(ac)
        !ac=ac+1  
        !
        !xr(:,ac)=agg%kappai2(t0:t1)*sf(ac)
        !ac=ac+1 
        
        xr(:,ac)=agg%abar(t0:t1)*sf(ac)
        ac=ac+1
        
        xr(:,ac)=agg%kappap4(t0:t1)*sf(ac)
        ac=ac+1
        
        !xr(:,ac)=agg%kappah1(t0:t1)*sf(ac)
        !ac=ac+1  
        !
        !xr(:,ac)=agg%kappah2(t0:t1)*sf(ac)
        !ac=ac+1 
        
     elseif (ind_param==2)then
         
        xr(:,ac)=agg%gammah_s(1,t0:t1)*sf(ac)
        ac=ac+1  
         
        xr(:,ac)=agg%gammah_s(2,t0:t1)*sf(ac)
        ac=ac+1 
        
        xr(:,ac)=agg%gammah_s(3,t0:t1)*sf(ac)
        ac=ac+1 
        
        xr(:,ac)=agg%zeta_cl(1,t0:t1)*sf(ac)
        ac=ac+1  
          
        xr(:,ac)=agg%omega_fin(t0:t1)*sf(ac)
        ac=ac+1
        
    
    endif
    
    x(:)=reshape(xr(:,:),(/size(x,1)/))
endif

end subroutine sub_makex_5param
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
subroutine sub_getx_5param(x,agg,t0,t1,ng,sf,ind,ind_param)
! get x-vector for GSQN implementation
! ind=1: steady state
! ind=2: transition
implicit none
integer,intent(in)::t0,t1,ng
real(dp),intent(in)::x(ng*(t1-t0+1))         ! total length of x-vector
type(t_agg),intent(inout)::agg
real(dp),intent(in)::sf(:)     ! length in steady state
integer,intent(in)::ind,ind_param

real(dp)::xr(t1-t0+1,ng)
integer::sc,ic,ac,zc

xr(:,:)=reshape(x(:),(/(t1-t0+1),ng/))
ac=1


if (ind_param==1)then
    agg%kappah(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
    agg%kappat(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
    !agg%kappai1(t0:t1)=xr(:,ac)/sf(ac)
    !ac=ac+1
    !
    !agg%kappai2(t0:t1)=xr(:,ac)/sf(ac)
    !ac=ac+1

    agg%abar(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
    agg%kappap4(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
    !agg%kappah1(t0:t1)=xr(:,ac)/sf(ac)
    !ac=ac+1
    !
    !agg%kappah2(t0:t1)=xr(:,ac)/sf(ac)
    !ac=ac+1
    
elseif (ind_param==2)then
    
    agg%gammah_s(1,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%gammah_s(2,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1 
    
    agg%gammah_s(3,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
    agg%zeta_cl(1,t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1

    agg%omega_fin(t0:t1)=xr(:,ac)/sf(ac)
    ac=ac+1
    
endif

 
end subroutine sub_getx_5param
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
subroutine sub_testagg(agg,grid,t0,t1)
! test of aggregation: Euler conditions, resource constraints
implicit none
type(t_agg),intent(in)::agg
type(t_grid),intent(in)::grid
integer,intent(in)::t0,t1

integer::pc,yc,ec,ic,jc,kc,sc,tc,kc_p,sc_p,zc
real(dp)::dist,temp,frac,frac_intp
real(dp)::max_euler_err,max_hins_err,max_pins_err,max_frac_err,max_frac_err_intp,max_res_err,max_wageinc_err,max_totinc_err,max_negass_err
real(dp),parameter::epsi=1.0e-04_dp
real(dp),parameter::epsi_frac=1.0e-06

max_euler_err=0.0_dp
max_frac_err=0.0_dp
max_frac_err_intp=0.0_dp

do tc=t0,t1
    
    ! test pension insurance:
    max_pins_err=func_err(agg%pcontr(tc),agg%ben_p(tc),max_pins_err)
    
    ! test Euler condition:
    temp=(agg%ret(tc)+delt)*agg%cap(tc)
    ! +agg%wage(tc)*agg%lab(tc)
    
    do sc=1,ns
        
        temp=temp+agg%wage_s(sc,tc)*agg%lab_s(sc,tc) 
        
    end do
    
    
    max_euler_err=func_err(temp,agg%gdp(tc),max_euler_err) 

    ! test resource constraint: expenditure side of GDP
    temp=agg%inv(tc)+agg%gdp(tc)*gyr_str+agg%cons(tc) + agg%igov(tc) + agg%clcost(tc)  ! agg%clsubs(tc) + agg%privclsubs(tc) !- agg%bq(tc)  !+ agg%moninv_tot(tc) - agg%bq(tc)   !agg%ivt(tc)
    max_res_err=func_err(temp,agg%gdp(tc),max_res_err)
    print*, agg%gdp(tc), temp,agg%moninv_tot(tc), agg%tot_inc(tc), agg%ass(tc), agg%cons(tc),agg%igov(tc),agg%clsubs(tc),agg%gdp(tc)*gyr_str,agg%gdp(tc)*0.25_dp
    
    ! test aggregation of wage income
    max_wageinc_err=func_err(agg%wage_inc(tc),agg%wage_inc_hh(tc),max_wageinc_err)

    ! test aggregation of total income
    max_totinc_err=func_err(agg%tot_inc(tc),agg%tot_inc_hh(tc),max_totinc_err)

    temp=1.0_dp-agg%negass(tc)/agg%cap(tc)
    max_negass_err=func_err(temp,1.0_dp,max_negass_err)
    
    ! test fraction sitting at second to last grid point in asset grid 
    do sc=1,ns
        do kc=1,nk
            do jc=1,nj
                do pc=1,np
                    frac=sum(grid%Phi(:,nx-1,pc,:,:,:,:,kc,:,sc,:,:,jc,tc))
                   
                    if (opt_cpl)then
                        do sc_p = 1,ns
                            do kc_p = 1,nk
                           !     frac=frac + sum(grid%Phi_cpl(:,nx-1,pc,:,:,:,kc,kc_p,sc,sc_p,jc,tc))
                            enddo
                        enddo
                        
                    endif
                    
                    if ( frac>epsi_frac ) then
                        if ( frac>max_frac_err ) then
                            max_frac_err=frac
                        endif    
                    endif
                   
                end do
            end do
        end do
    end do

end do

if (max_euler_err>epsi) print*, 'maximium Euler equation error is: ', max_euler_err
if (max_pins_err>epsi) print*, 'maximium pension insurance error is: ', max_pins_err
if (max_totinc_err>epsi) print*, 'maximum total income error is: ', max_totinc_err 

if (max_res_err>epsi_frac) print*, 'maximum resource error is: ', max_res_err 
!if (max_wageinc_err>epsi_frac) print*, 'maximum wage income error is: ', max_wageinc_err 
!if (max_frac_err>epsi_frac) print*, 'maximum grid error is: ', max_frac_err 
!if (max_frac_err_intp>epsi_frac) print*, 'maximum intp grid error is: ', max_frac_err_intp 
!if (max_negass_err>epsi_frac) print*, 'maximum negative asset share is: ', -max_negass_err 

contains


! -----------------------------------------------------------------
function func_err(x,y,max_err)
implicit none
real(dp)::func_err
real(dp),intent(in)::x,y,max_err
real(dp)::dist

func_err=max_err
dist=abs(x/y-1.0_dp)
if (dist>max_err) func_err=dist

end function func_err
! -----------------------------------------------------------------


end subroutine sub_testagg
! --------------------------------------------------------------------------------------
 
    
end module olg_mod





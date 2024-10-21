module solve_hh_main
    
use nrtype
use params_mod
use types_mod
use funcs_mod
use dc_egm_mod
use solvehhdlabpar_mod
!use solvecpl_mod,only:sub_solvecpl 
use omp_lib

implicit none

    contains
    
! ------------------------------------------------------------------- 
subroutine sub_solvehh_main(agg,demo,grid,pol,t0,t1,itm)
implicit none
type(t_agg),intent(inout)::agg
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
integer,intent(in)::t0,t1,itm
 
integer:: tc,tcp1,kc,sc,jc,gc,mc,xc,dc,yc,ec,ic,nspar_max,scpar,jmax_parent
real(dp)::R,R_til_tp1,betta_til,b(ns,nj),b_til_tp1(ns,nj),alpha1k,alpha0k,alpha1s,alpha0s,distv,distvm
integer:: mc_min,mc_max,opt_trans_calib,gckid,ickid,pc,hc,jc_kid,jmin_parent,jckid,epsc,epsc1,epsc2,kc1,kc2,sc1,sc2
real(dp):: dist_vf 
real(dp),parameter::epsi=1.0e-04_dp
! for linear univariate interpolation
integer::inds(2),indsh(2),inds2x(2,2)
real(dp)::vals(2),valsh(2),vals2x(2,2)
real(dp),allocatable:: v_guess(:,:,:,:,:,:,:,:,:,:,:,:)
integer:: jc_lockdown_tmp,itv,itvm,jc_kid_loc,tc_loc
integer,parameter:: maxitv = 2 

print*, "start hh"

!if (opt_trref==1 .and. opt_margprg==1)then
!    tau_pr = 0.072_dp
!endif 

opt_ext_ls = 1
opt_ext_compute = 1

! time settings
do tc=t1,t0,-1
    
    if (opt_trref==0)then
    
        !print*, "MOMENTS"
        !print*, "gyr: ", agg%gconsr(tc)
        !print*, "kyr: ", agg%ky(tc)
        !print*, "ivt/ass: ", agg%ivty(tc)
        !print*, "mon inv", agg%moninv(tc)
        !print*, "tinv", agg%tinv(tc)
        !print*, "av HK", agg%h_j(jt,tc)
        !
        !print*, "PARAMS"
        !print*, "beta: ", agg%beta(tc)  
        !print*, "lamtil: ", agg%psi_lambda(tc)
        !print*, "nu: ", agg%nu(tc)
        !print*, "kappai", agg%kappai(tc)
        !print*, "kappa", agg%kappat(tc)
        !print*, "Abar", agg%abar(tc)
        
    
        ! assign outer loops vars to globals 
        nu_param = agg%nu(tclb)  ! altruism
        abar_param = agg%abar(tclb) ! norm param in HK prod
        abar_hs_param = agg%abar_hs(tclb) 
        kappah_param = agg%kappah(tclb) ! weight on HK in outer CE
        kappai_b_param = agg%kappai1(tclb) 
        kappai_c_param = agg%kappai2(tclb)
        kappah_b_param = agg%kappah1(tclb) 
        kappah_c_param = agg%kappah2(tclb)
        !call sub_getinterc_linlog(1.0_dp,3,kappai_param,-kappai_b_param)
        kappai_param = -0.9_dp + kappai_b_param * (3+2) + kappai_c_param * (3+2)**2.0_dp 
        kappat_param = agg%kappat(tclb) ! weight on time in utility
        fitm_param = agg%fi1(tclb) ! s.e. money and time
        
        kappap4 = agg%kappap4(tclb)
        !kappah8 = agg%kappah8(tclb)
        
        iota_param = agg%iota(tclb)
        h0delta_param = agg%h0delta(tclb)
        
        inc_thrs_param = agg%inc_thrs(tclb)
        
        ! compute outer CES parameters:
        
        ! weight on human capital: thsi is IRRELEVANT CURRENTLY
        call sub_getalpha_kappa(kkappa(1),kkappa(2),alpha0k,alpha1k)
       ! call sub_param_jincr_concave(alpha0k,alpha1k,kappa_j_param)        
                
        ! S.E. across periods
        call sub_getalpha_sigma(ssigma(1),ssigma(2),alpha0s,alpha1s)
        call sub_param_jincr_convex(alpha0s,alpha1s,sigma_j_param)
        do jckid =1, jt-jf    
            fi0_j_param(jckid) =( sigma_j_param(jckid) - 1.0_dp ) / sigma_j_param(jckid)
        enddo
                
        !print*,"here", sigma_j_param,alpha0s,alpha1s
        !pause
        
        do jc_kid=1,4
            !call sub_param_linlog(kappai_param,-kappai_b_param,kappai_j_param(jc_kid),jc_kid ) 
            if (jc_kid==1)then
                jc_kid_loc = 1
            
            elseif (jc_kid==4)then
                jc_kid_loc = 6
            else
                jc_kid_loc = jc_kid*2-1     ! 3 and 5
            endif 
            
            kappai_j_param(jc_kid) =exp( kappai_param - kappai_b_param * (jc_kid_loc+2) - kappai_c_param * (jc_kid_loc+2)**2 )
            kappah_j_param(jc_kid) =exp( kappah_param - kappah_b_param * (jc_kid_loc+2)  - kappah_c_param * (jc_kid_loc+2)**2 )
            !kappah_j_param(jc_kid) =exp( kappah_param - kappah_b_param * jc_kid) ! - kappah_c_param * jc_kid**2 )
        enddo
        
        inv_mean = max(0.1_dp,agg%inv_mean(tclb))
        
        !print*, kappah_j_param(1:6),kappah_param, kappah_b_param,  kappah_c_param 
        !pause
        
        !print*, kappai_j_param,1.0_dp - (-kappat_b_param * (2)  ) 
        !pause
        
        
        ! weigt on money SLOPE         
        !kappai_b_param =agg%kappai1(tclb) !0.0_dp ! slope of money param
        !kappai_j_param = kappai_param ! NO age dependency for now
        
      !  call sub_param_jincr_concave(kappai_param,kappai_b_param,kappa_j_param) !kappai_j(jc_kid) = kappai_param *exp( kappai_b_param * (jc_kid-1)  )
       ! kappa_j_param =kappah_param 
  
        if (opt_ageindep==1)then
            kappah_j_param = sum(kappah_j_param) / size(kappah_j_param,1)
            fi0_j_param = sum(fi0_j_param) / size(fi0_j_param,1)
           
            kappai_j_param = sum(kappai_j_param) / size(kappai_j_param,1)
        endif
    
        h0norm_param = agg%h0norm(tclb)

        ! initial h0 distn
        !demo%h0prop(1,:) = (/0.843086904_dp, 0.843086904_dp,	0.906421724_dp,	1.106887727_dp/)
        !demo%h0prop(2,:) = demo%h0prop(1,:)
        if (opt_trref==0) call sub_h0distr(agg%fh0_no(tclb),agg%fh0_cl(tclb),agg%h0norm(tclb),demo%h0prop,agg%h0distr)
    

    
        gamma_s(:) =1.0_dp! 1.5_dp * ft_lab
            
        ! enodgenously calibrated parameters
        betta = agg%beta(tclb)
    
        pheye(2,1) = agg%phi(tclb)  
        pheye(2,2) = agg%phi(tclb)
   
        pheye(1,1) = agg%phi(tclb)
        pheye(1,2) =agg%phi(tclb)

        psi_ls = agg%psi_ls(tclb)
        
        zeta_cl =agg%zeta_cl(1,tclb) !-0.5_dp 
        taste_cl =agg%zeta_cl(2,tclb)
        
        omega_fin = agg%omega_fin(tclb)
        omega_fin_hs = omega_fin * 3.9_dp
        
        gamdelta_hs = agg%gamdelta(tclb) !0.5248_dp !* 1.438_dp !* agg%gammah_s(1,tclb)  !0.646_dp
        
        scaledelta(1) = 1.0_dp
        scaledelta(2) = 1.0_dp 
        scaledelta(3) = 1.3621_dp !* agg%gammah_s(2,tclb)/agg%gammah_s(1,tclb)
        scaledelta(4) = 1.7180_dp !* agg%gammah_s(3,tclb)/agg%gammah_s(1,tclb)
        !scaledelta(3) = 1.7_dp
        
        do sc=1,ns
           meandelta_s(sc) =agg%gammah_s(sc,tclb)! 1.0_dp !0.95_dp
        enddo
        
        probmar_s(1,:,:) = 0.5_dp !prob_mar_s(:,1)
        probmar_s(2,:,:) = 1.0_dp - probmar_s(1,1,1)
        
        probmar(1) = 0.5_dp !prob_mar_s(:,1)
        probmar(2) = 1.0_dp - probmar(1)
        
        
        if (nk>1)then
            do sc=1,ns
                gamdelta(sc) = gamdelta_hs * scaledelta(sc)
                gammafe(sc,1) = meandelta_s(sc)- 0.5_dp *gamdelta(sc)
                gammafe(sc,2) = meandelta_s(sc)  + 0.5_dp *gamdelta(sc)
            enddo
        else
            gammafe(:,1) = 1.0_dp
        endif
       
        ! adjustment factor for expressing transfers as a fractions of AW, not relevant here, I just keep it
        if (tc==tclb .and. opt_calib_loc==1)then
            adj_fac_tr = agg%avearn(tclb) / agg%avwage(tclb)
            avwage_tclb = agg%avwage(tclb)
        endif
   
        ! if SOE, then set ret accordingly 
        if (opt_soe) agg%ret(tc) = agg%ret_soe(tc)
    
        
    
        
    endif
    
    ! recover AGGREGATE gross acc bequests from bqr
    agg%bq(tc)  =agg%bqr(tc) *agg%avwage(tc) * sum(agg%pop_j(js:jr(1)-1,tc)) ! (jr(1) - 1 - (js+1) + 1) 
    
    
    
    ! deduct funds for private subsidies
    
    if ( (agg%bq(tc) - 0.166 * agg%clcost(tc) ) < 0.0_dp )then
        priv_subs_param = agg%bq(tc) / agg%clcost(tc)
    else
        priv_subs_param = 0.166_dp
    endif
    
    print*, "acc beq", agg%bq(tc),agg%bqr(tc) ,agg%avwage(tc), priv_subs_param
    
    
    agg%bq(tc) =max(0.0_dp , agg%bq(tc) - priv_subs_param * agg%clcost(tc) )
    
    ! now, from AGG acc bq to per capita bq
    agg%bq(tc) = agg%bq(tc) /  sum(agg%pop_j(js:jr(1)-1,tc)) ! (jr(1) - 1 - js + 1)
    
    if (opt_accbq>1)then ! redistribute them LUMP-SUM
        
        ! TBC
        agg%tr_sj(:,:,tc) = 0.0_dp
        agg%tr_sj(:,js:jr(1)-1,tc) = agg%bq(tc)
           
        
    else ! burn them
        agg%tr_sj(:,:,tc) = 0.0_dp
    endif
        
    print*, "acc beq1", agg%bq(tc),agg%bqr(tc) ,agg%avwage(tc)
    
    
    if (t0==t1) then
        tcp1=tc
        
        if (opt_calib_loc==1 .and. opt_calib==2)then
            opt_trans_calib=1
        else
            opt_trans_calib=0
        endif
        
    else
        tcp1=min(tc+1,nt)   ! by min operator we can also compute solution in period nt in transition
    endif
    
    
    tau_c = agg%tau_c(tc)
        
        
    ! avearn and avearn_tp1 for tax function indexation
    if (opt_calib_loc==1 .and. tc==tclb)then        
        avearn = agg%avearn(tclb)
        avearn_tp1 = agg%avearn(tclb)
    else
        avearn =  agg%avearn(tc)
        avearn_tp1 = agg%avearn(tcp1) 
    endif
    
    if (opt_trref==0)then
        avearn_base = avearn
        avearn_tp1_base = avearn_tp1
    endif
    
    lambda_pr = agg%psi_lambda(tc)
    
   
   ! print*, "psi lamb", agg%psi_lambda(:), agg%tau_c(:)
   
    
    if (opt_trref==0 .and. opt_init_ss==1)then

        call sub_norm_ageprod(demo%ageprod(1,:,:) ,demo%ageprod_dat(1,:,:),agg%wage_s(:,tc),agg%ret(tc) ) 
        !
        !print*, "adjusted prod1",demo%ageprod(1,:,1)*agg%wage_s(1,tc)
        !print*, "adjusted prod2",demo%ageprod(1,:,2)*agg%wage_s(2,tc)
        !print*, "adjusted prod3",demo%ageprod(1,:,3)*agg%wage_s(3,tc)
 
    
        ! for pension payments
        call sub_pia(agg,demo,grid,tc,tc) 
        do tc_loc=1,nt
            agg%pia0(:,:,:,tc_loc) = agg%pia0(:,:,:,1)
        enddo
    
        ! object used for all normalizations: LABOR INCOME of si lo (reference group) 
        earn_si_lo = agg%avearn(tclb) * 0.8_dp ! !agg%wage_s(1,tc) * demo%ageprod(1,js,1) * 0.6_dp   ! agg%avearn(tclb)
        

        !print*, "av wage", agg%avwage(tc) / earn_si_lo
        !pause
        
        ! set borrowing limits
        agg%amin_param(tc) =- bclimit * earn_si_lo
        demo%ass_input = demo%ass_input_dat * earn_si_lo 
    
                    
        grid%minv_grid = grid%minv_grid_dat * earn_si_lo !
    
    
        igov_j(1) = igov_frac * earn_si_lo * 0.25_dp !* edu_spend_fac  ! this goes into the gvt budget, cost side; the production input in HK function is increased by the same proportion, ie implicit assumption of linear technology
        igov_j(2) = igov_frac * earn_si_lo !* 0.5_dp
        igov_j(3:4) = igov_frac * earn_si_lo 
    
        igovbase_j = igov_j
        
        !agg%amin_param(tclb) =- 0.75_dp * fee_flow*2.0_dp * (1.0_dp - colsubs_pub - colsubs_priv)
    
      !  call sub_rep_parent(grid%amin_age(:,:,1,:),1.0_dp+agg%ret(tc), demo%ass_input(:,:,1) ,tc)
       
        call sub_rep_child(grid%amin_age(:,:,2,:),1.0_dp+agg%ret(tc),agg%amin_param(tclb),tc)
        
        call sub_rep_child(grid%amin_age(:,:,1,:),1.0_dp+agg%ret(tc),agg%amin_param(tclb),tc)
        
        !print*, grid%amin_age(1,3,1,:)
        !pause

        mean_moninv = 0.0275_dp *earn_si_lo 
        mean_tinv = 0.45_dp * ft_lab
        
    endif
    
    if (opt_trref==1) igov_j(2:4) = igovbase_j(2:4) * igov_ref
        
    edu_spend_fac = agg%edu_spend_fac(tc) ! relative increase of edu spending in a reform scenario (relative to baseline)
        
    subs_param_frac = subs_param_base_frac * (edu_spend_fac - 1.0_dp ) 
       
    subs_param = subs_param_frac * earn_si_lo
        
    !if (opt_trref==0)then
    !    fee_flow =agg%kappafee(tc) * agg%wage_s(ns,tc) * demo%ageprod(1,js,ns)/demo%ageprod_dat(1,js,ns) ! (fee_flow_frac) * earn_si_lo
    ! 
    !     col_subs_flow =fee_flow * agg%varrho(tc)  
    ! 
    !     col_subs_cost = col_subs_flow
    !     
    !     fee_flow_base = fee_flow
    !     col_subs_flow_base = col_subs_flow
    !     col_subs_cost_base = col_subs_cost
    ! else
    !     fee_flow = fee_flow_base
    !     col_subs_flow = col_subs_flow_base
    !     col_subs_cost = col_subs_cost_base
    ! endif
        
    !fee_flow =iota_param * agg%wage_s(ns,tc) *demo%ageprod(1,js+1,3) !  (fee_flow_frac) *agg%avearn(tc) ! earn_si_lo
    !    
    !col_subs_flow =(0.388_dp + 0.166_dp)*fee_flow !  subs_param
    !col_subs_cost =0.388_dp * fee_flow ! (subs_param_frac + subs_param_base_frac)*agg%avearn(tc) !earn_si_lo
    !
    !fee_flow_net = 1 - col_subs_flow
    !
    !print*, "fee flow new", fee_flow
    !print*, "subs flow new", col_subs_flow
    
    
    if (opt_trref==0)then
        fee_flow = (fee_flow_frac) * earn_si_lo / (1.0_dp -0.388_dp - priv_subs_param )
        
        col_subs_param = 0.388_dp
        
        col_subs_flow = (col_subs_param  + priv_subs_param)*fee_flow
        col_subs_cost = col_subs_param *fee_flow ! (subs_param_frac + subs_param_base_frac)*earn_si_lo
    
    !print*, "fee flow old", fee_flow
    !print*, "subs flow old", col_subs_flow
    
        ! iota should be fixed exogenously in baseline to whatever it is implied
        iota_param= fee_flow / ( agg%wage_s(ns,tc) *demo%ageprod(1,js+1,3)  ) 
        agg%iota(tclb) = iota_param
    else
    
       ! fee_flow = iota_param * ( agg%wage_s(ns,tc) *demo%ageprod(1,js+1,3)  ) 
       ! col_subs_flow = (0.388_dp + 0.166_dp)*fee_flow
        
        col_subs_param =col_subs_param_ref ! 1.5_dp
        
        col_subs_flow = (col_subs_param  + priv_subs_param)*fee_flow 
        col_subs_cost = (subs_param_frac + subs_param_base_frac)*earn_si_lo
    
        col_subs_cost = col_subs_param *fee_flow
        
    endif
    
   !pause
     
   ! demo%ageprod_dat(1,:,:) 
    ! transformation of rate of return:
    R=1.0_dp+agg%ret(tc)
    R_til_tp1=(1.0_dp+agg%ret(tcp1)) /(1.0_dp+techgr) 
    ! achtung make bq dummy arguments also skill and age specific (this was relevant once redistributive scheme was used, now they are anyway lump-sum)
    b(:,:)=agg%tr_sj(:,:,tc)
    b_til_tp1 = agg%tr_sj(:,:,tcp1) 
    
    opt_cpl_comp = 0
    
    if (opt_init_ss==1 .or. opt_fin_ss==1)then
        
        if (opt_init_ss==1 .and. opt_vf_init==1 )then
            
            ! solve hh problem
            do jc=nj,js,-1    
                 
                
                            
                do scpar = 1,ns
            
                    !$OMP PARALLEL private(sc,kc,hc,epsc)
                    !$OMP DO collapse(3)   
                    do kc= 1,nk
                        do sc=1,ns
                            do epsc=1,nw
                            do hc=1,hcmax(jc,1) !np
                                do gc=1,ng
                        
                                    call sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc,hc,epsc,sc,gc,scpar,jc,tcp1,tc,2,nj)  
                        
                                end do  ! end do kc
                            enddo
                            enddo
                    
                        end do  ! end do sc                     
                    enddo ! end do gc
                    !$OMP END DO
                    !$OMP END PARALLEL
                
                    grid%coh_child(:,:,:,:,:,:,:,:,:,:,1,scpar,jc,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,1,min(scpar,scpmax(jc,1)),jc,tc)
                    
                    pol%v_child(:,:,:,:,:,:,:,:,:,:,1,scpar,jc,tc) = pol%v(:,:,:,:,:,:,:,:,:,:,1,min(scpar,scpmax(jc,1)),jc,tc)
                   
                    
                enddo
            enddo
       
            ! STORE THE CHILD VALUE AT AGE ja
            pol%v_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
            grid%coh_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
            
            print*, "DONE WITH CHILD PROBLEM"
            !pause
        
          !  print*, "vkid1", pol%grinc(nd,:,1,1,2,1,1,1,1,1,1,1,nj-1,tc),agg%tau_c(tc), agg%ret(tc), R_til_tp1, avearn, avearn_tp1, tau_k
        
        else
            
            pol%v_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,1)
            grid%coh_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,1)
       
            grid%coh_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
            pol%v_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
            
            
        endif
        
        ! now, solve problem of PARENT generation
        opt_ext_ls = 1
        jmin_parent = js
        jmax_parent = nj
        
        if (probmar(2) > 0.0_dp   )then    
               print*, "TEST COUPLE",nj,jr(1),jt,jf,js
                do jc=nj,jf-2,-1   !nj,jmin_parent,-1

                !print*, "age in couple", jc
                !$OMP PARALLEL private(sc1,sc2,kc1,kc2,epsc1,epsc2) 
                !$OMP DO collapse(6)      
                do kc1= 1,nk
                    do kc2 = 1,nk
                        do sc1=1,ns
                            do sc2 = 1,ns
                                do epsc1=1,nw
                                    do epsc2 = 1,nw
                        
                                        call sub_solvehh_cpl(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc1,kc2,epsc1,epsc2,sc1,sc2,jc,tcp1,tc,1,nj)        
                                    enddo
                                enddo
                            enddo
                        
                       
                        end do  ! end do kc
                        enddo                          
                    end do  ! end do sc                     
           
                !$OMP END DO
                !$OMP END PARALLEL
           
            
                enddo
       !pause
            
        endif 
        
        opt_cpl_comp = 1
        
       do jc=jmax_parent,jmin_parent,-1   !nj,jmin_parent,-1

            
            do scpar = 1,scpmax(jc,1) 
            do hc=1,hcmax(jc,1) !np-2,np-2 !np
            !$OMP PARALLEL private(sc,kc,epsc) 
            !$OMP DO collapse(3)      
            do kc= 1,nk
                do sc=1,ns
                    do epsc=1,nw
                    do gc=1,ng            
                        call sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc,hc,epsc,sc,gc,scpar,jc,tcp1,tc,1,nj)            
                       
                    end do  ! end do kc
                    enddo                          
                end do  ! end do sc                     
            enddo ! end do gc
            !$OMP END DO
            !$OMP END PARALLEL
            enddo  
            enddo
            !if (jc<=jt .and. jc>=jf)then
            !    grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%coh_exo(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)
            !endif
            
            
            
            
       enddo
       
       
       !if (opt_vf_loop==1)then
               pol%v_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
               grid%coh_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
               pol%v_guessm(:,:,:,:,:,:,:,:,:,:,:,:,tc) =  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
               grid%coh_guessm(:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
       
               grid%coh_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)
               pol%v_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)
       
       
       
           
           itvm = 0
           distvm = 9000.0_dp
           
           do while (itvm<8)
           
               itv = 0

if (itvm>2)then
opt_ext_ls = 1
else
opt_ext_ls = 1
endif

if (itvm>3)then
    opt_ext_compute=1
endif

               
                if (probmar(2) > 0.0_dp   )then    
                print*, "TEST COUPLE",nj,jr(1),jt,jf,js
                do jc=jt,jf-2,-1   !nj,jmin_parent,-1

                !print*, "age in couple", jc
                !$OMP PARALLEL private(sc1,sc2,kc1,kc2,epsc1,epsc2) 
                !$OMP DO collapse(6)      
                do kc1= 1,nk
                    do kc2 = 1,nk
                        do sc1=1,ns
                            do sc2 = 1,ns
                                do epsc1=1,nw
                                    do epsc2 = 1,nw
                        
                                        call sub_solvehh_cpl(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc1,kc2,epsc1,epsc2,sc1,sc2,jc,tcp1,tc,1,nj)        
                                    enddo
                                enddo
                            enddo
                        
                       
                        end do  ! end do kc
                        enddo                          
                    end do  ! end do sc                     
           
                !$OMP END DO
                !$OMP END PARALLEL
           
            
                enddo
                
                opt_cpl_comp = 1 
           
               do jc=jt,jmin_parent,-1   !nj,jmin_parent,-1

            
                do scpar = 1,scpmax(jc,1) 
                    do hc=1,hcmax(jc,1)
                    !$OMP PARALLEL private(sc,kc,epsc)
                    !$OMP DO collapse(3)       
                    do kc= 1,nk
                        do sc=1,ns
                            do epsc=1,nw 
                            do gc=1,ng            
                                call sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc,hc,epsc,sc,gc,scpar,jc,tcp1,tc,1,nj)            
                       
                            end do  ! end do kc
                            enddo                          
                        end do  ! end do sc                     
                    enddo ! end do gc
                    !$OMP END DO
                    !$OMP END PARALLEL 
                    enddo 
                enddo
                    !if (jc<=jt .and. jc>=jf)then
                    !    grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%coh_exo(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)
                    !endif
            
               enddo
      
            
            endif
               
           if (distvm<0.000001_dp)then
       
           do while (itv <1)
       
            
              
          opt_cpl_comp = 1 
           
               do jc=jt,jmin_parent,-1   !nj,jmin_parent,-1

            
                do scpar = 1,scpmax(jc,1) 
                    do hc=1,hcmax(jc,1)
                    !$OMP PARALLEL private(sc,kc,epsc)
                    !$OMP DO collapse(3)       
                    do kc= 1,nk
                        do sc=1,ns
                            do epsc=1,nw 
                            do gc=1,ng            
                                call sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc,hc,epsc,sc,gc,scpar,jc,tcp1,tc,1,nj)            
                       
                            end do  ! end do kc
                            enddo                          
                        end do  ! end do sc                     
                    enddo ! end do gc
                    !$OMP END DO
                    !$OMP END PARALLEL 
                    enddo 
                enddo
                    !if (jc<=jt .and. jc>=jf)then
                    !    grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%coh_exo(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)
                    !endif
            
               enddo
               
           
           
               distv =  maxval(abs( pol%v(:,:,1,:,:,:,:,:,:,:,:,:,js,tc) - pol%v_guess(:,:,1,:,:,:,:,:,:,:,:,:,tc) ) )
       
               print*, "dist", distv !,pol%v(1,:,1,1,1,1,1,1,1,1,1,1,jf,tc) 
               
               pol%v_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
               grid%coh_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
       
               grid%coh_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)
               pol%v_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)
       
           
               itv = itv + 1
               
               
           
               if (distv < epsi)  exit
               !pause
           
           enddo
           endif
            
                distvm =  maxval(abs( pol%v(:,:,1,:,:,:,:,:,:,:,:,:,js,tc) - pol%v_guessm(:,:,1,:,:,:,:,:,:,:,:,:,tc) ) )
                pol%v_guessm(:,:,:,:,:,:,:,:,:,:,:,:,tc) =  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
                grid%coh_guessm(:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc)
                grid%coh_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)
                pol%v_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,:,tc)
       
                print*, "distm", distvm !,pol%v(1,:,1,1,1,1,1,1,1,1,1,1,jf,tc) 
                
                itvm = itvm + 1
               
               
           
               if (distvm < epsi)  exit
           
           
           enddo
       
       !endif
       
       !agg%wage_s(:,tc) = 1.0_dp
       !call sub_norm_ageprod(demo%ageprod(1,:,:) ,demo%ageprod_dat(1,:,:),agg%wage_s(:,tc),agg%meanwage_s(:,tc))      
     !  print*, "vkid1", pol%v(1,:,1,1,1,1,1,1,1,1,1,1,jt,tc),agg%tau_c(tc), agg%ret(tc), R_til_tp1, avearn, avearn_tp1, tau_k
      
       
       print*, "DONE WITH ADULT PROBLEM"
       !!print*, "nokid",pol%v_nokid(nd,:,1,1,2,1,1,1,1,1,1,1,jr(1),tc)
       !!print*, "kid",        pol%v(nd,:,1,1,2,1,1,1,1,1,1,1,jr(1),tc)
       !print*, "nokid",pol%v_nokid(1,:,3,1,1,1,1,1,1,1,1,1,jf,tc)
       !print*, "kid",        pol%v(1,:,3,1,1,1,1,1,1,1,1,1,jf,tc)
       !
       !pause
       
       
      ! endif
       
    else
        
        if (opt_curver==1)then
            if (tc>2)then 
                opt_trref = 0
            else
                opt_trref = 1
            endif
        endif
            
        opt_cpl_comp = 1
        
        opt_ext_compute = 1
        
        grid%coh_old(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh_old(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
        grid%coh_old_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) = grid%coh_old_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
        pol%dc(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) = pol%dc(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
        pol%dc_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) = pol%dc_cpl(:,:,:,:,:,:,:,:,:,:,:,:,:,1)
        
        ! transition: no ffp loop
       
        !pol%v_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tcp1)
        !grid%coh_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tcp1)
        !
        !grid%coh_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,:,tcp1)
        !pol%v_child(:,:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,:,tcp1)
        !
       
        ! print*, "vkid2", pol%v_child(1,6,1,1,1,1,1,1,:,3,1,1,js,tc) 
        !pause

        jmin_parent = js
        jmax_parent = nj
        
        ! first solve for newborns
        jc = js
        do scpar = 1,scpmax(jc,1) 
            do hc=1,hcmax(jc,1)
            !$OMP PARALLEL private(sc,kc,epsc)
            !$OMP DO collapse(3)       
            do kc= 1,nk
                do sc=1,ns
                    do epsc=1,nw 
                    do gc=1,ng            
                        call sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc,hc,epsc,sc,gc,scpar,jc,tcp1,tc,1,nj)            
                       
                    end do  ! end do kc
                    enddo                           
                end do  ! end do sc                     
            enddo ! end do gc
            !$OMP END DO
            !$OMP END PARALLEL 
            enddo 
        enddo
        pol%v_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) !tc) !  pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,nt) !tcp1)
        grid%coh_guess(:,:,:,:,:,:,:,:,:,:,:,:,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) !tc) ! grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,nt) !tcp1)
        !
        grid%coh_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) =grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) !tc) !grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,:,nt) !tcp1)
        pol%v_child(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) =pol%v(:,:,:,:,:,:,:,:,:,:,:,:,js,tc) !)tc) !pol%v(:,:,:,:,:,:,:,:,:,:,:,:,:,nt) !tcp1)
        !
         
        
        ! now for the rest, up to newborns
        
        if (probmar(2) > 0.0_dp   )then    
               print*, "TEST COUPLE",nj,jr(1),jt,jf,js
                do jc=nj,jf-2,-1   !nj,jmin_parent,-1

                !print*, "age in couple", jc
                !$OMP PARALLEL private(sc1,sc2,kc1,kc2,epsc1,epsc2) 
                !$OMP DO collapse(6)      
                do kc1= 1,nk
                    do kc2 = 1,nk
                        do sc1=1,ns
                            do sc2 = 1,ns
                                do epsc1=1,nw
                                    do epsc2 = 1,nw
                        
                                        call sub_solvehh_cpl(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc1,kc2,epsc1,epsc2,sc1,sc2,jc,tcp1,tc,1,nj)        
                                    enddo
                                enddo
                            enddo
                        
                       
                        end do  ! end do kc
                        enddo                          
                    end do  ! end do sc                     
           
                !$OMP END DO
                !$OMP END PARALLEL
           
            
                enddo
       !pause
            
            endif
        
        do jc=nj,js+1,-1   !nj,jmin_parent,-1

            
            do scpar = 1,scpmax(jc,1) 
                do hc=1,hcmax(jc,1)
                !$OMP PARALLEL private(sc,kc,epsc)
                !$OMP DO collapse(3)       
                do kc= 1,nk
                    do sc=1,ns
                        do epsc=1,nw 
                        do gc=1,ng            
                            call sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,b,b_til_tp1,kc,hc,epsc,sc,gc,scpar,jc,tcp1,tc,1,nj)            
                       
                        end do  ! end do kc
                        enddo                           
                    end do  ! end do sc                     
                enddo ! end do gc
                !$OMP END DO
                !$OMP END PARALLEL 
                enddo 
            enddo
            !if (jc<=jt .and. jc>=jf)then
            !    grid%coh(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc) = grid%coh_exo(:,:,:,:,:,:,:,:,:,:,:,:,jc,tc)
            !endif
            
        enddo 
        
       ! print*, "vkid1", pol%v(1,:,1,1,1,1,1,1,1,1,1,1,jt,tc),agg%tau_c(tc), agg%ret(tc), R_til_tp1, avearn, avearn_tp1, tau_k
        

    endif 
    
    
end do  ! end do tc

vf_init_flg =0

!flag_child_solve = 0
 
!print*, "end hh"

end subroutine sub_solvehh_main
! ------------------------------------------------------------------- 
    
    
    
    
    
    
    
end module solve_hh_main
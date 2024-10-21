module solvehhdlabpar_mod
	
use nrtype
use params_mod
use types_mod
use funcs_mod
use dc_egm_mod 
use omp_lib
use inst_mod

implicit none

    contains
  
  
! ------------------------------------------------------------------- 
subroutine sub_solvehh_cpl(agg,demo,grid,pol,R,R_til_tp1,bq,b_til_tp1,kc1,kc2,wc1,wc2,sc1,sc2,jc,tcp1,tc,gen_id,nj_in)
! solution of household policy and value functions
use ESPlot
implicit none
type(t_agg),intent(in)::agg 
type(t_demo),intent(inout)::demo 
type(t_grid),intent(inout)::grid 
type(t_pol),intent(inout)::pol
real(dp),intent(in):: R,R_til_tp1,bq(:,:),b_til_tp1(:,:)
integer,intent(in)::kc1,kc2,sc1,sc2,jc,tcp1,tc,gen_id,nj_in,wc1,wc2 

integer::xc,pc,ec,ic,ycp1(2),yc1p1,yc2p1,wc1p1,wc2p1,ecp1,icp1,dc,dcp1,dc1,dc2,dc1p1,dc2p1,gc,sc(2),yc(2),wc(2),kc(2),yc1,yc2
integer:: dc_min,dc_max,ickid_init,gckid_init,ind_below 
real(dp)::betta_til
real(dp)::grid_ass(nx),grid_sav(nx)
real(dp)::util,gr_wage(2),net_wage(2),wage_totax(2),ben_p(2),gr_ben_p(2),mu_c,coh_min,minv_in,coh_in,temp_vec1(nx),temp_vec2(nx),prob_ni(np),grinc,ben_u,pstk,grpstk,savp,assmax,assmin
real(dp)::coh_tp1,coh
real(dp)::gr_wage_tp1(2),net_wage_tp1(2),wage_totax_tp1(2),pens_tp1(2),gr_pens_tp1(2)
real(dp):: gr_wage1_tp1,gr_wage2_tp1,net_wage1_tp1,net_wage2_tp1
real(dp)::evp_coh_tp1,ev_tp1,ev_tp1_nokid
real(dp),parameter::epsi=1.0e-08
real(dp),parameter::tolf=1.0e-12
real(dp),parameter::min_x=1.0e-08
logical,parameter::opt_chk=.false., opt_chk_inv=.false.,opt_chk_ivt=.false.,opt_chk_inv_cpl=.false.,opt_chk_ivt_cpl=.false.
logical::lb_viol,ub_viol,flg_all_inf
real(dp)::rhs_foc,df
integer::nx0,xc0,flag_invalid,flag_conv,pc_tp1_or,pc_tp1_min,pc_tp1_max,kcp1,dc_in_max,dc_in,dc_str(nx)
real(dp)::vp_coh_tp1,v_tp1,v_tp1_nokid,v_dc_in(4,nx),v_dc_in_intp(4),coh_dc_in(4,nx),x_min,x_max,x_grid(nx),cons_dc_in(4,nx),lab1_dc_in(4,nx),cons_dc_in_intp(4,nx),lab1_dc_in_intp(4,nx), & 
    lab2_dc_in(4,nx),lab2_dc_in_intp(4,nx),sav_dc_in_intp(4,nx)
real(dp):: x_ref(nx)
integer:: num_sec
integer:: top(nx) !,ind_lab(nx)
real(dp),allocatable:: intersectionsx(:),intersectionsy(:)
integer:: dc_temp, dcl, dc_full, nd_glob,tinv_tc,opt_solve_parent
integer,allocatable:: dc_vec(:)
real(dp)::prob_dc(nd),muc,hp1,min_m,trans_1dim,trans_2dim,bc_sav,yroot,cont_val
integer:: sc_p,kc_p,xc_p,pcloc
real(dp):: frac_p(nd),ev_tp1_vec_2dim(nx,np),ev_tp1_vec_2dim_nokid(nx,np),ev_tp1_vec_2dim_trans(nx,np),evp_coh_tp1_vec_2dim(nx,np),time_cost,grid_sav2dim(nx,np),t_glob,ass_exo(nx),logsum_m,prob_m(nminv),coh_endo_2dim(nx,np),cons_endo_2dim(nx,np),lab1_endo_2dim(nx,np),lab2_endo_2dim(nx,np),lab_dc(nd),lab1eff_endo_2dim(nx,np),lab2eff_endo_2dim(nx,np),hp1_loc,inv_ces
integer:: ndlloc,gckid,ickid,bc,mc,pc_tp1,flag_stud
real(dp):: av_prod,av_prod_tp1,hkid_tp1,max_b,b_grid(nb),cons_endo(nx),coh_exo_min,coh_exo_max,coh_exo(nx),cons_temp,val_bc(nb),mcoh,sav_temp,ev_tp1_temp,util_temp,max_m,val_tc(ntinv),dist_foc,vfun_nokid
! for linear interpolation
integer::inds(2),indsh(2),inds2x(2,2)
real(dp)::vals(2),valsh(2),vals2x(2,2)

! transformation of parameters:
betta_til=betta*(1.0_dp+demo%lamt(tc))**(1.0_dp-tetta)

! build exogenous asset grid for last period of life
if (max_x_fac>epsi) then
    grid_ass(1:nx-1)=makegrid(0.0_dp,max_x/max_x_fac,nx-1,3.0_dp)
    grid_ass(nx)=max_x
else
    grid_ass(1:nx)=makegrid(0.0_dp,max_x,nx,3.0_dp)
endif


flag_stud = 0

kc(1)=kc1
kc(2)=kc2
sc(1)=sc1
sc(2)=sc2
wc(1)=wc1
wc(2)=wc2

! problem in last period of life is de-generate: household consumes all resources
if (jc==nj_in) then
        
    if (opt_test_noer)then
        dc_min=nd_cpl
        dc_max=nd_cpl
    else
        dc_min=nd_cpl-2
        dc_max=nd_cpl
    endif
                
    do pc=1,1 ! child hc
        ec=ne
     
        do ic= 1,1
            do yc1=1,ny
            do yc2 = 1,ny
                  yc(1) = yc1
                  yc(2) = yc2
                do dc=dc_min,dc_max ! retired TBC rewrite it generally allowi2 for endogenous retirement
                            
                    do xc=1,nx ! coh
                                
                        ! compute pension income
                        do gc=1,2
                            
                            av_prod =demo%ageprod(gc,jr(tc)-1,sc(gc) )             
                            call sub_pens_new(gr_ben_p(gc),agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc(gc),tc),av_prod, & 
                                demo%grid_y(yc(gc)),0.0_dp,jc,gc,dc,ec,tc,sc(gc),kc(gc),yc(1),nd_cpl)
                            ben_p(gc) = fun_netpens(gr_ben_p(gc),0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
                        enddo
                         
                           
                        ! coh today
                        coh=f_coh_cpl(grid_ass(xc),bq(sc1,jc) + bq(sc2,jc),R,(/0.0_dp,0.0_dp/),ben_p,ec,dc,1,agg%tau_k(tc))
                              
                        pol%indbc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0
                        grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=coh
                        grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=grid_ass(xc)
                        !grid%assmin_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=f_grid_ass_cpl(grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),bq(sc1,jc) + bq(sc2,jc),R_til_tp1,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1) 
                        pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=f_netcons(coh,agg%tau_c(tc))
                        pol%leis1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=biggam
                        pol%leis2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=biggam
                        pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0.0_dp
                        pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0.0_dp
                        pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0.0_dp
                        pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0.0_dp
                        grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0.0_dp 
                           
                        ! utility (value) function and its derivative
                        util=f_util_cpl(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,0.0_dp,ec,dc,sc1,sc2,gen_id)
                        pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=f_vfun(util,0.0_dp,betta_til,1.0_dp)
                        pol%v_nokid_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)

                        call sub_muc(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,mu_c)
                            
                        ! inverse of derivative of value function w.r.t. coh
                        pol%inv_vp_x_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=f_inv_vp_coh(mu_c)
                            
                        call sub_incsav_cpl(grid_ass(xc),0.0_dp,pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,0.0_dp,R,gr_wage,gammafe(sc1,kc1),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc,pol%grwage_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                            pol%grinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%netinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%netsav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                            pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%capinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%constaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                            pol%penstaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),jc,tc,1,ben_u, & 
                            demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),tau_k,pol%hsvtmp1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%hsvtmp2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%net_trr_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)) !,agg%inc_thrs(tc) )
                       
                    end do !enddo xc
                   
                enddo ! enddo dc
            enddo ! enddo yc  
            enddo
        enddo    
      
    end do  ! end do pc
        
        
else    ! solution for all other ages
     
    do gckid=1,1 ! gender child
        do ickid=1,1! innate ability child
              
            do ic = 1,1
           
                do yc1=1,ny  ! income states today (for retirees these are last worki2 period income shocks)
                    do yc2 = 1,ny
                        yc(1) = yc1
                        yc(2) = yc2

                    do ec=ec_min(gen_id,jc,tc),ec_max(gen_id,jc,tc) ! emp states
                                   
                        ! for each ec, determine set of discrete choices available
                        if (ec==1)then ! worker state 
                         
                            ndlloc = ndl*ndl
                            pol%lab1_cpl(:,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = 0.0_dp
                            pol%lab2_cpl(:,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = 0.0_dp
                                
                            do dc=1,ndlloc
                               
                                pol%lab1_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%lab(1,1,sc1)
                                pol%lab2_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%lab(2,1,sc2)
                                pol%lab1_eff_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%lab(1,1,1) !grid%lab_base(gc,dc,sc)
                                pol%lab2_eff_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%lab(2,1,2)
                                !lab_eff_dc(dc) = grid%lab(gc,dc,sc) 
                                
                                    
                            enddo
                        
                            if ( opt_test_noer==1)then ! no end retirement
                            
                                allocate(dc_vec(ndlloc))
                                            
                                do dcl=1,ndlloc
                                    dc_vec(dcl)=dcl
                                enddo
                                            
                            elseif (opt_test_noer==0)then ! yes end retirement
                                
                                if (jc>=jer .and. jc<jr(tc))then
                                
                                    allocate(dc_vec(ndlloc+1))
                                    do dcl=1,ndlloc
                                        dc_vec(dcl) = dcl
                                    enddo
                                    dc_vec(ndlloc+1) = nd_cpl
                                    
                                else
                                
                                    allocate(dc_vec(ndlloc))                                            
                                    do dcl=1,ndlloc
                                        dc_vec(dcl)=dcl
                                    enddo
                                endif
                                                                            
                        
                            endif                                        
                                      
                      
                        elseif (ec==ne)then ! retired, no choices
                        
                            pol%lab1_cpl(:,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = 0.0_dp
                            pol%lab2_cpl(:,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = 0.0_dp
                            pol%lab1_eff_cpl(:,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = 0.0_dp
                            pol%lab2_eff_cpl(:,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = 0.0_dp
                            
                            if (opt_test_noer)then ! no end retirement => put everybody in nd
                                allocate(dc_vec(1))
                                dc_vec(1)=nd_cpl
                            else ! end ret: nd-1 is ER, nd and nd-2 is LATE
                                if (jc>=jr(tc))then ! after mandatory age => late ret                            
                                    allocate(dc_vec(3))
                                    dc_vec = (/nd_cpl-2, nd_cpl-1, nd_cpl/)
                                elseif (jc>=jrr .and. jc<jr(tc))then ! regular ret SAME as LATE (only two retirement categories, but still keep it with three indices)
                                    allocate(dc_vec(2))
                                    dc_vec = (/nd_cpl-1, nd_cpl/)
                                elseif (jc>=jer .and. jc<jrr)then ! early ret
                                    allocate(dc_vec(1))
                                    dc_vec(1)=nd_cpl-1
                                else
                                    
                                    print*, "what else?"
                                    pause
                                endif
                                                                    
                            endif
                         
                           
                        endif                                
                                
                            
                            
                        do dc_temp=1,size(dc_vec)
                           
                            dc = dc_vec(dc_temp)
                        
                            ! wages and pensions
                            call sub_wagepens_cpl()
                             
                            do pc=1,pc_max(gen_id,jc)   ! child hkpc_tp1 = pcloc,pcloc
                                if (jc<jr(1) ) then
                                    dc_in_max = 4
                                else
                                    dc_in_max = 1
                                endif 
                                
                                
                                
                                flag_invalid = 0
                                        
                                if (gen_id==1 .and. jc<jt .and. jc>=jf)then
                                    pc_tp1 = pc
                                else
                                    pc_tp1 = 1
                                endif
                                
                                pc_tp1_or = pc_tp1
                                
                                if (gen_id==1 .and. jc==jf-1)then
                                    if (opt_corr==1)then
                                        call basefun(grid%hk_grid,np,agg%h0distr(2,sc2),vals,inds)
                                    else
                                        call basefun(grid%hk_grid,np,agg%h0distr(kc2,sc2),vals,inds)
                                    endif
                                    prob_ni = 0.0_dp
                                    prob_ni(inds(1) ) = vals(1)
                                    prob_ni(inds(2) ) = vals(2)
                                
                                    pc_tp1_min = inds(1)
                                    pc_tp1_max = inds(2)
                                    
                                else
                                    pc_tp1_min = pc_tp1_or
                                    pc_tp1_max = pc_tp1_or
                                    prob_ni = 0.0_dp
                                    prob_ni(pc_tp1) = 1.0_dp
                                    
                                endif
                                        
                                grid_sav(:)=f_grid_sav(xc0)
                                grid%sav_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=grid_sav(:)
                                grid_sav2dim(:,pc) = grid_sav 
                                        
                                
                                do dc_in = 1,dc_in_max
                                
                                do xc=nx,xc0,-1
                                                     
                                    call sub_solvehh_sav(grid_sav(xc),.false.,flag_invalid,dc_in,0 )
                                        
                                    ev_tp1_vec_2dim(xc,pc) = ev_tp1
                                    ev_tp1_vec_2dim_nokid(xc,pc) = ev_tp1_nokid
                                        
                                    evp_coh_tp1_vec_2dim(xc,pc) = evp_coh_tp1
                                          
                                enddo
                                            
                           
                                ! borrowi2 constraint
                                if (xc0>1) then
                                       
                                    ! compute coh for zero assets 
                                    
                                    coh_min=f_coh_cpl(grid%amin_age(1,sc1,gen_id,jc-1),bq(1,jc-1)+bq(2,jc-1),R_til_tp1,net_wage,ben_p,ec,dc,ic,agg%tau_k(tc))
                                    
                                                
                                    if (coh_min>grid%coh_cpl(dc,np1_aux,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)) then
                                                    
                                        coh_min=0.99_dp*grid%coh_cpl(dc,np1_aux,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)   ! use some value smaller than previous gridpoint
                                        flag_invalid=1
                                    else
                                        flag_invalid=0
                                    endif
                                        
                                    ! build aux grid (linear)
                                    grid%coh_cpl(dc,1:np1_aux,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=makegrid(coh_min,grid%coh_cpl(dc,np1_aux,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),np1_aux,1.0_dp)
                            
                                    ! solve hh problem for exo part of coh grid
                                           
                                    do xc=1,np1_aux-1
                                        if (flag_invalid==0)then
                                                        
                                            call sub_solvehh_sav(grid_sav(xc),.true.,flag_invalid,dc_in,0)
                                        else
                                            call sub_solvehh_sav(grid_sav(xc),.false.,flag_invalid,dc_in,0)
                                        endif
                                            
                                        ev_tp1_vec_2dim(xc,pc) = ev_tp1
                                        ev_tp1_vec_2dim_nokid(xc,pc) = ev_tp1_nokid
                                        
                                        evp_coh_tp1_vec_2dim(xc,pc) = evp_coh_tp1
                                            
                                    enddo
                                                
                                           
                                endif
                                       
                                ! call upper envelope
                                temp_vec1=0.0_dp
                                temp_vec2=0.0_dp
                                call sub_secondary_env(pol%v_nokid_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),temp_vec1,temp_vec2, & 
                                    grid%coh_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),x_ref,intersectionsx,intersectionsy,num_sec,top)
                                call sub_secondary_env(pol%v_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%cons_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%inv_vp_x_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                    grid%coh_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),x_ref,intersectionsx,intersectionsy,num_sec,top)
                                call sub_secondary_env(pol%v_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                    grid%coh_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),x_ref,intersectionsx,intersectionsy,num_sec,top)
                        
                                ! update coh grid
                                grid%coh_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=x_ref
                        
                                ! update asset grid (recompute it, not interpolate) 
                                
                                do xc=1,nx
                                    
                                    call sub_wagepens_cpl()                                    
                                                
                                    grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = f_grid_ass_cpl(grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),bq(sc1,jc)+bq(sc2,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                    !grid%assmin_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = f_grid_ass_cpl(grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),bq(sc1,jc)+bq(sc2,jc),R,net_wage*demo%grid_epsi(1),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                    !grid%assmax_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)= f_grid_ass_cpl(grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),bq(sc1,jc)+bq(sc2,jc),R,net_wage*demo%grid_epsi(nw),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                                                     
     
                                    pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)= evp_coh_tp1_vec_2dim(xc,pc)
                                    
                                    !if (opt_ext_compute==1)then
                                        
                                        pol%vd_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        pol%consd_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        pol%lab1d_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        pol%lab2d_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        grid%cohd_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        
                                   ! endif
                                    
                                    
                                   ! if (dc_in_max>1)then
                                    v_dc_in(dc_in,xc) = pol%vd_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                    coh_dc_in(dc_in,xc) = grid%cohd_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                    cons_dc_in(dc_in,xc) = pol%consd_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                    lab1_dc_in(dc_in,xc) = pol%lab1d_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                    lab2_dc_in(dc_in,xc) = pol%lab2d_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                enddo ! enddo xc
                                enddo
                                
                                if (dc_in_max>1)then
                                    x_min = minval(coh_dc_in)
                                    x_max = maxval(coh_dc_in)
                                    x_grid = makegrid(x_min,x_max,nx,3.0_dp)
                                    
                                    do xc=1,nx
                                        do dc_in = 1,4
                                            v_dc_in_intp(dc_in) = func_intp_mod(coh_dc_in(dc_in,:),v_dc_in(dc_in,:),x_grid(xc),vals,inds)
                                            cons_dc_in_intp(dc_in,xc) = func_intp(coh_dc_in(dc_in,:),cons_dc_in(dc_in,:),x_grid(xc),vals,inds,.false.,.false.)
                                            lab1_dc_in_intp(dc_in,xc) = func_intp(coh_dc_in(dc_in,:),lab1_dc_in(dc_in,:),x_grid(xc),vals,inds,.false.,.false.)
                                            lab2_dc_in_intp(dc_in,xc) = func_intp(coh_dc_in(dc_in,:),lab2_dc_in(dc_in,:),x_grid(xc),vals,inds,.false.,.false.)
                                            sav_dc_in_intp(dc_in,xc) = func_sav_cpl(x_grid(xc),net_wage,wage_totax,lab1_dc_in_intp(dc_in,xc),lab2_dc_in_intp(dc_in,xc),cons_dc_in_intp(dc_in,xc), & 
                                            0.0_dp,demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below)
                                            cont_val = func_intp(grid_sav2dim(:,pc),ev_tp1_vec_2dim(:,pc),sav_dc_in_intp(dc_in,xc), vals,inds,.false.,.false.)
                                            util=f_util_cpl(cons_dc_in_intp(dc_in,xc),lab1_dc_in_intp(dc_in,xc),lab2_dc_in_intp(dc_in,xc),ec,dc,sc1,sc2,gen_id)
                                            v_dc_in_intp(dc_in) =f_vfun(util,cont_val,betta_til,1.0_dp)                                          
! if (sav_dc_in_intp(dc_in,xc) < grid%amin_age(1,sc1,gen_id,jc)) v_dc_in_intp(dc_in)  = -9000.0_dp
                                           ! if (dc_in>1 .and. inds(1)<=np1_aux) v_dc_in_intp(dc_in) = - 9000.0_dp
                                        enddo
                                
                                        if ( (v_dc_in_intp(2) - v_dc_in_intp(1)) > 0.0001_dp .and. (v_dc_in_intp(2) - v_dc_in_intp(3)) > 0.0001_dp .and. (v_dc_in_intp(2) - v_dc_in_intp(4)) > 0.0001_dp ) then
                                            dc_str(xc) = 2
                                        elseif ((v_dc_in_intp(3) - v_dc_in_intp(1)) > 0.0001_dp .and. (v_dc_in_intp(3) - v_dc_in_intp(2)) > 0.0001_dp .and. (v_dc_in_intp(3) - v_dc_in_intp(4)) > 0.0001_dp  ) then
                                            dc_str(xc) = 3
                                        elseif ((v_dc_in_intp(4) - v_dc_in_intp(1)) > 0.0001_dp .and. (v_dc_in_intp(4) - v_dc_in_intp(3)) > 0.0001_dp .and. (v_dc_in_intp(4) - v_dc_in_intp(2)) > 0.0001_dp ) then    
                                            dc_str(xc) = 4
                                        else
                                            dc_str(xc) = 1
                                        endif
                                
                                        dc_str(xc) = maxloc(v_dc_in_intp,1) 
!print*, dc_str(xc)
!if (dc_str(xc) > 1) print*, "dcstr", dc_str(xc), v_dc_in_intp

                                        if (opt_ext_compute==1)then
                                            pol%dc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =real(dc_str(xc))
                                            grid%coh_old_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        else
                                            dc_str(xc) = nint(func_intp(grid%coh_old_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                                pol%dc_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),x_grid(xc),vals,inds,.false.,.false.) )
                                            if (x_grid(xc) <grid%coh_cpl(dc,1,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) ) dc_str(xc) = 1
                                        endif
                                
                                        !dc_str(xc) = maxloc(v_dc_in_intp,1)
!                                       if (dc_str(xc) > 1) print*, "dcstr", dc_str(xc), v_dc_in_intp
                                        grid_sav(xc) = func_sav_cpl(x_grid(xc),net_wage,wage_totax,lab1_dc_in_intp(dc_str(xc),xc),lab2_dc_in_intp(dc_str(xc),xc),cons_dc_in_intp(dc_str(xc),xc), & 
                                            0.0_dp,demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below)
                                    enddo
                            
                                    grid%sav_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=grid_sav(:)
                                    grid_sav2dim(:,pc) = grid_sav
                            
                                    flag_invalid = 0
                                
                                    do xc=1,nx
                                                     
                                        call sub_solvehh_sav(grid_sav(xc),.false.,flag_invalid,dc_str(xc),1 )
                                        
                                        ev_tp1_vec_2dim(xc,pc) = ev_tp1
                                        ev_tp1_vec_2dim_nokid(xc,pc) = ev_tp1_nokid
                                        
                                        evp_coh_tp1_vec_2dim(xc,pc) = evp_coh_tp1
                                          
                                    enddo
                                            
                           
                            
                                endif
                            
                            
                            
                            enddo ! enddo pc
                                    
                                        
                            if (gen_id==1 .and. jc==jt   .and. opt_ivt==1)then
                                                
                                opt_solve_parent=1
                                        
                                ! 2 dim objects from STEP ONE
                                coh_endo_2dim = grid%coh_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                cons_endo_2dim = pol%cons_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) 
                                lab1_endo_2dim = pol%lab1_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) 
                                lab1eff_endo_2dim = pol%lab1_eff_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                lab2_endo_2dim = pol%lab2_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) 
                                lab2eff_endo_2dim = pol%lab2_eff_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        
                                do pc=1,pc_max(gen_id,jc)
                                    
                                    coh_exo_min = coh_endo_2dim(1,pc)
                                    coh_exo_max = coh_endo_2dim(nx,pc)
                                    
                                    coh_exo =makegrid(coh_exo_min,coh_exo_max,nx,3.0_dp)
                                    
                                    !grid%coh(dc,:,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = coh_exo
                                    
                                    do xc=1,nx 
                                            
                                        ass_exo(xc) = f_grid_ass_cpl(coh_exo(xc),bq(sc1,jc)+bq(sc1,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                            
                                        max_b= max(0.0_dp, coh_exo(xc) - minval(grid_sav2dim) -  net_wage(1)*(biggam-pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) ) -net_wage(2)*(biggam-pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) )  &
                                            - fun_hsv_new(wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) +wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) & 
                                            ,wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),1,1,sc1,ec,dc,ic,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,ind_below )  & 
                                            - epsi )
                                         b_grid = makegrid(0.0_dp,max_b,nb,1.0_dp)
                                        
                                        do bc = 1,nb
                                            
                                            val_bc(bc) = f_vfun_ivt(b_grid(bc))
                                            
                                        enddo 
                                            
                                        call sub_max_vfun(b_grid,val_bc,pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) ,pol%b_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc),.true.)
                                            
                                        ! after transfer coh
                                        mcoh = coh_exo(xc) -pol%b_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc) * demo%numchild_gsj(2,sc2,jc,tc)
                                            
                                        grid%coh_exo_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = coh_exo(xc) 
                                            
                                        pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)= f_vfun_ivt(pol%b_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc))
                                        pol%v_nokid_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = vfun_nokid
                                            
                                        ! consumption by interpolati2 on (after transfer) consumption policy function
                                        !pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = func_intp(coh_endo_2dim(:,pc),cons_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        
                                          
                                        
                                        cons_temp  = func_intp(coh_endo_2dim(:,pc),cons_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        
                                        pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = cons_temp
                                        
                                        pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=func_intp(coh_endo_2dim(:,pc),lab1_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=func_intp(coh_endo_2dim(:,pc),lab2_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=func_intp(coh_endo_2dim(:,pc),lab1eff_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=func_intp(coh_endo_2dim(:,pc),lab2eff_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        
                                        !if (pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) * sum(net_wage) <=agg%inc_thrs(tc) ) then
                                        !    ind_below = 1
                                        !else
                                        !    ind_below= 0
                                        !endif
                                        
                                        ! savi2s from budget constraint
                                        grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = & !func_intp(coh_endo_2dim(:,pc),grid_sav2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                            func_sav_cpl(coh_exo(xc),net_wage,wage_totax,pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%b_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc), demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below )
                                            
                                        !print*, "test", grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc), func_intp(x_ref,grid_sav,mcoh,vals,inds,.false.,.false.)
                                        !pause
                                            
                                        ! check borrowi2 constraint
                                        bc_sav = grid_sav2dim(1,pc) 
                                        
                                        
                                        
                                            
                                        if (grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) < bc_sav)then
                                            cons_temp = coh_exo(xc) - bc_sav -  net_wage(1)*(biggam-pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)) &
                                                - net_wage(2)*(biggam-pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)) &
                                                - fun_hsv_new(wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) +wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)  ,wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),  & 
                                                pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),  &  
                                                pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) ,pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) , & 
                                                1,1,sc1,ec,dc,ic,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,ind_below )  & 
                                                - pol%b_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc) *  demo%numchild_gsj(2,sc2,jc,tc)
                                            
                                            pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = cons_temp/(1.0_dp + agg%tau_c(tc))
                                            
                                            grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = bc_sav
                                            !print*, "yes"
                                        endif
                                            
                                        ! assets, given coh
                                        grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)  = f_grid_ass_cpl(coh_exo(xc),bq(sc1,jc)+bq(sc2,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                        !grid%assmin(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(1),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                        !grid%assmax(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(nw),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                                                               
    
                                        ! rhs of euler equation 
                                        pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = func_intp(coh_endo_2dim(:,pc),evp_coh_tp1_vec_2dim(:,pc)**(-1.0_dp),mcoh,vals,inds,.false.,.false.)
                                        ! reinvert
                                        pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)  = pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) **(-1.0_dp)
                                            
                                    !    ! check of consumption euler eq
                                    !     if (grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) > bc_sav )then
                                    !        if (abs(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp) - betta*f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) *(1.0_dp + agg%tau_c(tc))) > epsi)then
                                    !            print*, "solve errrorrrr",jc, hc,grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                    !
                                    !             pause
                                    !        else
                                    !!            print*, "no errorrrr",jc
                                    !!            pause
                                    !        endif
                                    !    endif 
                                            
                                        ! inverse derivative of vfun w.r.t. coh
                                        call sub_muc(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,mu_c)
                                        ! inverse of derivative of value function w.r.t. coh 
                                        pol%inv_vp_x_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=f_inv_vp_coh(mu_c)    
                                            
                                        call sub_checkbudget_parent_cpl(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%b_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            bq(sc1,jc)+ bq(sc2,jc),net_wage,wage_totax,ben_p,R,ic,ec,dc,.false.,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc))  
                                        
                                        
                                        call sub_incsav_cpl(grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            R,gr_wage,gammafe(sc1,kc1),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc, & 
                                            pol%grwage_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &  
                                            pol%grinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%netinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%netsav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%capinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%constaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%penstaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),jc,tc,1,ben_u, & 
                                            demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),tau_k, & 
                                            pol%hsvtmp1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%hsvtmp2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%net_trr_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc))
                                          
                                    enddo
                                                
                                    !print*, "finished"
                                    !pause
                                                
                                    if (opt_chk_ivt_cpl .and. sc1==3)then
                                        
                                        print*, "bpol", pol%b_cpl(dc,1:nx,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,tc)
                                        pause
                                    
                                        !call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%cons(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,'cons','cons')
                                        !call execplot()
                                        !call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%b(dc,1:nx,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc),'b','b')
                                        !call execplot()
                                                
                                    endif
                                enddo
                                        
                                grid%coh_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%coh_exo_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)       
                                        
                            elseif ((gen_id==1 .and. opt_inv==1 ).and. (jc<jt .and. jc>=jf))then
                                        
                                ! 2 dim objects from STEP ONE
                                coh_endo_2dim = grid%coh_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                cons_endo_2dim = pol%cons_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                lab1_endo_2dim = pol%lab1_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                lab1eff_endo_2dim = pol%lab1_eff_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) 
                                lab2_endo_2dim = pol%lab2_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                lab2eff_endo_2dim = pol%lab2_eff_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) 
                                        
                                ! STEP TWO
                                opt_solve_parent=2 
                                        
                                do pc=1,pc_max(gen_id,jc) ! currrent period HK
                                         
                                    ! span exogenous coh grid
                                    coh_exo_min =minval(coh_endo_2dim(1,:))
                                    coh_exo_max = maxval(coh_endo_2dim(nx,:))
                                    
                                    coh_exo = makegrid(coh_exo_min,coh_exo_max,nx,3.0_dp)
                                    
                                    
                                    grid%coh_exo_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = coh_exo
                                           
                                    do xc=1,nx
                                        
                                        ! exogenous assets
                                        ass_exo(xc) = f_grid_ass_cpl(coh_exo(xc),bq(sc1,jc)+bq(sc2,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                            
                                        flg_all_inf = .true.
                                        
                                        ! min and max mon investment feasible 
                                        min_m = 0.0_dp
                                        grinc = net_wage(1) * pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) + net_wage(2) * pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        if (grinc<=agg%inc_thrs(tc) ) then
                                            ind_below=1
                                        else
                                            ind_below=0
                                        endif
                                        
                                        max_m = coh_exo(xc) - minval(grid_sav2dim) -  net_wage(1)*(biggam-pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) ) -net_wage(2)*(biggam-pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) )  &
                                            - fun_hsv_new(wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) +wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) & 
                                            ,wage_totax(1)*pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),wage_totax(2)*pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),1,1,sc1,ec,dc,ic,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,ind_below )  & 
                                            - epsi

                                        ! loop over possible time inv choices (on grid, then go between points)
                                        do tinv_tc = 1,ntinv
                                                
                                            val_tc(tinv_tc) = f_vfun_tinv(grid%tinv_grid(tinv_tc))
                                                    
                                            if ( val_tc(tinv_tc) > -9000.0_dp )then
                                                flg_all_inf = .false.
                                            endif
                                                    
                                        enddo
                                              
                                        if (flg_all_inf==.true. .or. opt_invconst==1)then ! no positive investment choices are feasible [for completeness, should not have any positive weight here]
                                            pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)= 0.0_dp
                                            pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)= 0.0_dp
                                            pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)= 0.0_dp
                                            mcoh = coh_exo(xc)
                                            ! next period HK 
                                            hp1_loc = f_hh_tp1(grid%hk_grid(pc),gamma_s(sc1),pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                                pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) +pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)  ,jc-jf+1,inv_ces) 
                                            cons_temp  = f_hyb2d_inter(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,cons_endo_2dim)
                                            ! reinvert
                                           
                                            pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)  =cons_temp 
                                            muc = pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)**(-1.0_dp)
                                        else                                                                                                      
                                            ! pick maximum                                            
                                            call sub_max_vfun(grid%tinv_grid,val_tc,pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                                pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),.true.)                                            
                                            
                                            call sub_solvemoney_cpl(grid%hk_grid(pc),coh_exo(xc),net_wage,wage_totax,pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                                pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                                muc,pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                                pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),& 
                                                demo%numchild_gsj(2,sc1,jc,tc),mcoh,hp1,cons_temp,grid%hk_grid,cons_endo_2dim,lab1_endo_2dim,lab1eff_endo_2dim,lab2_endo_2dim,lab2eff_endo_2dim,coh_endo_2dim,evp_coh_tp1_vec_2dim,grid_sav2dim,f_aftertaxR(1.0_dp + agg%ret(tc),tau_k),ec,dc,ic,jc,tc,min_m,max_m,flag_conv)  
                                            pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = cons_temp
                                            if (flag_conv==0) print*, "no money conv cpl",jc,xc,pc
                                        endif
                                         
                                        ! after inv coh
                                        mcoh = coh_exo(xc) -pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) * demo%numchild_gsj(2,sc2,jc,tc)
                                        
                                        ! next period HK 
                                        hp1_loc = f_hh_tp1(grid%hk_grid(pc),gamma_s(sc1),pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) + pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),jc-jf+1,inv_ces) 
                                        
                                        pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,cons_endo_2dim ,vals2x,inds2x,valsh,indsh) 
                                        
                                        pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,lab1_endo_2dim ,vals2x,inds2x,valsh,indsh)
                                        pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,lab1eff_endo_2dim ,vals2x,inds2x,valsh,indsh)
            
                                        pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,lab2_endo_2dim ,vals2x,inds2x,valsh,indsh)
                                        pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,lab2eff_endo_2dim ,vals2x,inds2x,valsh,indsh)
            
                                        
                                        if (isnan(hp1_loc))then
            
                                            print*,"hp1 nan", hp1_loc, grid%hk_grid(pc),1.0_dp,pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                                pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                            pause
                                        endif
            
                                        if (pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)* net_wage(1) + pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)* net_wage(2) < agg%inc_thrs(tc) ) then
                                            ind_below = 1
                                        else
                                            ind_below = 0
                                        endif
                                        
                                        ! savi2s from the budget constraint
                                        grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = & ! f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,grid_sav2dim(:,:) ,vals2x,inds2x,valsh,indsh) 
                                            func_sav_cpl(coh_exo(xc),net_wage,wage_totax,pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below )
                                        
                                        !print*, "comp", grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,grid_sav2dim(:,:) ,vals2x,inds2x,valsh,indsh),pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        !pause
                                        
                                        ! check borrowi2 constraint                                        
                                        bc_sav = func_intp(grid%hk_grid,grid_sav2dim(1,:),hp1_loc,vals,inds,.false.,.false.)
                                        
                                        
                                        !! intratemporal foc error
                                        !call sub_fochk_check(pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) , & 
                                        !    pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), demo%numchild_gsj(gc,sc,jc,tc),gc,jc,dist_foc)
                                        !if (abs(dist_foc)>0.0001_dp .and. pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)>0.0_dp .and. pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)>0.0_dp)then
                                        !    print*, "beeep", dist_foc 
                                        !endif        
                                        
                                        !grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) =  & !f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,grid_sav2dim(:,:) ,vals2x,inds2x,valsh,indsh) 
                                        !    func_sav(coh_exo(xc),net_wage,pol%lab(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                        !    pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) )
                                            
                                        
                                        pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = f_vfun_tinv_eval(pol%t1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%t2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%v_nokid_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc))
                                        
                                        
                                        ! assets, given coh
                                        grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = f_grid_ass_cpl(coh_exo(xc),bq(sc1,jc) + bq(sc1,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                        !grid%assmin(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(1),ben_p,agg%tau_k(tc),ec,dc,jc,1)                                        
                                        !grid%assmax(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(nw),ben_p,agg%tau_k(tc),ec,dc,jc,1)                                        


                                        ! save rhs of euler equation, for computi2 euler errors later
                                        pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = f_hyb2d_inter_mod(mcoh ,hp1_loc, & 
                                            coh_endo_2dim,grid%hk_grid,evp_coh_tp1_vec_2dim**(-1.0_dp),vals2x,inds2x,valsh,indsh) 
                                        pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) =  pol%evtp1_coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)**(-1.0_dp)
                                        
                                        !print*, ev_tp1_vec_2dim_trans(10,:),trans_2dim
                                        !pause
                                        
                                        !if (abs(grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) - bc_sav)>epsi )then
                                        !    if (abs(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp) - betta*f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * &
                                        !        pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) *(1.0_dp + tau_c) ) > epsi)then
                                        !        print*, "solve errrorrrr",jc, pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp),  betta* f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) *pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), hp1_loc, & 
                                        !        pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) , bc_sav
                                        !
                                        !        ! pause
                                        !    else
                                        !            !           print*, "no errorrrr",jc
                                        !        !            pause
                                        !    endif
                                        !endif
                                        
                                        
                                        ! inverse derivative of vfun w.r.t. coh
                                        call sub_muc(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,mu_c)
                                        ! inverse of derivative of value function w.r.t. coh 
                                        pol%inv_vp_x_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=f_inv_vp_coh(mu_c)    
                                            
                                        call sub_checkbudget_parent_cpl(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%m_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            grid%sav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            bq(sc1,jc)+bq(sc2,jc),net_wage,wage_totax,ben_p,R,ic,ec,dc,.false.,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc))  
                                            
                                        call sub_incsav_cpl(grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),0.0_dp,pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),R,gr_wage,gammafe(sc1,kc1),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc, & 
                                            pol%grwage_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%grinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%netinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%netsav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
                                            pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%capinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%constaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
                                            pol%penstaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),jc,tc,1,ben_u, & 
                                            demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),tau_k,pol%hsvtmp1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),& 
                                            pol%hsvtmp2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%net_trr_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc))
                                            
                                    enddo
                                           
                                    if (opt_chk_inv_cpl .and. sc1==2)then
                                        print*, "pol m", pol%m_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),dc,pc
                                        pause
                                        print*, "pol t", pol%t1_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        pause
                                        print*, "pol t2", pol%t2_cpl(dc,:,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                                        pause
                                            
                                        !call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%cons(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'cons','cons')
                                        !call execplot()
                                        !call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'m','m')
                                        !call execplot()
                                        !call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%t1(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'t1','t1')
                                        !call execplot()
                                    endif
                                        
                                enddo
                                    
                                        
                                ! END TBC
                                grid%coh_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = grid%coh_exo_cpl(dc,:,:,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)       
                            endif
                                        
                            
                          !  endif
                                    
                                
                        
                        enddo ! enddo dc
                   
                        deallocate(dc_vec)   
                          
                        ! if (opt_chk .and. (jc<(jr(tc)-1)) ) then
                        if ( opt_chk .and. gen_id==1 .and. jc>jt .and. sc1==1 ) then
                        ! if (opt_chk) then
                            do gc=1,2 
                                call sub_wage(gr_wage(gc),net_wage(gc),wage_totax(gc),agg%wage_s(sc(gc),tc),demo%ageprod(gc,jc,sc(gc)),demo%grid_y(yc(gc)),0.0_dp,agg%tau_p(tc),sc(gc),kc(gc),dc,ec,jc,gen_id,agg%gammah_s(sc(gc),tc),agg%abgrad_s(:,sc(gc)),agg%hk_cutoff(:,tc))
                            enddo
                            
                            nx0=nx   
                            !print*, 'counters: ', yc(gc),ec,ic,jc,kc(gc),sc(gc), net_wage(1)
                            pc=1
                            if (jc>=jr(1) )then
                                dc= nd
                               ! ec = 2
                            else
                                dc = 1
                               ! ec = 1
                            endif
                            
                            dc=1! for plotti2
                            pc=1
                    
                            print*, 'assets: '
                            print*, grid%ass_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                            print*, 'coh: '
                            print*, grid%coh_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                            print*, 'cons: '
                            print*, pol%cons_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                            print*, 'lab: '
                            print*, pol%lab1_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                            print*, 'sav: '
                            print*, grid%sav_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)
                            !pause
                                    
                            !call plot_xy(grid%coh_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%cons_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),'cons','cons')
                            !call execplot()
                            !        
                            !call plot_xy(grid%coh_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%leis1_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),'leis','leis')
                            !call execplot()
                            !        
                            !call plot_xy(grid%coh_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%inv_vp_x_cpl(dc,1:nx0,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),'vp-coh','vp-coh')
                            !call execplot()
                        endif
                        
                    end do  ! end do ec
                end do  ! end do yc
                enddo
            enddo
           
        enddo
    enddo
        
                    
endif   ! end if jc


contains

    ! -------------------------------------------------------------------
    subroutine sub_wagepens_cpl()

    implicit none
    
    ! compute wages and pension benefits today 
    gr_wage=0.0_dp
    net_wage=0.0_dp                                                
    ben_p=0.0_dp
                            
                    
    if (ec==1) then
       
            
            do gc = 1,2
                call sub_wage(gr_wage(gc),net_wage(gc),wage_totax(gc),agg%wage_s(sc(gc),tc),demo%ageprod(gc,jc,sc(gc)),demo%grid_y(yc(gc)),grid%hk_grid(1),agg%tau_p(tc),sc(gc),kc(gc),dc,ec,jc,gen_id,agg%gammah_s(sc(gc),tc),agg%abgrad_s(:,sc(gc)),agg%hk_cutoff(:,tc))
             
            
                gr_wage(gc) = gr_wage(gc) * demo%grid_epsi(wc(gc))
                net_wage(gc) = net_wage(gc) * demo%grid_epsi(wc(gc))
                wage_totax(gc) = wage_totax(gc) * demo%grid_epsi(wc(gc))

            enddo
                 
                              
    else 
        do gc = 1,2
            av_prod = demo%ageprod(gc,jr(tc)-1,sc(gc) )   
            call sub_pens_new(gr_ben_p(gc),agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc(gc),tc),av_prod , & 
                demo%grid_y(yc(gc)),0.0_dp,jc,gc,dc,ec,tc,sc(gc),kc(gc),yc(gc),nd_cpl)
            ben_p(gc) = fun_netpens(gr_ben_p(gc),0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
        enddo
        
    endif
    
    end subroutine sub_wagepens_cpl
    ! -------------------------------------------------------------------


    ! -------------------------------------------------------------------
    subroutine sub_max_vfun(grid_b,grid_w,w_max,b_max,opt_acc)
    ! pick max of grid_w, evaluated on grid_b

    implicit none

    real(dp),intent(in)::grid_b(:),grid_w(:)
    real(dp),intent(out)::w_max,b_max
    integer::bc
    real(dp)::ax,bx,cx,stp,w_alt,sav_check(nx), v1, v2, v3, bx2, stp1, stp2,cons,sav
    integer::ns, bc2,ind_bc
    real(dp),parameter::stp_min=1.0e-04
    integer,parameter:: case_meth=2
    logical,intent(in):: opt_acc ! for vfi
    logical :: fflag

    ! pick and index maximum
    w_max=maxval(grid_w)
    bc=indmax(grid_w,w_max)

    b_max=grid_b(bc)
    
    if (not(opt_acc) ) then
        return
    else    
        ! proceed with solver in interval:
        ns=size(grid_w,1)
    
        if (bc==ns) then
            ! check if there is an increase of objective for marginal cha2e:
            bx=grid_b(ns)-stp_min
            if (opt_solve_parent==1)then
                w_alt=f_vfun_ivt(bx)
            elseif (opt_solve_parent==2)then
                w_alt=f_vfun_tinv(bx)
            endif
            
            if (w_alt<w_max) return            
            
            ! check if v is increasi2 between grid points that is if grid was too coarse
            stp=(grid_b(ns)-grid_b(ns-1))/2.0_dp  
            bx=grid_b(ns)-stp
            ax=grid_b(ns)
            cx=grid_b(ns-1)
        
        elseif (bc==1) then
            ! check if there is an increase of objective for marginal cha2e:
            bx=grid_b(1)+stp_min
            if (opt_solve_parent==1)then
	            w_alt=f_vfun_ivt(bx)     
            elseif (opt_solve_parent==2)then
                w_alt=f_vfun_tinv(bx)
            endif
            
            if (w_alt<w_max) return
            
            stp=(grid_b(2)-grid_b(1))/2.0  
            bx=grid_b(1)+stp
            ax=grid_b(2)
            cx=grid_b(1)
        else
            bx=grid_b(bc)
            ax=grid_b(bc+1)
            cx=grid_b(bc-1)   
        endif
    
        ! compute solution in between grid points
        stp1=bx-ax
        stp2=cx-bx
 
        if (case_meth==1) then
            w_max=golden(ax,bx,cx,func_minw,tolf,b_max)
        elseif (case_meth==2) then
            w_max=brent(ax,bx,cx,func_minw,tolf,b_max,fflag)
        endif
    
        w_max=-w_max
        
    endif
       
    end subroutine sub_max_vfun
    ! -------------------------------------------------------------------
    
    ! -------------------------------------------------------------------
    function func_minw(b)
    
    implicit none
    
    real(dp)::func_minw
    real(dp),intent(in)::b
    real(dp):: cons,sav
    integer:: ind_bc
    
    if (opt_solve_parent==1)then
    
        func_minw=-f_vfun_ivt(b)
    elseif (opt_solve_parent==2)then
        func_minw=-f_vfun_tinv(b)
    endif
    
    
    end function func_minw
    ! -------------------------------------------------------------------


    ! -------------------------------------------------------------------
    function f_vfun_ivt(bx)
    ! compute for b on as well as outside the b-grid

    USE CSHER_INT
    USE UMACH_INT
    USE CSVAL_INT
    use pchip_module
    
    implicit none

    real(dp):: f_vfun_ivt
    real(dp),intent(in):: bx
    real(dp):: mcoh,cons_temp,sav_temp,util_temp, ev_tp1_temp,prob_edu_kid(ns),ev_kid(ns),v_kid,v_kid_dc(ndl),logsum_kid,prob_kid(ndl),logsum_kid_educ,prob_kid_educ(ns),coh_kid,net_wage_kid,wage_totax_kid,gr_wage_kid,bnet,val_sc(ns)
    integer:: sc_kid_min,sc_kid_max,dc_kid,sc_kid,kc_kid,yc_kid,xc_loc,xc_sw,sc_loc,ierr,wc_kid, ind_below, gc_kid
    real(dp):: ass_kid,ass_kid_netfee, sav_temp2,high_sh(nx),coh_loc,vkid_s_intp(ns),prob_s_loc(ns),logsum_loc, dist_test,csbreak_sh(nx),cscoeff_sh(4,nx),v_lin,xe(1),fe(1),prob_fin(2),prob_fin_hs(2),val_col,prob_col(2),ev_kid_choice(2),probfe(nk),lab1_eff,lab1,lab2_eff,lab2
    logical:: skip
    
    mcoh = coh_exo(xc) -bx * demo%numchild_gsj(2,sc2,jc,tc)
    
    lab1  = func_intp(coh_endo_2dim(:,pc),lab1_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    lab1_eff  = func_intp(coh_endo_2dim(:,pc),lab1eff_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    lab2  = func_intp(coh_endo_2dim(:,pc),lab2_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    lab2_eff  = func_intp(coh_endo_2dim(:,pc),lab2eff_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    
    pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc) = lab1
    pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)  = lab1_eff
    
    if (lab1 * net_wage(1) + lab2 * net_wage(2) <= agg%inc_thrs(tc) ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
    
    cons_temp  = func_intp(coh_endo_2dim(:,pc),cons_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    !cons_temp = cons_temp**(-1.0_dp)
    !cons_temp =( cons_temp *  f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * betta*demo%sr(jc,tc)   )**(-1.0_dp)
    sav_temp =func_sav_cpl(coh_exo(xc),net_wage,wage_totax,lab1,lab2,cons_temp,bx,demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below) !,agg%inc_thrs(tc) ) 
    
    bc_sav = grid_sav2dim(1,pc) 
    
    if (sav_temp < bc_sav)then
        cons_temp = coh_exo(xc) - bc_sav -  net_wage(1)*(biggam-lab1)-  net_wage(2)*(biggam-lab2) &
            - fun_hsv_new(wage_totax(1)*lab1 + wage_totax(2)*lab2,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,sc1,ec,dc,ic,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,ind_below )  &  !,avearn_base,ind_below
        - bx *  demo%numchild_gsj(2,sc2,jc,tc)
        
        sav_temp = bc_sav
    endif
  
                                          
    if (mcoh < coh_endo_2dim(1,pc) .or. sav_temp < grid_sav2dim(1,pc) )then
        f_vfun_ivt = -90000.0_dp
    else
         
        dist_test = abs(sav_temp - func_intp(coh_endo_2dim(:,pc),grid_sav2dim(:,pc),mcoh,vals,inds,.false.,.false.) ) 
        
        ass_kid_netfee =  (bx )/f_aftertaxR(R,tau_k)
        sc_kid_max  = ns
            
        util_temp = f_util_cpl(cons_temp,lab1_eff,lab2_eff,ec,dc,sc1,sc2,gen_id)
                                            
        ev_tp1_temp = func_intp(grid_sav2dim(:,pc),ev_tp1_vec_2dim(:,pc),sav_temp,vals,inds,.false.,.false.)
       
        f_vfun_ivt = f_vfun(util_temp,ev_tp1_temp,betta_til,demo%sr(jc,tc) )
        
        ev_kid = 0.0_dp
            
        do gc_kid = 1,2
            
            do sc_kid=1,sc_kid_max
                
                do yc_kid=1,ny
                    do wc_kid = 1,nw
                    
                        if (nk>1)then
                            probfe(nk) = f_probfe(grid%hk_grid(pc),1,agg%gammah_s(sc_kid,tc))
                            probfe(1) = 1.0_dp -probfe(nk) 
                        else
                            probfe(1) = 1.0_dp
                        endif 
                    
                        do kc_kid=1,nk
                        
                            val_sc = 0.0_dp 
                      
                            ! transfer net 
                            bnet = bx 
                        
                            do dc_kid=1,ndl
                            
                                ! wage of kid: ALL kids work for no wages
                                call sub_wage(gr_wage_kid,net_wage_kid,wage_totax_kid,agg%wage_s(1,tc),demo%ageprod(gc_kid,js,1),demo%grid_y(yc_kid),grid%hk_grid(pc),agg%tau_p(tc),1,kc_kid,dc_kid,1,js,2,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                            
                                gr_wage_kid = gr_wage_kid * demo%grid_epsi(wc_kid)
                                net_wage_kid = net_wage_kid * demo%grid_epsi(wc_kid)
                                wage_totax_kid = wage_totax_kid * demo%grid_epsi(wc_kid)
                            
                                ! kids coh
                                coh_kid = bnet + bq(sc_kid,js+1) + net_wage_kid
                  
                                
                                if ( coh_kid < grid%coh_child(dc_kid,1,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc) )then
                                    v_kid_dc(dc_kid) = -90000.0_dp
                                    v_lin = v_kid_dc(dc_kid)
                                else
                                    
                                
                                    v_kid_dc(dc_kid) = func_intp_mod(grid%coh_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc),pol%v_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc),coh_kid  ,vals,inds) !,.false.,.false.)
                                
                                    v_lin = v_kid_dc(dc_kid)
                                
                                    if (opt_pchip==1)then
                                
                                        if (grid%coh_child(dc_kid,np1_aux-1,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc) == grid%coh_child(dc_kid,np1_aux-2,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc) ) then
                                        
                                            !call d_csher(grid%coh_child(dc,np1_aux:nx,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),pol%v_child(dc,np1_aux:nx,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            !    pol%cons_child(dc,np1_aux:nx,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),grid%break_sh_child_sh(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            !    grid%coeff_sh_child_sh(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc))
                                            !    
                                            !v_kid_dc(dc_kid) =  d_csval(coh_kid,grid%break_sh_child_sh(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            !    grid%coeff_sh_child_sh(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc))
                                            !
                                            !print*, "so1", v_lin, v_kid_dc(dc_kid)
                                    
                                            call dpchim (nx-np1_aux+1, grid%coh_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                pol%v_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), pol%vder_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                1, ierr)
                                    
                                            xe(1) = coh_kid
                                            skip = .true.
                                    
                                            call dpchfe (nx-np1_aux+1, grid%coh_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                pol%v_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), pol%vder_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                1, skip, 1, xe, fe, ierr)
                                        
                                            v_kid_dc(dc_kid) = fe(1)
                                    
                                            !print*, "so2", v_lin, v_kid_dc(dc_kid)
                                            !pause
                                        else
                                        
                                                                            
                                            !call d_csher(grid%coh_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),pol%v_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            !    pol%cons_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),grid%break_sh_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            !    grid%coeff_sh_child(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc))
                                            !    
                                            !csbreak_sh = grid%break_sh_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc)
                                            !cscoeff_sh = grid%coeff_sh_child(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc)
                                            !
                                            !v_kid_dc(dc_kid) =  d_csval(coh_kid,csbreak_sh,cscoeff_sh)
                                    
                                            call dpchim (nx, grid%coh_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                pol%v_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), pol%vder_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                1, ierr)
                                    
                                            xe(1) = coh_kid
                                            skip = .true.
                                    
                                            call dpchfe (nx, grid%coh_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                pol%v_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), pol%vder_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,max(sc1,sc2),js,tc), & 
                                                1, skip, 1, xe, fe, ierr)
                                        
                                            v_kid_dc(dc_kid) = fe(1)
                                        
                                    endif
                                    
                                    
                                endif
                                
                                
                            
                                !if (opt_pchip==1)then
                                !    if (v_kid_dc(dc_kid) < vals(1) .or. v_kid_dc(dc_kid) > vals(2) ) v_kid_dc(dc_kid) = v_lin
                                !endif
                            
                            endif
                            
                            
                        enddo
                        
                        call sub_logsumprob(v_kid_dc,logsum_kid,prob_kid,ndl,sigma_emp)
                        
                        val_sc(sc_kid)  =logsum_kid 
                         
                        
                        ev_kid(sc_kid) = ev_kid(sc_kid) + val_sc(sc_kid) *  demo%pini(yc_kid)  * demo%prob_epsi(wc_kid) * probfe(kc_kid) * 2**(-1.0_dp)
                    enddo
                enddo
            enddo 
            
        enddo
            
    enddo  
           
        
if (opt_corr==1)then
    prob_kid_educ = 0.0_dp 
    ! first DROPOUT DECISION, with taste shock
    call sub_logsumprob(ev_kid(ns-1:ns),logsum_kid_educ,prob_col,2,sigma_emp*0.0_dp)
    ! now, to those willing to graduate, apply lottery
    prob_fin(1) = f_probfin(grid%hk_grid(pc)) ! CL completion prob
    prob_fin(2) = 1.0_dp - prob_fin(1) ! CL dropout prob

    prob_fin(1) = prob_col(2) * prob_fin(1)
    prob_fin(2) = 1.0_dp - prob_fin(1)

else

    prob_kid_educ = 0.0_dp
    if ( (ev_kid(ns) - ev_kid(ns-1) ) > epsi) then
            
        prob_fin(1) = f_probfin(grid%hk_grid(pc))
        prob_fin(2) = 1.0_dp - prob_fin(1)
    else
        prob_fin(1) =0.0_dp
        prob_fin(2) = 1.0_dp
    endif
endif

        
        prob_fin_hs(1) = f_probfin_hs(grid%hk_grid(pc))
        prob_fin_hs(2) = 1.0_dp - prob_fin_hs(1)
            
        val_col = prob_fin(1) * ev_kid(ns) + prob_fin(2) * ev_kid(ns-1)
        prob_col = 0.0_dp
            
        ev_kid_choice(1)= ev_kid(2)
        ev_kid_choice(2) = val_col
                        
        call sub_logsumprob(ev_kid_choice,logsum_kid_educ,prob_col,2,sigma_emp)
                    
        prob_kid_educ(1) = prob_fin_hs(2) ! hs dropout
        prob_kid_educ(2)=prob_fin_hs(1) * prob_col(1) ! hs graduate: choose no college
        prob_kid_educ(3)=prob_fin_hs(1) *prob_col(2) * prob_fin(2) ! dropout (hs grad, chose college, dropout)
        prob_kid_educ(4)=prob_fin_hs(1) *prob_col(2) * prob_fin(1) ! finish (hs grad, chose college, finish)

        logsum_kid_educ = 0.0_dp
        do sc_kid=1,ns
           logsum_kid_educ = logsum_kid_educ + prob_kid_educ(sc_kid) * ev_kid(sc_kid)
        enddo
           
        vfun_nokid =f_vfun_ivt
        f_vfun_ivt = f_vfun_ivt + nu_param * logsum_kid_educ
             
            
       ! endif
        
        
    endif
   
    end function f_vfun_ivt
    ! -------------------------------------------------------------------

  
    
    ! -------------------------------------------------------------------
    function f_vfun_tinv(tx)
    ! compute value function for given time input
    use pchip_module
    
    implicit none

    real(dp):: f_vfun_tinv
    real(dp),intent(in):: tx
    real(dp):: mcoh,cons_temp,sav_temp,util_temp, ev_tp1_temp,prob_edu_kid(ns),ev_kid,v_kid,v_kid_dc(ndl),logsum_kid,prob_kid(ndl),coh_kid,net_wage_kid,gr_wage_kid,bnet,mloc,muc,mul,t1,t2,hp1,val_tinv(ntinv),val_t_str
    integer:: sc_kid_min,sc_kid_max,dc_kid,sc_kid,kc_kid,yc_kid,tinv_loc,ierr,hcloc,ind_below
    real(dp):: logsum_t1,prob_t1(ntinv),mx,bc_sav,fe(1),xe(1),vder_temp_h(nx,np),vder_temp(np),ev_tp1_temp_h(np),lab1,lab1_eff,lab2,lab2_eff
    logical:: skip
    
    ! correction for infeasible choices
    if (max_m <=0.0_dp  )then
        mx = 0.0_dp 
        mcoh = coh_exo(xc)
        t2 = 0.0_dp
    else
        ! compute mon inv, given time, usi2 FOCs
        call sub_solvemoney_cpl(grid%hk_grid(pc),coh_exo(xc),net_wage,wage_totax,mx,tx,t2,muc,pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
            pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
            demo%numchild_gsj(2,sc2,jc,tc),mcoh,hp1,cons_temp,grid%hk_grid,cons_endo_2dim,lab1_endo_2dim,lab1eff_endo_2dim,lab2_endo_2dim,lab2eff_endo_2dim,coh_endo_2dim,evp_coh_tp1_vec_2dim,grid_sav2dim,f_aftertaxR(1.0_dp + agg%ret(tc),tau_k),ec,dc,wc1,jc,tc,min_m,max_m,flag_conv)  
      
    endif  
    
    !print*,"inv", mx,t2
    
    ! next period HK, given inputs
    hp1 = f_hh_tp1(grid%hk_grid(pc),1.0_dp,mx,tx+t2,jc-jf+1,inv_ces) 
    
    cons_temp  = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,cons_endo_2dim)
    !cons_temp = cons_temp**(-1.0_dp)
    !cons_temp =( cons_temp *  f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * betta*demo%sr(jc,tc)  )**(-1.0_dp)
    
    lab1_eff = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,lab1eff_endo_2dim)
    lab1 = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,lab1_endo_2dim)
    lab2_eff = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,lab2eff_endo_2dim)
    lab2 = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,lab2_endo_2dim)
    
    if (lab1 * net_wage(1) + lab2 * net_wage(2) <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
    sav_temp =func_sav_cpl(coh_exo(xc),net_wage,wage_totax,lab1,lab2,cons_temp,mx,demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below) !,agg%inc_thrs(tc) ) 
    
    bc_sav = func_intp(grid%hk_grid,grid_sav2dim(1,:),hp1,vals,inds,.false.,.false.)
    
    
    
    if (lab1 * net_wage(1) + lab2 * net_wage(2) <=agg%inc_thrs(tc) ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
        
    
    if (sav_temp < bc_sav)then
        cons_temp = coh_exo(xc) - bc_sav -  net_wage(1)*(biggam-lab1) - net_wage(2)*(biggam-lab2) &
            - fun_hsv_new(wage_totax(1)*lab1 + wage_totax(2)*lab2,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,sc1,ec,dc,ic,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,ind_below )  &  !,avearn_base,ind_below
        - mx *  demo%numchild_gsj(2,sc2,jc,tc)
        
        sav_temp = bc_sav
        
        
        cons_temp  = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,cons_endo_2dim)
        sav_temp =func_sav_cpl(coh_exo(xc),net_wage,wage_totax,lab1,lab2,cons_temp,mx,demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below) !,agg%inc_thrs(tc) ) 
        
    endif
    
    ! after inv coh
    mcoh = coh_exo(xc) - mx * demo%numchild_gsj(2,sc2,jc,tc)  
    
    if ( mcoh < minval(coh_endo_2dim) )then
        ! violation of borrowi2 constraint => infeasible solution
        f_vfun_tinv = -900000.0_dp
    else      
        ! current period utility       
        util_temp = f_util_parent_cpl(cons_temp,lab1_eff,lab1_eff,tx,t2,demo%numchild_gsj(2,sc2,jc,tc), ec,dc,jc-jf+1,sc1,sc2)
             
        if (sav_temp < bc_sav )then
            
            ! make sure that borrowi2 constraint is not violated
            f_vfun_tinv = -900000.0_dp
            
        else
            
            ! evaluate continuation value by interpolation
            ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)  
            
            if (opt_pchip==1)then
            
                do hcloc = 1,np
                
                    if (coh_endo_2dim(np1_aux-2,hcloc) == coh_endo_2dim(np1_aux-1,hcloc) ) then
            
                        call dpchim (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                            ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                            1, ierr)
                                    
                        xe(1) = mcoh
                        skip = .true.
                                    
                        call dpchfe (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                            ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                            1, skip, 1, xe, fe, ierr)
                                        
                        ev_tp1_temp_h(hcloc) = fe(1)
                    
                    else
                    
                        call dpchim (nx, coh_endo_2dim(:,hcloc), & 
                            ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                            1, ierr)
                                    
                        xe(1) = mcoh
                        skip = .true.
                                    
                        call dpchfe (nx, coh_endo_2dim(:,hcloc), & 
                            ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                            1, skip, 1, xe, fe, ierr)
                                        
                        ev_tp1_temp_h(hcloc) = fe(1)
                    
                    endif

                
                
                enddo
            
                call dpchim (np, grid%hk_grid, & 
                    ev_tp1_temp_h, vder_temp, & 
                    1, ierr)
                                    
                xe(1) = hp1
                skip = .true.
                                    
                call dpchfe (np, grid%hk_grid, & 
                    ev_tp1_temp_h, vder_temp, & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp = fe(1)
            
            endif
            
            
            !print*, "sav1", sav_temp
            !ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)
            !print*, "sav2", f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,grid_sav2dim,vals2x,inds2x,valsh,indsh),bc_sav  
            !pause
            
           !  ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_trans,vals2x,inds2x,valsh,indsh) 
           !  ev_tp1_temp = log( ev_tp1_temp * trans_2dim) 
                          
            ! value function
            f_vfun_tinv = f_vfun(util_temp,ev_tp1_temp,betta_til,demo%sr(jc,tc) )
            
            !print*, mx,hp1,ev_tp1_temp,cons_temp,t1,mcoh,util_temp
            !pause
            
            ! make sure that HK does not fall below min_h (i.e. punish extrapolation)
            if (hp1 < min_h .or. flag_conv==0) f_vfun_tinv = -900000.0_dp
        endif
         
        if (isnan(f_vfun_tinv))then
            print*, "puff", f_hyb2d_inter_mod(sav_temp,hp1,grid_sav2dim,grid%hk_grid,ev_tp1_vec_2dim_trans,vals2x,inds2x,valsh,indsh) 
            pause
        endif
        
        
    endif
    
    end function f_vfun_tinv
    ! -------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------
    function f_vfun_tinv_eval(tx,t2,mx,cons,lab1,lab1_eff,lab2,lab2_eff,vfun_tinv_eval_nokid)
    ! compute value function for given time input
    use pchip_module
    
    implicit none

    real(dp):: f_vfun_tinv_eval,vfun_tinv_eval_nokid
    real(dp),intent(in):: tx,t2
    real(dp),intent(in):: mx,cons,lab1,lab1_eff,lab2,lab2_eff
    real(dp):: mcoh,cons_temp,sav_temp,util_temp, ev_tp1_temp,prob_edu_kid(ns),ev_kid,v_kid,v_kid_dc(ndl),logsum_kid,prob_kid(ndl),coh_kid,net_wage_kid,gr_wage_kid,bnet,mloc,muc,mul,t1,hp1,val_tinv(ntinv),val_t_str,ev_tp1_temp_nokid 
    integer:: sc_kid_min,sc_kid_max,dc_kid,sc_kid,kc_kid,yc_kid,tinv_loc,hcloc,ierr
    real(dp):: logsum_t1,prob_t1(ntinv),bc_sav,fe(1),xe(1),ev_tp1_temp_h(np),vder_temp_h(nx,np),vder_temp(np),taggr
    logical:: skip
    
    taggr = tx + t2
    
    ! after inv coh
    mcoh = coh_exo(xc) - mx * demo%numchild_gsj(2,sc2,jc,tc)
    
    ! next period HK, given inputs
    hp1 = f_hh_tp1(grid%hk_grid(pc),1.0_dp,mx,taggr,jc-jf+1,inv_ces) 
    
    !cons_temp  = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,evp_coh_tp1_vec_2dim(:,:)**(-1.0_dp))
    !cons_temp = cons_temp**(-1.0_dp)
    !cons_temp =( cons_temp *  (1.0_dp + agg%ret(tc) ) * betta*demo%sr(jc,tc)  )**(-1.0_dp)
    
    if (lab1 * net_wage(1) + lab2 * net_wage(2) <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
    cons_temp = cons
    
    sav_temp =func_sav_cpl(coh_exo(xc),net_wage,wage_totax,pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),cons_temp,mx,demo%numchild_gsj(2,sc2,jc,tc),ec,dc,ic,jc,tc,ind_below ) 
    
    !bc_sav = func_intp(grid%hk_grid,grid_sav2dim(1,:),hp1,vals,inds,.false.,.false.)
    !
    !if (sav_temp < bc_sav)then
    !    cons_temp = coh_exo(xc) - bc_sav -  net_wage*(biggam-pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)) &
    !        - fun_hsv_new(wage_totax*pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn )  & 
    !    - mx *  demo%numchild_gsj(gc,sc,jc,tc)
    !    
    !    sav_temp = bc_sav
    !    !print*, "how",coh_exo(xc),coh_min,func_intp(grid%hk_grid,coh_endo_2dim(np1_aux,:),hp1,vals,inds,.false.,.false.)
    !    !pause
    !endif
    
    ! current period utility       
    util_temp = f_util_parent_cpl(cons_temp,lab1_eff,lab2_eff,tx,t2,demo%numchild_gsj(2,sc2,jc,tc), ec,dc,jc-jf+1,sc1,sc2)
    
        
    ! evaluate continuation value by interpolation
    ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)  
    ev_tp1_temp_nokid =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_nokid,vals2x,inds2x,valsh,indsh)
    if (opt_pchip==1)then
    
        do hcloc = 1,np
                
            if (coh_endo_2dim(np1_aux-2,hcloc) == coh_endo_2dim(np1_aux-1,hcloc) ) then
            
                call dpchim (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), &  
                    1, ierr)
                                    
                xe(1) = mcoh 
                skip = .true.
                                    
                call dpchfe (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            else
                    
                call dpchim (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, ierr)
                                    
                xe(1) = mcoh
                skip = .true.
                                    
                call dpchfe (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            endif

                
                
        enddo
            
        call dpchim (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, ierr)
                                    
        xe(1) = hp1
        skip = .true.
                                    
        call dpchfe (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, skip, 1, xe, fe, ierr)
                                        
        ev_tp1_temp = fe(1)
    
        ev_tp1_temp_nokid =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_nokid,vals2x,inds2x,valsh,indsh)  
    
        do hcloc = 1,np
                
            if (coh_endo_2dim(np1_aux-2,hcloc) == coh_endo_2dim(np1_aux-1,hcloc) ) then
            
                call dpchim (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim_nokid(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                    1, ierr)
                                    
                xe(1) = mcoh
                skip = .true.
                                    
                call dpchfe (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim_nokid(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            else
                    
                call dpchim (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim_nokid(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, ierr)
                                    
                xe(1) = mcoh
                skip = .true.
                                    
                call dpchfe (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim_nokid(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            endif

                
                
        enddo
            
        call dpchim (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, ierr)
                                    
        xe(1) = hp1
        skip = .true.
                                    
        call dpchfe (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, skip, 1, xe, fe, ierr)
                                        
        ev_tp1_temp_nokid = fe(1)
        
    endif
    
            
    !print*, "sav1", sav_temp
    !ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)
    !print*, "sav2", f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,grid_sav2dim,vals2x,inds2x,valsh,indsh),bc_sav  
    !pause
            
    !  ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_trans,vals2x,inds2x,valsh,indsh) 
    !  ev_tp1_temp = log( ev_tp1_temp * trans_2dim) 
                          
    ! value function
    f_vfun_tinv_eval = f_vfun(util_temp,ev_tp1_temp,betta_til,demo%sr(jc,tc) )
    
    vfun_tinv_eval_nokid = f_vfun(util_temp,ev_tp1_temp_nokid,betta_til,demo%sr(jc,tc) )
    
    end function f_vfun_tinv_eval
    ! -------------------------------------------------------------------
    
    
    ! ------------------------------------------------------------------- 
    function f_grid_sav(xc0)
    ! computes savi2 grid
    use nr
    use nrutil
    implicit none
    real(dp)::f_grid_sav(nx)
    integer,intent(out)::xc0
    real(dp)::max_pstk0,max_netw_tp1,max_inc_tp1
    real(dp)::min_sav0,max_sav0,min_ass0,max_ass0
    real(dp),parameter::epsi=1.0e-08_dp
    real(dp)::distx_1,distx_2,pd
    real(dp),parameter::tolbr=2.0_dp*min_x     ! set tolbr to 2.0*min_x so that solver converges
    real(dp),parameter::errabs=tolbr*100.0_dp
    real(dp):: minass1,minass2,maxass1,maxass2,minass3,maxass3,minass4,maxass4
    integer,allocatable:: ic_vec(:),dc_vecs(:),ickid_vec(:),gckid_vec(:),ass_temp_min(:,:,:,:,:),asshc_temp_min(:,:,:,:,:,:),ass_temp_max(:,:,:,:,:),asshc_temp_max(:,:,:,:,:,:)
    integer:: dcl, dc_nxt,ecp1_grd,ecp1,xc0cpl,ickid_loc,hckid_locp,gckid_loc,gckid_loc_temp,ickid_loc_temp,ic_loc,ic_loc_temp,yc_loc,dc_loc_temp,dc_loc,hckid_loc
    real(dp):: hkid_intp,gckid_loc_tem
   
    allocate(ic_vec(1))
    ic_vec = (/ic/)         
    
    if (jc+1<jr(tcp1))then
        
        if (ec==1)then
            if ( dc<nd_cpl)then
                ecp1 = 1
            else
                ecp1 = 2
            endif
        else
            ecp1=2
        endif
       
        if (ecp1==1  ) then ! not retired next period as a state
            if ( opt_test_noer==1)then ! no enodogenous retirement
           
                allocate(dc_vecs(ndl*ndl)) 
                do dcl=1,ndl*ndl
                    dc_vecs(dcl)=dcl
                enddo
          
            elseif ( opt_test_noer==0)then 
            
                if ( (jc+1)>=jer .and. (jc+1)<jr(tcp1))then
            
                    allocate(dc_vecs(ndl+1))
                    do dcl=1,ndl*ndl
                        dc_vecs(dcl)=dcl
                    enddo
                    dc_vecs(ndl*ndl+1)=nd_cpl            
                
                else
                
                    allocate(dc_vecs(ndl*ndl)) 
                    do dcl=1,ndl*ndl
                        dc_vecs(dcl)=dcl
                    enddo
                endif
                            
          
            endif
            
            
            allocate(ickid_vec(1))
            allocate(gckid_vec(1))
                
            ickid_vec(1) = 1  ! this is always one
            gckid_vec(1) = 1
            
             
            !maxass1 =min( minval(grid%assmin(dc_vecs,nx,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) , minval(grid%assmax(dc_vecs,nx,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) ) 
            if (jc==jf-1)then
                maxass1 =minval(grid%ass_cpl(dc_vecs,nx,pc_tp1_min:pc_tp1_max,:,:,ecp1,:,:,kc1,kc2,sc1,sc2,jc+1,tcp1))  
            else
                maxass1 =minval(grid%ass_cpl(dc_vecs,nx,pc_tp1,:,:,ecp1,:,:,kc1,kc2,sc1,sc2,jc+1,tcp1))
            endif
            
                               
            max_ass0=maxass1 
          
            !minass1 =max ( maxval(grid%assmin(dc_vecs,1,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) , maxval(grid%assmax(dc_vecs,1,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) )
            if (jc==jf-1)then
                minass1 = maxval(grid%ass_cpl(dc_vecs,1,pc_tp1_min:pc_tp1_max,:,:,ecp1,:,:,kc1,kc2,sc1,sc2,jc+1,tcp1))
            else
                minass1 = maxval(grid%ass_cpl(dc_vecs,1,pc_tp1,:,:,ecp1,:,:,kc1,kc2,sc1,sc2,jc+1,tcp1))
            endif
            
           
            min_ass0=minass1        
            
            
        else ! ecp1=2
            
            allocate(ickid_vec(1))
            allocate(gckid_vec(1))
                
            ickid_vec(1) = ickid  ! this stays constant after jf
            gckid_vec(1) = gckid              
        
            if (opt_test_noer)then
                print*, "should be possible only with end ret" ! tbc put assert here
                pause
                
            else
                
                if (jc>=jer )then ! early
                    max_ass0=grid%ass_cpl(nd_cpl-1,nx,pc,yc1,yc2,ecp1,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                    min_ass0=grid%ass_cpl(nd_cpl-1,1,pc,yc1,yc2,ecp1,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                elseif (jc>=jrr)then ! regular
                    max_ass0=grid%ass_cpl(nd_cpl,nx,pc,yc1,yc2,ecp1,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                    min_ass0=grid%ass_cpl(nd_cpl,1,pc,yc1,yc2,ecp1,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                else
                    print*, "what are we missi2?"
                    pause
                endif
                
            endif
                    
     
        endif
       
    
    else ! jc+1>=jr next period
        
        allocate(ickid_vec(1))
        allocate(gckid_vec(1))
                
        ickid_vec(1) = 1  ! this stays constant after jf
        gckid_vec(1) = 1           
        
        if (opt_test_noer)then 
            max_ass0=grid%ass_cpl(nd_cpl,nx,1,yc1,yc2,ne,1,1,kc1,kc2,sc1,sc2,jc+1,tcp1)
            min_ass0=grid%ass_cpl(nd_cpl,1,1,yc1,yc2,ne,1,1,kc1,kc2,sc1,sc2,jc+1,tcp1)
        else
            
            if (jc+1==jr(tc))then        
                if (ec==1)then ! go in ret at mand age jr
                    max_ass0=grid%ass_cpl(nd_cpl-2,nx,pc,yc1,yc2,ne,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                    min_ass0=grid%ass_cpl(nd_cpl-2,1,pc,yc1,yc2,ne,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                else
                    max_ass0=grid%ass_cpl(dc,nx,pc,yc1,yc2,ne,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                    min_ass0=grid%ass_cpl(dc,1,pc,yc1,yc2,ne,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                endif
            else
                max_ass0=grid%ass_cpl(dc,nx,pc,yc1,yc2,ne,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
                min_ass0=grid%ass_cpl(dc,1,pc,yc1,yc2,ne,wc1,wc2,kc1,kc2,sc1,sc2,jc+1,tcp1)
            endif
        endif
        
                    
    endif
   
    deallocate(ic_vec,ickid_vec,gckid_vec)
    
    ! bound on max_x0, min_x0: avoid maki2 grids too large
    !if (gen_id ==1)then
    !    max_ass0=min(max_ass0,max_x)
    !    min_ass0=max(min_ass0,min_x)
    !else
        max_ass0=min(max_ass0,max_x)
        min_ass0=max(min_ass0,grid%amin_age(1,sc1,gen_id,jc))
    !endif
    
    ! compute bounds of savi2 grid:
    max_sav0=max_ass0 * (1.0_dp + lamb)
    min_sav0=min_ass0 * (1.0_dp + lamb)
   
    if (min_sav0>max_sav0) then
        print*, 'there is a problem with flexible grid: increase parameter max_x',jc, min_ass0,max_ass0,maxass1
        print*, 'counters: ', ec,dc,ic,gc,jc+1,kc,sc,jr(sc),dc_vecs,maxass1,ickid,gckid,pc_tp1
        !print*, grid%ass(ec1(jc+1,tcp1):ec2(jc+1,tcp1),nx,pc,:,ec1(jc+1,tcp1):ec2(jc+1,tcp1),ic,jc+1,kc,sc,tcp1)
        pause
    endif
     
    if (min_sav0<epsi .and. max_sav0<epsi)then
        print*, "there is another problem"
        pause
    endif
   
    if (jc+1<jr(tc) .and. ecp1==1)then
        deallocate(dc_vecs)
    endif
   
    xc0=np1_aux   ! default: throw in point in region of bindi2 borrowi2 constraint 
   
    !if (gen_id==1)then
    !
    !    ! build savi2 grid: 
    !    if (xc0==1) then
    !        if (max_x_fac>epsi) then
    !            f_grid_sav(1:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-1,3.0_dp)
    !            f_grid_sav(nx)=max_sav0
    !        else
    !            f_grid_sav(1:nx)=makegrid(min_sav0,max_sav0,nx,3.0_dp)
    !        endif
    !    else    
    !        f_grid_sav(1:np1_aux-1)=0.0_dp
    !        if (max_x_fac>epsi) then
    !            f_grid_sav(np1_aux:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-np1_aux,3.0_dp)
    !            f_grid_sav(nx)=max_sav0
    !        else
    !            f_grid_sav(np1_aux:nx)=makegrid(min_sav0,max_sav0,nx-np1_aux+1,3.0_dp)
    !        endif
    !    endif
    !    
    !else
    !    
        ! build savi2 grid: 
        if (xc0==1) then
            if (max_x_fac>epsi) then
                f_grid_sav(1:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-1,3.0_dp)
                f_grid_sav(nx)=max_sav0
            else
                f_grid_sav(1:nx)=makegrid(min_sav0,max_sav0,nx,3.0_dp)
            endif
        else    
            f_grid_sav(1:np1_aux-1)=grid%amin_age(1,sc1,gen_id,jc)
            if (max_x_fac>epsi) then
                f_grid_sav(np1_aux:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-np1_aux,3.0_dp)
                f_grid_sav(nx)=max_sav0
            else
                f_grid_sav(np1_aux:nx)=makegrid(min_sav0,max_sav0,nx-np1_aux+1,3.0_dp)
            endif
        endif
        
   ! endif
   
    end function f_grid_sav
    ! ------------------------------------------------------------------- 

    
    ! -------------------------------------------------------------------
    subroutine sub_prepintp_ret(sav,v_tp1,vp_coh_tp1,ind_bc)
    ! compute continuation value and rhs_foc in retirement
    
    implicit none
    real(dp),intent(in):: sav
    logical,intent(in):: ind_bc
    real(dp),intent(out):: v_tp1,vp_coh_tp1
    integer:: gc
                                        
    ! pension payments (linked to respective aggregate wage)
    do gc = 1,2
        av_prod_tp1 =demo%ageprod(gc,jr(tcp1)-1,sc(gc) )  
        call sub_pens_new(gr_pens_tp1(gc),agg%rho_p(tcp1),agg%tau_p(tcp1),0.0_dp,agg%avwage(tcp1),agg%pia0(gc,1,sc(gc),tcp1), & 
            av_prod_tp1 ,demo%grid_y(ycp1(gc)),0.0_dp,jc+1,gc,dcp1,ecp1,tcp1,sc(gc),kc(gc),ycp1(gc),nd_cpl)
        pens_tp1(gc) = fun_netpens(gr_pens_tp1(gc),0.0_dp,0.0_dp,2,gc,ecp1,dcp1,1,jc+1,tcp1,avearn_tp1) 
    enddo
    
    ! cash on hand tomorrow: based on maximum income
    coh_tp1=f_coh_tp1_cpl(sav,b_til_tp1(sc1,jc+1) + b_til_tp1(sc2,jc+1),R_til_tp1,net_wage_tp1,pens_tp1,demo%lamt(tcp1),agg%tau_k(tcp1),ecp1,dcp1)
    
    call sub_chkbd(grid%coh_cpl(dcp1,:,pc_tp1:pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,lb_viol,ub_viol,dc,ec,dcp1,ecp1,ind_bc)   ! correction in case of bound violation                
    
    !if (ub_viol )then
    !    print*, "ret ub viol", jc,hc,sc
    !    pause
    !endif
    
    
    ! interpolation: value function and rhs_foc
    v_tp1=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%v_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
    
    if (isnan(v_tp1))then
        print*, "ret",jc, grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%v_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1)
        pause
    endif
                                     
    vp_coh_tp1=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%inv_vp_x_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
        
    
     
    vp_coh_tp1=f_reinv(vp_coh_tp1,0.0_dp)
                           
    end subroutine sub_prepintp_ret
    ! -------------------------------------------------------------------
    
   
    
    ! -------------------------------------------------------------------
    subroutine sub_prepintp_wrk(sav,v_tp1,v_tp1_nokid,vp_coh_tp1,ind_bc)
    ! compute continuation value and rhs_foc for workers
    
    implicit none
    real(dp),intent(in):: sav
    logical,intent(in):: ind_bc
    real(dp),intent(out):: v_tp1,vp_coh_tp1,v_tp1_nokid
    real(dp):: vals_h(2),valsx_h(np,2),hp1_hs
    integer:: inds_h(2),indsx_h(np,2),gc,wcp1(2)
                                        
    ! wages, pension benefits tomorrow [would be relevant for endogenous retirement]
   
    wcp1(1) = wc1p1
    wcp1(2) = wc2p1
    
    do gc = 1,2
    
        call sub_wage(gr_wage_tp1(gc),net_wage_tp1(gc),wage_totax_tp1(gc),agg%wage_s(sc(gc),tcp1),demo%ageprod(gc,jc+1,sc(gc)),demo%grid_y(ycp1(gc)),grid%hk_grid(1),agg%tau_p(tcp1),sc(gc),kc(gc),dcp1,ecp1,jc+1,gen_id,agg%gammah_s(sc(gc),tc),agg%abgrad_s(:,sc(gc)),agg%hk_cutoff(:,tc)) 
        gr_wage_tp1(gc) = gr_wage_tp1(gc) * demo%grid_epsi(wcp1(gc))
        net_wage_tp1(gc) = net_wage_tp1(gc) * demo%grid_epsi(wcp1(gc))
        wage_totax_tp1(gc) = wage_totax_tp1(gc) * demo%grid_epsi(wcp1(gc))
    
    
        ! pension payments (linked to respective aggregate wage)
        av_prod_tp1 = demo%ageprod(gc,jr(tcp1)-1,sc(gc) )  
        call sub_pens_new(gr_pens_tp1(gc),agg%rho_p(tcp1),agg%tau_p(tcp1),0.0_dp,agg%avwage(tcp1),agg%pia0(gc,1,sc(gc),tcp1), & 
            av_prod_tp1 ,demo%grid_y(ycp1(gc)),0.0_dp,jc+1,gc,dcp1,ecp1,tcp1,sc(gc),kc(gc),ycp1(gc),nd_cpl)
        pens_tp1(gc) = fun_netpens(gr_pens_tp1(gc),0.0_dp,0.0_dp,2,gc,ecp1,dcp1,icp1,jc+1,tcp1,avearn_tp1) 
    
    enddo
    
    
    ! cash on hand tomorrow: based on maximum income
    coh_tp1=f_coh_tp1_cpl(sav,b_til_tp1(sc(1),jc+1) + b_til_tp1(sc(2),jc+1),R_til_tp1,net_wage_tp1,pens_tp1,demo%lamt(tcp1),agg%tau_k(tcp1),ecp1,dcp1)
    
    ! interpolation: value function
     call sub_chkbd(grid%coh_cpl(dcp1,:,pc_tp1:pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,lb_viol,ub_viol,dc,ec,dcp1,ecp1,ind_bc)   ! correction in case of bound violation
        
    !if (ub_viol )then
    !    print*, "w ub viol", jc,dcp1,pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid_tp1(gen_id,jc+1,ickid,ickid_init),gckid_tp1(gen_id,jc+1,gckid,gckid_init),jc+1,tcp1,gen_id
    !    !pause
    !endif
    
    v_tp1=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%v_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals ,inds ,.false.,.false.)
        
    v_tp1_nokid=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1), & 
        pol%v_nokid_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals ,inds ,.false.,.false.)
        
        
    if (isnan(v_tp1))then
        print*, "aha", v_tp1,coh_tp1,dcp1,pc_tp1,yc1p1,ecp1,icp1,gc,kcp1,sc1,jc+1,tcp1
        pause
    endif
                                                        
    vp_coh_tp1=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%inv_vp_x_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
   
   
        
        
    vp_coh_tp1=f_reinv(vp_coh_tp1,0.0_dp)
    
    !if (jc==jt-1)then
    !    print*, vp_coh_tp1, v_tp1
    !    pause
    !endif
    
    
    end subroutine sub_prepintp_wrk
    ! -------------------------------------------------------------------
   
    ! ------------------------------------------------------------------- 
    subroutine sub_solvehh_sav(sav,ind_bc,flag_invalid,ext_flg,opt_input)
    ! computes solution of household problem on current savi2 grid point
    implicit none
    real(dp),intent(in)::sav
    integer,intent(in):: flag_invalid,ext_flg,opt_input
    logical,intent(in)::ind_bc
    real(dp)::check
    real(dp),dimension(ni)::evp_coh_tp1_ni,evp_grpstk_tp1_ni,ev_tp1_ni,ev_tp1_ni_nokid
    real(dp):: vp_coh_tp1_test
    integer:: dc_max,dc_locc
    real(dp),allocatable:: prob(:),vp_coh_tp1_temp(:),v_tp1_temp(:),v_tp1_temp_nokid(:)
    real(dp):: logsum,logsum_nokid,logsum_emp,prob_empret(2),v_tp1_empret(2)
    integer::ecp1_min,ecp1_max,dcp1_temp,dc_loc,dc_temp_loc
    integer,allocatable:: dc1_vec(:)
    integer:: dcl,ecp1_temp,n_ep1
    integer,allocatable:: vec_ecp1(:)
    integer:: icp1_temp,qcp1_max,ickid_init_min,ickid_init_max,gckid_init_min,gckid_init_max,ycp1_max,wc1p1_max,wc2p1_max,yc2p1_max
    real(dp),allocatable:: ic1_vec(:)
    
    ! compute wages and pension benefits TODAY 
    call sub_wagepens_cpl()
                    
    ! initialization of expected value function and its derivatives
    evp_coh_tp1_ni(:)=0.0_dp
    evp_grpstk_tp1_ni(:)=0.0_dp
    ev_tp1_ni(:)=0.0_dp
    ev_tp1_ni_nokid = 0.0_dp
   
    ycp1_max = ny
    wc1p1_max = nw
    wc2p1_max = nw
   
    
    if (jc+1<jr(tcp1)) then ! worki2 period (before mandatory retirement age)
       
        allocate(ic1_vec(1)) ! child innate ability next period
        ic1_vec = (/1/)                 
        
        do icp1_temp = 1,1 !size(ic1_vec) ! innate ability parent
             
            icp1 = 1 !ic1_vec(icp1_temp)
             
            if (ec==1)then
                if ( dc<nd_cpl)then
                    ecp1 = 1
                else
                    ecp1 = 2
                endif
            else
                ecp1=2
            endif
            
            
            if (ecp1<ne)then ! not retired tomorrow (as a state)
                
                do yc1p1=1,ycp1_max !ny     ! income state tomorrow 
                    
                    do yc2p1 = 1,ycp1_max
                        ycp1(1) = yc1p1
                        ycp1(2) = yc2p1
                    do wc1p1 = 1,wc1p1_max
                    do wc2p1 = 1,wc2p1_max                       
                    ! number of discrete choices available
                    if (opt_test_noer==1)then ! no endogenous retirement
                            
                        allocate(dc1_vec(ndl*ndl))
                        do dcl=1,ndl*ndl
                            dc1_vec(dcl)=dcl
                        enddo
                     
                   
                    elseif (opt_test_noer==0)then ! yes endogenous retirement
                                
                        if ( (jc+1)>=jer .and. (jc+1)<jr(tcp1)) then
                            
                            allocate(dc1_vec(ndl*ndl+1))
                            do dcl=1,ndl*ndl
                                dc1_vec(dcl)=dcl
                            enddo
                            dc1_vec(ndl*ndl+1)=nd_cpl
                                 
                        else
                            allocate(dc1_vec(ndl*ndl))
                            do dcl=1,ndl*ndl
                                dc1_vec(dcl)=dcl
                            enddo
                        endif
                                                                
                                             
                    endif
                        
                    nd_glob=size(dc1_vec)
                        
                    allocate(prob(nd_glob))
                    allocate(v_tp1_temp(nd_glob))
                    allocate(v_tp1_temp_nokid(nd_glob))
                    allocate(vp_coh_tp1_temp(nd_glob))
                            
                    if (nd_glob<1)then
                        print*, "what happened?"
                        pause
                    endif
                    
                    if (gen_id==1 .and. jc+1==jf)then
                            
                        ickid_init_min = 1
                        ickid_init_max = ni    
                        gckid_init_min = 1
                        gckid_init_max =1
                        
                    else
                        
                        ickid_init_min = 1
                        ickid_init_max = 1    
                        gckid_init_min = 1
                        gckid_init_max =1
                        
                    endif
                    
                    do pc_tp1 = pc_tp1_min,pc_tp1_max
                    
                    do gckid_init = 1,gckid_init_max
                    
                        do ickid_init = 1,ickid_init_max                    
                    
                            ! determine probabilities
                            do dcp1_temp=1,size(dc1_vec)
                       
                                dcp1 = dc1_vec(dcp1_temp)  
                        
                                v_tp1_temp(dcp1_temp)=0.0_dp
                                v_tp1_temp_nokid(dcp1_temp)=0.0_dp
                                vp_coh_tp1_temp(dcp1_temp)=0.0_dp
                        
                                call sub_prepintp_wrk(sav,v_tp1,v_tp1_nokid,vp_coh_tp1,ind_bc)
                               
                                v_tp1_temp(dcp1_temp)=v_tp1
                                v_tp1_temp_nokid(dcp1_temp)=v_tp1_nokid
                                vp_coh_tp1_temp(dcp1_temp)=vp_coh_tp1
                        
                            enddo                          
                            
                            if (size(dc1_vec)==ndl*ndl+1)then
                                call sub_logsumprob(v_tp1_temp(1:ndl*ndl),logsum_emp,prob(1:ndl*ndl),nd_glob-1,sigma_emp)
                                v_tp1_empret(1) = logsum_emp
                                v_tp1_empret(2) = v_tp1_temp(ndl*ndl+1)
                                call sub_logsumprob(v_tp1_empret,logsum,prob_empret,2,sigma_ret)
                                do dc_temp_loc = 1,ndl*ndl
                                    prob(dc_temp_loc)= prob(dc_temp_loc) * prob_empret(1)
                                enddo
                                prob(ndl*ndl + 1) = prob_empret(2)
                                                
                            else
                                if (nd_glob>1)then
                                    call sub_logsumprob(v_tp1_temp,logsum,prob,nd_glob,sigma_emp)
                                else
                                    logsum = v_tp1_temp(1)
                                    prob = 1.0_dp

                                    logsum_nokid = v_tp1_temp_nokid(1)
                                    
                                endif
                                
                            endif
                    
                            prob_dc=0.0_dp
        
                            do dc_temp_loc=1,size(dc1_vec)
                                dc_loc=dc1_vec(dc_temp_loc)
            
                                do dc_full = 1,nd_cpl
                                    if (dc_loc==dc_full)then
                                        prob_dc(dc_full)=prob(dc_temp_loc)
                                    endif
                                enddo
                            enddo
                        
                            
                                ev_tp1_ni(icp1)=ev_tp1_ni(icp1)+demo%prob_epsi(wc1p1)*demo%prob_epsi(wc2p1)*demo%prob_y(yc1,yc1p1)*demo%prob_y(yc2,yc2p1)*logsum* prob_ni(pc_tp1)
                                    
                                logsum_nokid = 0.0_dp
                                do dc_locc = 1,size(dc1_vec)
                                    dcp1 = dc1_vec(dc_locc)
                                    logsum_nokid = logsum_nokid + v_tp1_temp_nokid(dcp1) * prob(dcp1)
                                enddo
                                                                
                                ev_tp1_ni_nokid(1)=ev_tp1_ni_nokid(1)+demo%prob_epsi(wc1p1)*demo%prob_epsi(wc2p1)*demo%prob_y(yc1,yc1p1)*demo%prob_y(yc2,yc2p1)*logsum_nokid* prob_ni(pc_tp1)
                             
                            
                        
                            do dcp1_temp=1,size(dc1_vec)
                    
                                dcp1=dc1_vec(dcp1_temp)
                            
                                    evp_coh_tp1_ni(1)=evp_coh_tp1_ni(1)+demo%prob_epsi(wc1p1)*demo%prob_epsi(wc2p1)*demo%prob_y(yc1,yc1p1)*demo%prob_y(yc2,yc2p1)*prob_dc(dcp1)*vp_coh_tp1_temp(dcp1_temp)* prob_ni(pc_tp1)
                               
                            enddo
                        
                        enddo
                    enddo
                    
                    enddo
                    
                    
                    deallocate(prob,v_tp1_temp,v_tp1_temp_nokid,vp_coh_tp1_temp,dc1_vec)
                enddo 
                
                                    
                    end do  ! end do ycp1 
                    enddo
                    enddo
                
               
            else ! ecp1 = ne => retired (before mandatory age)
                    
                yc1p1=yc1
                yc2p1 = yc2
                ycp1(1) = yc1p1
                ycp1(2) = yc2p1
               
                wc1p1 = wc1
                wc2p1 = wc2
                
                if (jc>=jer .and. jc<jrr)then ! early ret
                    dcp1=nd_cpl-1
                elseif (jc>=jrr)then ! reg
                    dcp1=nd_cpl
                else
                    print*, "should not be here3!" ! tbc put assert statement
                    pause
                endif                                                                                                                       
               
                call sub_prepintp_ret(sav,v_tp1,vp_coh_tp1,ind_bc)
               
               
                ev_tp1_ni(icp1)=v_tp1
                ev_tp1_ni_nokid(icp1)=v_tp1
                evp_coh_tp1_ni(icp1)=vp_coh_tp1
               
            endif                                
           
        end do  ! end do icp1
        
        deallocate(ic1_vec)
       
        ev_tp1=ev_tp1_ni(1)
        ev_tp1_nokid=ev_tp1_ni_nokid(1)
        evp_coh_tp1=evp_coh_tp1_ni(1)
       
        if (evp_coh_tp1<0.0_dp)then
            print*, "uuupps", evp_coh_tp1,ec,dc,jc, v_tp1_temp, logsum
            pause
        endif
        
    else ! jc+1>=jr => retirement after mand age
              
        allocate(ic1_vec(1))
        ic1_vec = (/ic/)         
        
        
        do icp1_temp=1,size(ic1_vec) ! citizenship tomorrow
            icp1 = ic1_vec(icp1_temp)
           
            yc1p1=yc1
            yc2p1=yc2
            ycp1(1) = yc1p1
            ycp1(2) = yc2p1
            wc1p1 = wc1
            wc2p1 = wc2
            ecp1=ne
            
            if (jc+1==jr(tc))then 
                if (ec==1)then
                    
                    if (opt_test_noer)then
                        dcp1=nd_cpl
                    else                        
                        dcp1=nd_cpl-2 ! late retirement
                    endif
                    
                else
                    dcp1=dc
                endif
            else
                dcp1=dc
            endif
           
           
            call sub_prepintp_ret(sav,v_tp1,vp_coh_tp1,ind_bc)
            
            
            ev_tp1_ni(icp1)=v_tp1
            ev_tp1_ni_nokid(icp1)=v_tp1
            evp_coh_tp1_ni(icp1)=vp_coh_tp1
           
        end do  ! end do icp1
        
        deallocate(ic1_vec)
            
      
        ev_tp1=ev_tp1_ni(ic)
        ev_tp1_nokid=ev_tp1_ni_nokid(ic)
        evp_coh_tp1=evp_coh_tp1_ni(ic)
       
    endif
    
    if (isnan(ev_tp1))then
        print*, "NaN cont value, how can this be?",tc,jc,ec,dc,ic,v_tp1,ben_p
        pause
    endif
    
    
    if (evp_coh_tp1<0.0_dp)then
        print*, "here uuupps", evp_coh_tp1,ec,dc,jc
        pause
    endif
                     
    ! solution of household problem given expected and interpolated values
    
    if (ind_bc)then
        pol%indbc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=1
    else
        pol%indbc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc)=0
        df=betta_til *demo%sr(jc,tc)              ! discount factor: notice: sr is prob to survive from jc to jc+1 
        rhs_foc=df*f_aftertaxR(R_til_tp1,tau_k)*evp_coh_tp1         ! term in FOC(c)
        
        if (rhs_foc<=0.0_dp .and. df/=0.0_dp)then
            print*, "messed up rhs",jc,ec,dc
            pause
        endif        
        
    endif
                            
    call sub_solvehh_dec_cpl(sav,pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
        pol%leis1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),time_cost, &
        pol%leis2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_eff_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        grid%coh_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
        grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), assmin,assmax,&
        pol%grwage_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%grwageinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), pol%grinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        pol%netinc_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), pol%netsav_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
        pol%v_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%v_nokid_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        pol%inv_vp_x_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),& 
        gr_wage,net_wage,wage_totax,agg%wage_s(sc1,tc),ben_p,R,rhs_foc,pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),ind_bc, &
        pol%labinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%hsvtmp1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), & 
        pol%hsvtmp2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        pol%capinctaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        pol%constaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        pol%penstaxrv_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),bq(sc1,jc) + bq(sc2,jc),ben_u, &
        demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc),0.0_dp,flag_stud,flag_invalid,pol%net_trr_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),ext_flg,opt_input) 
    
    
    ! check budget constraint
    call sub_checkbudget_cpl(pol%cons_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab1_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),pol%lab2_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc), &
        sav,grid%ass_cpl(dc,xc,pc,yc1,yc2,ec,wc1,wc2,kc1,kc2,sc1,sc2,jc,tc),bq(sc1,jc) + bq(sc2,jc),net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,jc,tc,demo%numchild_gsj(2,sc2,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc))  
                                        
    end subroutine sub_solvehh_sav
    ! ------------------------------------------------------------------- 
           
    
    ! ------------------------------------------------------------------- 
    subroutine sub_solvehh_dec_cpl(sav,cons,leis1,lab1_eff,leis2,lab2_eff,tcost,coh,ass,assmi2r,assmaxgr,grwage,grwageinc,grinc,netinc,netsav,v,v_nokid,inv_vp_x, & 
        gr_wage,net_wage,wage_totax,agg_wage,ben_p,R,rhs_foc,lab1,lab2,ind_bc,labinctaxrv,hsvtmp1,hsvtmp2,capinctaxrv,constaxrv,penstaxrv,tr,sa_tr,numchld,avearn,tau_c,tau_k,tau_h,flag_stud, & 
        flag_invalid,net_trr,dc,opt_input)
    ! solution of household problem at given savi2 grid point / coh grid point
    use nr
    use nrutil
    implicit none
    
    real(dp),intent(inout)::cons,leis1,lab1_eff,lab1,leis2,lab2_eff,lab2,coh,ass,assmi2r,assmaxgr,grwage,grwageinc,grinc,netinc,netsav,v,inv_vp_x,rhs_foc,v_nokid
    real(dp),intent(out):: labinctaxrv,capinctaxrv,constaxrv,penstaxrv,sa_tr,hsvtmp1,hsvtmp2
    integer,intent(in):: flag_stud,dc,opt_input
    integer,intent(out):: net_trr
    real(dp),intent(in)::gr_wage(:),net_wage(:),wage_totax(:),agg_wage,ben_p(:),R,numchld,avearn,tau_c,tau_k,tau_h,tcost
    real(dp),intent(in):: sav,tr
    integer,intent(in):: flag_invalid
    logical,intent(in)::ind_bc
    real(dp)::util,mu_c,mu_leis,dist,lab1_u,lab2_u
    real(dp)::c0,c1,f0,f1,util_dc(4),util_u1,util_u2,util_u12,cons_u,cons_u1,cons_u2
    real(dp),parameter::epsi=1.0e-06 
    integer:: dc_str
    
    if (jc<jr(tc)) then ! worki2 period (before mandatory retirement age)
        
        if ( (ec==1 )  ) then  ! employment
            
            
            
            
            if (dc==1)then
                dc1=1
                dc2=1
            elseif (dc==2)then
                dc1=1
                dc2=2
            elseif (dc==3)then
                dc1=2
                dc2=1
            elseif (dc==4)then
                dc1=2
                dc2=2
            endif
            
            if (opt_input==1)then
                
                coh = x_grid(xc)
                lab1 = lab1_dc_in_intp(dc,xc)
                lab2 = lab2_dc_in_intp(dc,xc)
                cons = cons_dc_in_intp(dc,xc)
                lab1_eff = lab1
                lab2_eff = lab2
                leis1=biggam - lab1_eff
                leis2=biggam - lab2_eff
                
            else
                
                
            if (ind_bc .and. flag_invalid==0 ) then        ! case of bindi2 borrowi2 constraint

                if (dc1==2 .and. dc2==2)then
                    lab1 =0.0_dp ! ft_lab * 0.1_dp
                    lab1_eff = lab1
                    leis1=biggam - lab1_eff 
                    lab2 =0.0_dp !  ft_lab * 0.1_dp
                    lab2_eff = lab2
                    leis2=biggam - lab2_eff 
                    ! GROSS consumption from BC            
                    cons=f_cons_constr_cpl(coh,sav,net_wage,wage_totax,lab1,lab2,ec,dc,ic,jc,tc,numchld,1) !,agg%inc_thrs(tc))
                else
                    if (dc1==1 .and. dc2==1)then
                        ind_below = 1
                        !if (gen_id==1 )then 
                        
                            call sub_corner_cons_lab_cpl(cons,sav,lab1,lab2,coh,net_wage,wage_totax,jc,tc,numchld,agg%tau_c(tcp1),ind_below,1,2)   
                   
                       ! endif
                        if (lab1<0.0_dp .or. lab2<0.0_dp)then
                            print*, lab1,lab2,cons,sav
                            pause
                        endif
                    
                        !lab1 =0.2_dp ! max(0.0_dp, lab1) 
                        !lab2 = 0.2_dp !max(0.0_dp, lab2) 
                        lab1_eff = lab1
                        lab2_eff = lab2
                       
                        if (gen_id==1 )then     
                        
                            if (opt_mt==1)then
                        
                                if (lab1*net_wage(1) +lab2*net_wage(2)  <= (agg%inc_thrs(tc) ) ) then
                                    ! fine
                                    ind_below = 1
                                else
                                    ind_below = 0
                                    !call sub_corner_cons_lab(cons,sav,lab,coh,net_wage,wage_totax,gc,jc,tc,numchld,flag_stud,agg%tau_c(tcp1),ind_below) 
                                    !lab_eff = lab
                                    !lab = lab**1.4_dp
                                    !
                                    !if (lab < (agg%inc_thrs(tc) / net_wage) ) then
                                    !    lab = (agg%inc_thrs(tc) / net_wage)
                                    !    lab_eff = lab**(1.0_dp/1.4_dp)
                                    !    ind_below = 1
                                    !endif
                            
                            
                            
                                endif    
                            
                            endif
                        
                    
                        endif
                        
                    elseif (dc1==1 .and. dc2==2)then
                        
                        ind_below = 1
                        !if (gen_id==1 )then 
                        
                            call sub_corner_cons_lab_cpl(cons,sav,lab1,lab2,coh,net_wage,wage_totax,jc,tc,numchld,agg%tau_c(tcp1),ind_below,1,1)   
                   
                       ! endif
                        if (lab1<0.0_dp .or. lab2<0.0_dp)then
                            print*, lab1,lab2,cons,sav
                            pause
                        endif
                    
                        !lab1 =0.2_dp ! max(0.0_dp, lab1) 
                        !lab2 = 0.2_dp !max(0.0_dp, lab2) 
                        lab1_eff = lab1
                        lab2_eff = lab2
                        
                    elseif (dc1==2 .and. dc2==1)then
                        
                        ind_below = 1
                        !if (gen_id==1 )then 
                        
                            call sub_corner_cons_lab_cpl(cons,sav,lab1,lab2,coh,net_wage,wage_totax,jc,tc,numchld,agg%tau_c(tcp1),ind_below,2,2)   
                   
                       ! endif
                        if (lab1<0.0_dp .or. lab2<0.0_dp)then
                            print*, lab1,lab2,cons,sav
                            pause
                        endif
                    
                        !lab1 =0.2_dp ! max(0.0_dp, lab1) 
                        !lab2 = 0.2_dp !max(0.0_dp, lab2) 
                        lab1_eff = lab1
                        lab2_eff = lab2    
                        
                    endif
                    
                    
                    leis1 = biggam - lab1_eff
                    leis2 = biggam - lab2_eff
                    !lab =ft_lab! grid%lab(1,1,1)
                    cons=f_cons_constr_cpl(coh,sav,net_wage,wage_totax,lab1,lab2,ec,dc,ic,jc,tc,numchld,ind_below)
                   
                endif
               
                ! NET consumption
                cons = f_netcons(cons,tau_c)
              
    leis2=biggam - lab2
    leis1=biggam - lab1
               
            else    ! no bindi2 constraint, solution for interior labor supply                
                
                ! leisure is fixed
                if (dc1==2 .and. dc2==2)then ! zero lab
                    lab1 =0.0_dp !  ft_lab * 0.1_dp
                    lab1_eff = lab1
                   
                    leis1=biggam - lab1
                    
                    lab2 =0.0_dp !  ft_lab * 0.1_dp
                    lab2_eff = lab2
                   
                    leis2=biggam - lab2
                else
                    if (dc1==1 .and. dc2==1 )then
                        
                        ind_below = 1
                        lab1 = func_lab_cpl(rhs_foc,net_wage(1),1,jc,agg%tau_c(tcp1),ind_below) 
                        lab1_eff = lab1
                        lab2 =  func_lab_cpl(rhs_foc,net_wage(2),2,jc,agg%tau_c(tcp1),ind_below) 
                        lab2_eff = lab2
                        
                        !print*, lab1,lab2
                            
                        if (opt_mt==1)then
                        
                            if (lab1*net_wage(1) +lab2*net_wage(2)  <= (agg%inc_thrs(tc) ) ) then
                                ! fine
                                ind_below = 1
                            else
                                ind_below = 0
                                !lab = func_lab(rhs_foc,net_wage,gc,jc,flag_stud,agg%tau_c(tcp1),ind_below)
                                !lab_eff = lab
                                !lab = lab**1.4_dp
                                !
                                !if (lab < (agg%inc_thrs(tc) / net_wage) ) then
                                !    lab = (agg%inc_thrs(tc) / net_wage)
                                !    lab_eff = lab**(1.0_dp/1.4_dp)
                                !    ind_below = 1
                                !endif
                            
                            
                            
                            endif
                            
                        endif
                        
                        
                    elseif (dc1==1 .and. dc2==2)then
                        
                        ind_below = 1
                        lab1 = func_lab_cpl(rhs_foc,net_wage(1),1,jc,agg%tau_c(tcp1),ind_below) 
                        lab1_eff = lab1
                        lab2 = 0.0_dp !  ft_lab * 0.1_dp
                        lab2_eff = lab2
                        
                    elseif (dc1==2 .and. dc2==1)then
                        
                        ind_below = 1
                        lab1 =0.0_dp !  ft_lab * 0.1_dp
                        lab1_eff = lab1
                        lab2 =  func_lab_cpl(rhs_foc,net_wage(2),2,jc,agg%tau_c(tcp1),ind_below) 
                        lab2_eff = lab2
                        
                            
                    endif
                    
                    leis1=biggam - lab1
                    leis2=biggam - lab2
                endif 
                
                     
              
                ! closed form solution for consumption: interior solution
                cons=f_cons_dc(rhs_foc,0.0_dp,ec,dc)
                ! NET consumption
                !cons = f_netcons(cons,tau_c)
  
    leis1=biggam - lab1
    leis2=biggam - lab2            
                ! check cons FOC
                call sub_muc(cons,0.0_dp,mu_c)
                dist=abs(mu_c/rhs_foc-1.0_dp)
                if ( dist>epsi .and. cons>epsi ) then
                    print*, 'first-order condition for consumption violated si2les',mu_c,rhs_foc,cons,leis1,rhs_foc**(-1.0_dp)*pheye * leis1**( (1.0-pheye)*(1.0-tetta) )
                    pause
                endif
            
                ! endogenous coh grid:
                coh=f_grid_coh_cpl(sav,cons,lab1,lab2,net_wage,wage_totax,ic,ec,dc,jc,tc,numchld,avearn,tau_c)
                
            endif
            
            endif
            
       
        elseif ( (ec==1 .and. dc==nd_cpl) .or. ec==ne)then ! early ret
           
            leis1=biggam
            leis2=biggam
            !lab = 0.0_dp
            !lab_eff = 0.0_dp
            if (ind_bc ) then
               
                cons=coh + sav
                cons = f_netcons(cons,tau_c)
                
            else
                cons=f_cons_dc(rhs_foc,0.0_dp,ec,dc)
                ! NET consumption
                !cons = f_netcons(cons,tau_c)

                coh=f_grid_coh_cpl(sav,cons,lab1,lab2,net_wage,wage_totax,ic,ec,dc,jc,tc,numchld,avearn,tau_c)
        
                ! check cons first-order condition:
                call sub_muc(cons,0.0_dp,mu_c)
                
                dist=abs(mu_c/rhs_foc-1.0_dp)
                if ( dist>epsi .and. cons>epsi   ) then                    
                    print*, 'first-order condition for consumption violated ret si2les',jc,ec,dc,net_wage(1)
                    pause
                endif
            endif
            
        else
            print*, "missed any cases?",ec,dc,ndlloc,gr_wage
            pause
            
        endif
       
    else    ! if (jc>=jr(tc)): retirement (after mand age)
        leis1=biggam
        leis2=biggam
        !print*, "ret", lab, lab_eff
        !pause
        
        !lab = 0.0_dp
        !lab_eff = 0.0_dp
        if (ind_bc .and. flag_invalid==0 ) then
            cons=coh + sav
            cons = f_netcons(cons,tau_c)
        else
            cons=f_cons_dc(rhs_foc,0.0_dp,ec,dc)
            ! NET consumption
           ! cons = f_netcons(cons,tau_c)
           
            coh=f_grid_coh_cpl(sav,cons,lab1,lab2,net_wage,wage_totax,ic,ec,dc,jc,tc,numchld,avearn,tau_c)
        
            ! check cons first-order condition:
            call sub_muc(cons,0.0_dp,mu_c)
            dist=abs(mu_c/rhs_foc-1.0_dp)
         
            if ( dist>epsi .and. cons>epsi  ) then
               
                print*, 'first-order condition for consumption violated ret mand si2les', jc,ec,dc,ecp1,dcp1,net_wage(1),ben_p(1),agg%wage(tc)
                pause
            endif

        endif
    endif
    
    util=f_util_cpl(cons,lab1_eff,lab2_eff,ec,dc,sc1,sc2,gen_id)
        
    
    ! endogenous assets
    ass=f_grid_ass_cpl(coh,bq(sc1,jc)+bq(sc2,jc),R,net_wage,ben_p,tau_k,ec,dc,jc,ic)
    assmi2r=f_grid_ass_cpl(coh,bq(sc1,jc)+bq(sc2,jc),R,net_wage*demo%grid_epsi(1),ben_p,tau_k,ec,dc,jc,ic)
    assmaxgr=f_grid_ass_cpl(coh,bq(sc1,jc)+bq(sc2,jc),R,net_wage*demo%grid_epsi(nw),ben_p,tau_k,ec,dc,jc,ic)

    
    !print*, ic,sc,gc,ass,coh
    !pause
    
    ! compute net income and net savi2
    call sub_incsav_cpl(ass,tr,cons,lab1,lab2,R,gr_wage,gammafe(sc1,kc1),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc,grwage,grwageinc,grinc,netinc,netsav, &
        labinctaxrv,capinctaxrv,constaxrv,penstaxrv,jc,tc,1,sa_tr,numchld,avearn,tau_c,tau_k,hsvtmp1,hsvtmp2,net_trr) !,agg%inc_thrs(tc))
                         
    ! value function: 

    !if (opt_input==1)then
    !    call sub_logsumprob(v_dc_in_intp(:,xc),v,prob,2,sigma_emp)
    !else
        v=f_vfun(util,ev_tp1,betta_til,demo%sr(jc,tc) )
    !endif
    v_nokid=f_vfun(util,ev_tp1_nokid,betta_til,demo%sr(jc,tc) )
                                
    ! inverse derivative of vfun w.r.t. coh
    call sub_muc(cons,0.0_dp,mu_c)
    ! inverse of derivative of value function w.r.t. coh 
    inv_vp_x=f_inv_vp_coh(mu_c) 
     
    end subroutine sub_solvehh_dec_cpl 
    ! ------------------------------------------------------------------- 
        
                
end subroutine sub_solvehh_cpl
! -------------------------------------------------------------------     
    
! ------------------------------------------------------------------- 
subroutine sub_solvehh(agg,demo,grid,pol,R,R_til_tp1,bq,b_til_tp1,kc,hc,wc,sc,gc,scpar,jc,tcp1,tc,gen_id,nj_in)
! solution of household policy and value functions
use ESPlot
implicit none
type(t_agg),intent(in)::agg 
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid 
type(t_pol),intent(inout)::pol
real(dp),intent(in):: R,R_til_tp1,bq(:,:),b_til_tp1(:,:)
integer,intent(in)::kc,sc,gc,jc,tcp1,tc,gen_id,hc,nj_in,scpar,wc

integer::xc,pc,yc,ec,ic,ycp1,wcp1,ecp1,icp1,dc,dcp1,ycp1_p,wcp1_p,yc_p,wc_p
integer:: dc_min,dc_max,ickid_init,gckid_init,ind_below,dc_in,dc_in_max,dc_str(nx)
real(dp)::betta_til
real(dp)::grid_ass(nx),grid_sav(nx)
real(dp)::util,gr_wage,net_wage,wage_totax,ben_p,gr_ben_p,mu_c,coh_min,minv_in,coh_in,temp_vec1(nx),temp_vec2(nx),prob_ni(np),grinc,ben_u,pstk,grpstk,savp,assmax,assmin
real(dp)::coh_tp1,coh
real(dp)::gr_wage_tp1,net_wage_tp1,wage_totax_tp1,pens_tp1,gr_pens_tp1,perfsort_flg
real(dp):: gr_wage1_tp1,gr_wage2_tp1,net_wage1_tp1,net_wage2_tp1
real(dp)::evp_coh_tp1,ev_tp1,ev_tp1_nokid
real(dp),parameter::epsi=1.0e-08
real(dp),parameter::tolf=1.0e-12
real(dp),parameter::min_x=1.0e-08
logical,parameter::opt_chk=.false., opt_chk_inv=.false.,opt_chk_ivt=.false.
logical::lb_viol,ub_viol,flg_all_inf 
real(dp)::rhs_foc,df
integer::nx0,xc0,flag_invalid,flag_conv,pc_tp1_or,pc_tp1_min,pc_tp1_max,kcp1,gc_p
real(dp)::vp_coh_tp1,v_tp1,v_tp1_nokid,x_min,x_max,x_grid(nx),cont_val
real(dp):: x_ref(nx)
integer:: num_sec
integer:: top(nx) !,ind_lab(nx)
real(dp),allocatable:: intersectionsx(:),intersectionsy(:)
integer:: dc_temp, dcl, dc_full, nd_glob,tinv_tc,opt_solve_parent
integer,allocatable:: dc_vec(:)
real(dp)::prob_dc(nd),muc,hp1,min_m,trans_1dim,trans_2dim,bc_sav,yroot,v_dc_in_intp(2),v_dc_in(2,nx),cons_dc_in(2,nx),cons_dc_in_intp(2,nx),lab_dc_in(2,nx),lab_dc_in_intp(2,nx),coh_dc_in(2,nx),sav_dc_in_intp(2,nx)
integer:: sc_p,kc_p,xc_p,pcloc
real(dp):: frac_p(nd),ev_tp1_vec_2dim(nx,np),ev_tp1_vec_2dim_nokid(nx,np),ev_tp1_vec_2dim_trans(nx,np),evp_coh_tp1_vec_2dim(nx,np),time_cost,grid_sav2dim(nx,np),t_glob,ass_exo(nx),logsum_m,prob_m(nminv),coh_endo_2dim(nx,np),cons_endo_2dim(nx,np),lab_endo_2dim(nx,np),lab_dc(nd),labeff_endo_2dim(nx,np),hp1_loc,inv_ces
integer:: ndlloc,gckid,ickid,bc,mc,pc_tp1,scp1,flag_stud
real(dp):: av_prod,av_prod_tp1,hkid_tp1,max_b,b_grid(nb),cons_endo(nx),coh_exo_min,coh_exo_max,coh_exo(nx),cons_temp,val_bc(nb),mcoh,sav_temp,ev_tp1_temp,util_temp,max_m,val_tc(ntinv),dist_foc,vfun_nokid
! for linear interpolation
integer::inds(2),indsh(2),inds2x(2,2)
real(dp)::vals(2),valsh(2),vals2x(2,2)
real(dp):: sav_p

! transformation of parameters:
betta_til=betta*(1.0_dp+demo%lamt(tc))**(1.0_dp-tetta)

! build exogenous asset grid for last period of life
if (max_x_fac>epsi) then
    grid_ass(1:nx-1)=makegrid(0.0_dp,max_x/max_x_fac,nx-1,3.0_dp)
    grid_ass(nx)=max_x
else
    grid_ass(1:nx)=makegrid(0.0_dp,max_x,nx,3.0_dp)
endif

!print*, "current age", jc
!pause
!!    

! determine MT threshold

!call fun_hsv_new_thrs(yroot,0.0_dp,0.0_dp,2,gc,1,1,1,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,avearn_base,ind_below )
!print*, "yroot", yroot, yroot/earn_si_lo
!pause

if (ng==2)then

    if (gc==1)then
        gc_p=2
    elseif (gc==2)then
        gc_p=1
    endif
else
    
    gc_p = gc
endif


flag_stud = 0

! problem in last period of life is de-generate: household consumes all resources
if (jc==nj_in) then
        
    if (opt_test_noer)then
        dc_min=nd
        dc_max=nd
    else
        dc_min=nd-2
        dc_max=nd
    endif
                
    do pc=1,1 ! child hc
        ec=ne
     
        do ic= 1,1
            do yc=1,ny
                  
                do dc=dc_min,dc_max ! retired TBC rewrite it generally allowing for endogenous retirement
                            
                    do xc=1,nx ! coh
                                
                        ! compute pension income
                        av_prod =demo%ageprod(gc,jr(tc)-1,sc )             
                        call sub_pens_new(gr_ben_p,agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc,tc),av_prod, & 
                            demo%grid_y(yc),0.0_dp,jc,gc,dc,ec,tc,sc,kc,yc,nd)
                        ben_p = fun_netpens(gr_ben_p,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
                           
                        ! coh today
                        coh=f_coh(grid_ass(xc),bq(sc,jc),R,0.0_dp,ben_p,ec,dc,1,sc,agg%tau_k(tc))
                              
                        pol%indbc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=0
                        grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=coh
                        grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=grid_ass(xc)
                        !grid%assmin(dc,xc,pc,yc,wc,ic,gc,kc,hc,sc,1,1,jc,tc)=f_grid_ass(grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),bq(sc,jc),R_til_tp1,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1) 
                        pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=f_netcons(coh,agg%tau_c(tc))
                        pol%leis(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=biggam
                        pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=0.0_dp
                        pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=0.0_dp
                        grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=0.0_dp 
                           
                        ! utility (value) function and its derivative
                        util=f_util(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),0.0_dp,0.0_dp,gc,ec,dc,sc,gen_id,0)
                        pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=f_vfun(util,0.0_dp,betta_til,1.0_dp)
                        pol%v_nokid(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc) = pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)

                        call sub_muc(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),0.0_dp,mu_c)
                            
                        ! inverse of derivative of value function w.r.t. coh
                        pol%inv_vp_x(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)=f_inv_vp_coh(mu_c)
                            
                        call sub_incsav(grid_ass(xc),0.0_dp,pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),0.0_dp,R,gr_wage,gammafe(sc,kc),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc,pol%grwage(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%grwageinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc), & 
                            pol%grinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%netinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%netsav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc), &
                            pol%labinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%capinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%constaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc), & 
                            pol%penstaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),gc,sc,jc,tc,1,ben_u, & 
                            demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),tau_k,pol%hsvtmp1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%hsvtmp2(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),pol%net_trr(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)) !,agg%inc_thrs(tc) )
                       
                    end do !enddo xc
                   
                enddo ! enddo dc
            enddo ! enddo yc                
        enddo    
      
    end do  ! end do pc
        
        
else    ! solution for all other ages
     
    do gckid=1,1 ! gender child
        do ickid=1,1! innate ability child
              
            do ic = 1,1
           
                do yc=1,ny  ! income states today (for retirees these are last working period income shocks)

                    do ec=ec_min(gen_id,jc,tc),ec_max(gen_id,jc,tc) ! emp states
                                   
                        ! for each ec, determine set of discrete choices available
                        if (ec==1)then ! worker state 
                         
                            ndlloc = ndl
                            pol%lab(:,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = 0.0_dp
                                
                            do dc=1,ndlloc
                                pol%lab(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = grid%lab(gc,dc,sc)
                                pol%lab_eff(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = grid%lab(gc,dc,sc) !grid%lab_base(gc,dc,sc)

                                !lab_eff_dc(dc) = grid%lab(gc,dc,sc) 
                                    
                                if (jc<jstud(sc) .and. sc>2)then
                                        
                                    if (sc==3)then
                                        flag_stud = 1
                                    else
                                       
                                        flag_stud = 2
                                    endif  
                                    
                                    time_cost =func_tcost(grid%hk_grid(hc),sc,scpar)
                                                                             
                                    !lab_eff_dc(dc) = pol%lab(dc,1,1,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)   
                                    pol%lab_eff(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = pol%lab(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) 
                                            
                                endif
                                    
                                    
                            enddo
                        
                            if ( opt_test_noer==1)then ! no end retirement
                            
                                allocate(dc_vec(ndlloc))
                                            
                                do dcl=1,ndlloc
                                    dc_vec(dcl)=dcl
                                enddo
                                            
                            elseif (opt_test_noer==0)then ! yes end retirement
                                
                                if (jc>=jer .and. jc<jr(tc))then
                                
                                    allocate(dc_vec(ndlloc+1))
                                    do dcl=1,ndlloc
                                        dc_vec(dcl) = dcl
                                    enddo
                                    dc_vec(ndlloc+1) = nd
                                    
                                else
                                
                                    allocate(dc_vec(ndlloc))                                            
                                    do dcl=1,ndlloc
                                        dc_vec(dcl)=dcl
                                    enddo
                                endif
                                                                            
                        
                            endif                                        
                                      
                      
                        elseif (ec==ne)then ! retired, no choices
                        
                            pol%lab(:,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = 0.0_dp
                            pol%lab_eff(:,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = 0.0_dp
                            !lab_eff_dc(:) = 0.0_dp 
                            
                            if (opt_test_noer)then ! no end retirement => put everybody in nd
                                allocate(dc_vec(1))
                                dc_vec(1)=nd
                            else ! end ret: nd-1 is ER, nd and nd-2 is LATE
                                if (jc>=jr(tc))then ! after mandatory age => late ret                            
                                    allocate(dc_vec(3))
                                    dc_vec = (/nd-2, nd-1, nd/)
                                elseif (jc>=jrr .and. jc<jr(tc))then ! regular ret SAME as LATE (only two retirement categories, but still keep it with three indices)
                                    allocate(dc_vec(2))
                                    dc_vec = (/nd-1, nd/)
                                elseif (jc>=jer .and. jc<jrr)then ! early ret
                                    allocate(dc_vec(1))
                                    dc_vec(1)=nd-1
                                else
                                    
                                    print*, "what else?"
                                    pause
                                endif
                                                                    
                            endif
                         
                           
                        endif                                
                                
                            
                            
                        do dc_temp=1,size(dc_vec)
                           
                            dc = dc_vec(dc_temp)
                        
                            ! wages and pensions
                            call sub_wagepens()
                             
                            do pc=1,pc_max(gen_id,jc)   ! child hkpc_tp1 = pcloc,pcloc
                                


                                if (jc<jr(1)  ) then
                                    dc_in_max = 2
                                else
                                    dc_in_max = 1
                                endif
                                
                                    
                                flag_invalid = 0
                                        
                                if (gen_id==1 .and. jc<jt .and. jc>=jf)then
                                    pc_tp1 = pc
                                else
                                    pc_tp1 = 1
                                endif
                                
                                pc_tp1_or = pc_tp1
                                
                                if (gen_id==1 .and. jc==jf-1)then
                                    if (opt_corr==1)then
                                        call basefun(grid%hk_grid,np,agg%h0distr(1,sc),vals,inds)
                                    else
                                        call basefun(grid%hk_grid,np,agg%h0distr(kc,sc),vals,inds)
                                    endif
                                    prob_ni = 0.0_dp
                                    prob_ni(inds(1) ) = vals(1)
                                    prob_ni(inds(2) ) = vals(2)
                                
                                    pc_tp1_min = inds(1)
                                    pc_tp1_max = inds(2)
                                    
                                else
                                    pc_tp1_min = pc_tp1_or
                                    pc_tp1_max = pc_tp1_or
                                    prob_ni = 0.0_dp
                                    prob_ni(pc_tp1) = 1.0_dp
                                    
                                endif
                                        
                                grid_sav(:)=f_grid_sav(xc0)
                                grid%sav(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)=grid_sav(:)
                                grid_sav2dim(:,pc) = grid_sav
                                
                                do dc_in = 1,dc_in_max
                                        
                                do xc=nx,xc0,-1
                                                     
                                    call sub_solvehh_sav(grid_sav(xc),.false.,flag_invalid,dc_in,0 )
                                        
                                    ev_tp1_vec_2dim(xc,pc) = ev_tp1
                                    ev_tp1_vec_2dim_nokid(xc,pc) = ev_tp1_nokid
                                        
                                    evp_coh_tp1_vec_2dim(xc,pc) = evp_coh_tp1
                                          
                                enddo
                                            
                           
                                ! borrowing constraint
                                if (xc0>1) then
                                       
                                    ! compute coh for zero assets 
                                    
                                    coh_min=f_coh(grid%amin_age(gc,sc,gen_id,jc-1),bq(sc,jc-1),R_til_tp1,net_wage,ben_p,ec,dc,ic,sc,agg%tau_k(tc))
                                    
                                                
                                    if (coh_min>grid%coh(dc,np1_aux,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)) then
                                                    
                                        coh_min=0.99_dp*grid%coh(dc,np1_aux,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)   ! use some value smaller than previous gridpoint
                                        flag_invalid=1
                                    else
                                        flag_invalid=0
                                    endif
                                        
                                    ! build aux grid (linear)
                                    grid%coh(dc,1:np1_aux,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)=makegrid(coh_min,grid%coh(dc,np1_aux,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),np1_aux,1.0_dp)
                            
                                    ! solve hh problem for exo part of coh grid
                                           
                                    do xc=1,np1_aux-1
                                        if (flag_invalid==0)then
                                                        
                                            call sub_solvehh_sav(grid_sav(xc),.true.,flag_invalid,dc_in,0)
                                        else
                                            call sub_solvehh_sav(grid_sav(xc),.false.,flag_invalid,dc_in,0)
                                        endif
                                            
                                        ev_tp1_vec_2dim(xc,pc) = ev_tp1
                                        ev_tp1_vec_2dim_nokid(xc,pc) = ev_tp1_nokid
                                        
                                        evp_coh_tp1_vec_2dim(xc,pc) = evp_coh_tp1
                                            
                                    enddo
                                                
                                           
                                endif
                                       
                                ! call upper envelope
                                temp_vec1=0.0_dp
                                temp_vec2=0.0_dp
                                call sub_secondary_env(pol%v_nokid(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),temp_vec1,temp_vec2, & 
                                    grid%coh(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),x_ref,intersectionsx,intersectionsy,num_sec,top)
                                call sub_secondary_env(pol%v(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%cons(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%inv_vp_x(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), & 
                                    grid%coh(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),x_ref,intersectionsx,intersectionsy,num_sec,top)
                                call sub_secondary_env(pol%v(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%lab(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%leis(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), & 
                                    grid%coh(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),x_ref,intersectionsx,intersectionsy,num_sec,top)
                        
                                ! update coh grid
                                grid%coh(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)=x_ref
                        
                                ! update asset grid (recompute it, not interpolate) 
                                
                                do xc=1,nx
                                    
                                    call sub_wagepens()                                    
                                                
                                    grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = f_grid_ass(grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),bq(sc,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                    !grid%assmin(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = f_grid_ass(grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),bq(sc,jc),R,net_wage*demo%grid_epsi(1),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                    !grid%assmax(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = f_grid_ass(grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),bq(sc,jc),R,net_wage*demo%grid_epsi(nw),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                                                     
     
                                    pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = evp_coh_tp1_vec_2dim(xc,pc)
                                    
                                   ! if (opt_ext_compute==1)then
                                        pol%vd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                        pol%consd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                        pol%labd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                        grid%cohd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                    !endif
                                    
                                    
                                    v_dc_in(dc_in,xc) = pol%vd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                    cons_dc_in(dc_in,xc) = pol%consd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                    lab_dc_in(dc_in,xc) = pol%labd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                    coh_dc_in(dc_in,xc) = grid%cohd(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                        
                                enddo ! enddo xc
                                
                                enddo
                                
                                if (dc_in_max > 1)then
                                    
                                    x_min = minval(coh_dc_in)
                                    x_max = maxval(coh_dc_in)
                                    
                                    x_grid =makegrid(x_min,x_max,nx,3.0_dp)
                                    
                                    do xc = 1,nx
                                         
                                        do dc_in = 1,2 
                                            
                                            v_dc_in_intp(dc_in) = func_intp_mod(coh_dc_in(dc_in,:),v_dc_in(dc_in,:),x_grid(xc),vals,inds) !,.false.,.false.)
                                            cons_dc_in_intp(dc_in,xc) = func_intp(coh_dc_in(dc_in,:),cons_dc_in(dc_in,:),x_grid(xc),vals,inds,.false.,.false.)
                                            lab_dc_in_intp(dc_in,xc) = func_intp(coh_dc_in(dc_in,:),lab_dc_in(dc_in,:),x_grid(xc),vals,inds,.false.,.false.)
                                            if (flag_stud==1)then
                                                sav_dc_in_intp(dc_in,xc) =  func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(dc_in,xc),cons_dc_in_intp(dc_in,xc), & 
                                                    (fee_flow - col_subs_flow) * 0.5_dp,1.0_dp,gc,sc,ec,dc,ic,jc,tc,ind_below) 
                                            elseif (flag_stud==2)then
                                                sav_dc_in_intp(dc_in,xc) =  func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(dc_in,xc),cons_dc_in_intp(dc_in,xc), & 
                                                    (fee_flow - col_subs_flow) * 1.0_dp,1.0_dp,gc,sc,ec,dc,ic,jc,tc,ind_below) 
                                            else
                                                sav_dc_in_intp(dc_in,xc) =  func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(dc_in,xc),cons_dc_in_intp(dc_in,xc), & 
                                                    0.0_dp,demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below) 
                                            endif
                                            cont_val = func_intp(grid_sav2dim(:,pc),ev_tp1_vec_2dim(:,pc),sav_dc_in_intp(dc_in,xc), vals,inds,.false.,.false.)
                                            util=f_util(cons_dc_in_intp(dc_in,xc),lab_dc_in_intp(dc_in,xc),0.0_dp,gc,ec,dc,sc,gen_id,flag_stud)
                                            v_dc_in_intp(dc_in) =f_vfun(util,cont_val,betta_til,1.0_dp)

                                            
                                            
                                           ! if (sav_dc_in_intp(dc_in,xc) < grid%amin_age(gc,sc,gen_id,jc) ) v_dc_in_intp(dc_in) = - 9000.0_dp
                                            !if (dc_in==2 .and. inds(1)<=np1_aux) v_dc_in_intp(dc_in) = - 9000.0_dp
                                        enddo
                                        
                                        if ( (v_dc_in_intp(2) - v_dc_in_intp(1)  )> 0.0001_dp)then
                                            dc_str(xc) = 2
                                        else
                                            dc_str(xc) = 1
                                        endif
                                        
                                        if (opt_ext_compute==1)then
                                            pol%dc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) =real(dc_str(xc))
                                            grid%coh_old(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) = grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)
                                        else
                                            dc_str(xc) = nint(func_intp(grid%coh_old(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%dc(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),x_grid(xc),vals,inds,.false.,.false.) )
                                            if (x_grid(xc) < grid%coh(dc,1,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc) ) dc_str(xc)   =1
                                        endif
                                        
                                        
                                        !if (dc_str(xc) == 2)then
                                        !    print*, "2", xc, x_grid(xc), coh_dc_in(2,:)
                                        !   ! pause
                                        !endif 
                                        
                                        
                                        !dc_str(xc) = maxloc(v_dc_in_intp,1)

!if (dc_str(xc) > 1) print*, "dcstr", dc_str(xc),lab_dc_in_intp(:,xc),cons_dc_in_intp(:,xc),v_dc_in_intp(:),func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(1,xc),cons_dc_in_intp(1,xc), & 
!                                            0.0_dp,demo%numchild_gsj(gc,sc,jc,tc),gc,ec,dc,ic,jc,tc,ind_below) , & 
!    func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(2,xc),cons_dc_in_intp(2,xc), & 
!                                            0.0_dp,demo%numchild_gsj(gc,sc,jc,tc),gc,ec,dc,ic,jc,tc,ind_below) 
                                        if (flag_stud==1)then
                                            grid_sav(xc) = func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(dc_str(xc),xc),cons_dc_in_intp(dc_str(xc),xc), & 
                                                (fee_flow - col_subs_flow) * 0.5_dp,1.0_dp,gc,sc,ec,dc,ic,jc,tc,ind_below) 
                                        elseif (flag_stud==2)then    
                                            grid_sav(xc) = func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(dc_str(xc),xc),cons_dc_in_intp(dc_str(xc),xc), & 
                                                (fee_flow - col_subs_flow) * 1.0_dp,1.0_dp,gc,sc,ec,dc,ic,jc,tc,ind_below) 
                                        else
                                            grid_sav(xc) = func_sav(x_grid(xc),net_wage,wage_totax,lab_dc_in_intp(dc_str(xc),xc),cons_dc_in_intp(dc_str(xc),xc), & 
                                                0.0_dp,demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below) 
                                        endif
                                        
                                        
                                    enddo
                                    
                                    grid%sav(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)=grid_sav(:)
                                    grid_sav2dim(:,pc) = grid_sav
                                    
                                    do xc=1,nx
                                                     
                                        call sub_solvehh_sav(grid_sav(xc),.false.,flag_invalid,dc_str(xc),1 )
                                        
                                        ev_tp1_vec_2dim(xc,pc) = ev_tp1
                                        ev_tp1_vec_2dim_nokid(xc,pc) = ev_tp1_nokid
                                        
                                        evp_coh_tp1_vec_2dim(xc,pc) = evp_coh_tp1
                                          
                                    enddo
                                    
                                endif
                                
                                
                            enddo ! enddo pc
                                    
                                        
                            if (gen_id==1 .and. gc==2 .and. jc==jt   .and. opt_ivt==1)then
                                                
                                opt_solve_parent=1
                                        
                                ! 2 dim objects from STEP ONE
                                coh_endo_2dim = grid%coh(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                cons_endo_2dim = pol%cons(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) 
                                lab_endo_2dim = pol%lab(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) 
                                labeff_endo_2dim = pol%lab_eff(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) 
                                        
                                do pc=1,pc_max(gen_id,jc)
                                    
                                    coh_exo_min = coh_endo_2dim(1,pc)
                                    coh_exo_max = coh_endo_2dim(nx,pc)
                                    
                                    coh_exo =makegrid(coh_exo_min,coh_exo_max,nx,3.0_dp)
                                    
                                    !grid%coh(dc,:,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = coh_exo
                                    
                                    do xc=1,nx
                                            
                                        ass_exo(xc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                            
                                        max_b= max(0.0_dp, coh_exo(xc) - net_wage * (biggam - pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ) ) 
                                                   
                                        b_grid = makegrid(0.0_dp,max_b,nb,1.0_dp)
                                        
                                        do bc = 1,nb
                                            
                                            val_bc(bc) = f_vfun_ivt(b_grid(bc))
                                            
                                        enddo
                                            
                                        call sub_max_vfun(b_grid,val_bc,pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%b(dc,xc,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc),.true.)
                                            
                                        ! after transfer coh
                                        mcoh = coh_exo(xc) -pol%b(dc,xc,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc) * demo%numchild_gsj(gc,sc,jc,tc)
                                            
                                        grid%coh_exo(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = coh_exo(xc) 
                                            
                                        pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_vfun_ivt(pol%b(dc,xc,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc))
                                        pol%v_nokid(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = vfun_nokid
                                            
                                        ! consumption by interpolating on (after transfer) consumption policy function
                                        !pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = func_intp(coh_endo_2dim(:,pc),cons_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        
                                          
                                        
                                        cons_temp  = func_intp(coh_endo_2dim(:,pc),cons_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        
                                        pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = cons_temp
                                        
                                        pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) =func_intp(coh_endo_2dim(:,pc),lab_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) =func_intp(coh_endo_2dim(:,pc),labeff_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                        
                                        if (pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) * net_wage <=agg%inc_thrs(tc) ) then
                                            ind_below = 1
                                        else
                                            ind_below= 0
                                        endif
                                        
                                        ! savings from budget constraint
                                        grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = & !func_intp(coh_endo_2dim(:,pc),grid_sav2dim(:,pc),mcoh,vals,inds,.false.,.false.)
                                            func_sav(coh_exo(xc),net_wage,wage_totax,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%b(dc,xc,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc), demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below )
                                            
                                        !print*, "test", grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc), func_intp(x_ref,grid_sav,mcoh,vals,inds,.false.,.false.)
                                        !pause
                                            
                                        ! check borrowing constraint
                                        bc_sav = grid_sav2dim(1,pc) 
                                        
                                        
                                        
                                            
                                        if (grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) < bc_sav)then
                                            cons_temp = coh_exo(xc) - bc_sav -  net_wage*(biggam-pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)) &
                                                - fun_hsv_new(wage_totax*pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,0.0_dp,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,ind_below )  & 
                                                - pol%b(dc,xc,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc) *  demo%numchild_gsj(gc,sc,jc,tc)
                                            
                                            pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = cons_temp/(1.0_dp + agg%tau_c(tc))
                                            
                                            grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = bc_sav
                                            !print*, "yes"
                                        endif
                                            
                                        ! assets, given coh
                                        grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                        !grid%assmin(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(1),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                        !grid%assmax(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(nw),ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                                                               
    
                                        ! rhs of euler equation 
                                        pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = func_intp(coh_endo_2dim(:,pc),evp_coh_tp1_vec_2dim(:,pc)**(-1.0_dp),mcoh,vals,inds,.false.,.false.)
                                        ! reinvert
                                        pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp)
                                            
                                    !    ! check of consumption euler eq
                                    !     if (grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) > bc_sav )then
                                    !        if (abs(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp) - betta*f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) *(1.0_dp + agg%tau_c(tc))) > epsi)then
                                    !            print*, "solve errrorrrr",jc, hc,grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                    !
                                    !             pause
                                    !        else
                                    !!            print*, "no errorrrr",jc
                                    !!            pause
                                    !        endif
                                    !    endif 
                                            
                                        ! inverse derivative of vfun w.r.t. coh
                                        call sub_muc(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,mu_c)
                                        ! inverse of derivative of value function w.r.t. coh 
                                        pol%inv_vp_x(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)=f_inv_vp_coh(mu_c)    
                                            
                                        call sub_checkbudget_parent(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%b(dc,xc,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc),pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            bq(sc,jc),net_wage,wage_totax,ben_p,R,ic,ec,dc,.false.,gc,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc))  
                                        
                                        call sub_incsav(grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),R,gr_wage,gammafe(sc,kc),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc, & 
                                            pol%grwage(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%grwageinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &  
                                            pol%grinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%netinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%netsav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            pol%labinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%capinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%constaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%penstaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),gc,sc,jc,tc,1,ben_u, & 
                                            demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),tau_k,pol%hsvtmp1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%hsvtmp2(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%net_trr(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)) !,agg%inc_thrs(tc))
                                          
                                    enddo
                                                
                                    !print*, "finished"
                                    !pause
                                                
                                    if (opt_chk_ivt .and. sc==3)then
                                        
                                        print*, "bpol", pol%b(dc,1:nx,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc)
                                        pause
                                    
                                        call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%cons(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,'cons','cons')
                                        call execplot()
                                        call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%b(dc,1:nx,pc,yc,ec,wc,gc,kc,sc,hc,gckid,tc),'b','b')
                                        call execplot()
                                                
                                    endif
                                enddo
                                        
                                grid%coh(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = grid%coh_exo(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)       
                                        
                            elseif ((gen_id==1 .and. gc==2. .and. opt_inv==1 ).and. (jc<jt .and. jc>=jf))then
                                        
                                ! 2 dim objects from STEP ONE
                                coh_endo_2dim = grid%coh(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                cons_endo_2dim = pol%cons(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) 
                                lab_endo_2dim = pol%lab(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) 
                                labeff_endo_2dim = pol%lab_eff(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) 
                                        
                                ! STEP TWO
                                opt_solve_parent=2 
                                        
                                do pc=1,pc_max(gen_id,jc) ! currrent period HK
                                         
                                    ! span exogenous coh grid
                                    coh_exo_min =minval(coh_endo_2dim(1,:))
                                    coh_exo_max = maxval(coh_endo_2dim(nx,:))
                                    
                                    coh_exo = makegrid(coh_exo_min,coh_exo_max,nx,3.0_dp)
                                    
                                    
                                    grid%coh_exo(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = coh_exo
                                           
                                    do xc=1,nx
                                        
                                        ! exogenous assets
                                        ass_exo(xc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                            
                                        flg_all_inf = .true.
                                        
                                        ! min and max mon investment feasible 
                                        min_m = 0.0_dp
                                        grinc = net_wage * pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        if (grinc<=agg%inc_thrs(tc) ) then
                                            ind_below=1
                                        else
                                            ind_below=0
                                        endif
                                        
                                        max_m = coh_exo(xc) - minval(grid_sav2dim) -  net_wage*(biggam-pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)) &
                                            - fun_hsv_new(wage_totax*pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,0.0_dp,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,ind_below )  & 
                                            - epsi

                                        ! loop over possible time inv choices (on grid, then go between points)
                                        do tinv_tc = 1,ntinv
                                                
                                            val_tc(tinv_tc) = f_vfun_tinv(grid%tinv_grid(tinv_tc))
                                                    
                                            if ( val_tc(tinv_tc) > -9000.0_dp )then
                                                flg_all_inf = .false.
                                            endif
                                                    
                                        enddo
                                              
                                        if (flg_all_inf==.true. .or. opt_invconst==1)then ! no positive investment choices are feasible [for completeness, should not have any positive weight here]
                                            pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)= 0.0_dp
                                            pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)= 0.0_dp
                                            mcoh = coh_exo(xc)
                                            ! next period HK 
                                            hp1_loc = f_hh_tp1(grid%hk_grid(pc),gamma_s(sc),pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                                pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),jc-jf+1,inv_ces) 
                                            cons_temp  = f_hyb2d_inter(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,cons_endo_2dim)
                                            ! reinvert
                                           
                                            pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)  =cons_temp 
                                            muc = pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp)
                                        else                                                                                                      
                                            ! pick maximum                                            
                                            call sub_max_vfun(grid%tinv_grid,val_tc,pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),.true.)                                            
                                            
                                            call sub_solvemoney(grid%hk_grid(pc),coh_exo(xc),net_wage,wage_totax,pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                                pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),muc,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                                demo%numchild_gsj(gc,sc,jc,tc),mcoh,hp1,cons_temp,grid%hk_grid,cons_endo_2dim,lab_endo_2dim,labeff_endo_2dim,coh_endo_2dim,evp_coh_tp1_vec_2dim,grid_sav2dim,f_aftertaxR(1.0_dp + agg%ret(tc),tau_k),gc,ec,dc,ic,jc,tc,min_m,max_m,flag_conv)  
                                            pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = cons_temp
                                            if (flag_conv==0) print*, "no money conv",jc,xc,pc
                                        endif
                                         
                                        ! after inv coh
                                        mcoh = coh_exo(xc) -pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) * demo%numchild_gsj(gc,sc,jc,tc)
                                        
                                        ! next period HK 
                                        hp1_loc = f_hh_tp1(grid%hk_grid(pc),gamma_s(sc),pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),jc-jf+1,inv_ces) 
                                        
                                        pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,cons_endo_2dim ,vals2x,inds2x,valsh,indsh) 
                                        
                                        pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,lab_endo_2dim ,vals2x,inds2x,valsh,indsh)
                                        pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) =  f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,labeff_endo_2dim ,vals2x,inds2x,valsh,indsh)
            
                                        if (isnan(hp1_loc))then
            
                                            print*,"hp1 nan", hp1_loc, grid%hk_grid(pc),1.0_dp,pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                            pause
                                        endif
            
                                        if (pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) * net_wage < agg%inc_thrs(tc) ) then
                                            ind_below = 1
                                        else
                                            ind_below = 0
                                        endif
                                        
                                        ! savings from the budget constraint
                                        grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = & ! f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,grid_sav2dim(:,:) ,vals2x,inds2x,valsh,indsh) 
                                            func_sav(coh_exo(xc),net_wage,wage_totax,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below )
                                        
                                        !print*, "comp", grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,grid_sav2dim(:,:) ,vals2x,inds2x,valsh,indsh),pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        !pause
                                        
                                        ! check borrowing constraint                                        
                                        bc_sav = func_intp(grid%hk_grid,grid_sav2dim(1,:),hp1_loc,vals,inds,.false.,.false.)
                                        
                                        
                                        !! intratemporal foc error
                                        !call sub_fochk_check(pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) , & 
                                        !    pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) ,pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), demo%numchild_gsj(gc,sc,jc,tc),gc,jc,dist_foc)
                                        !if (abs(dist_foc)>0.0001_dp .and. pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)>0.0_dp .and. pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)>0.0_dp)then
                                        !    print*, "beeep", dist_foc 
                                        !endif        
                                        
                                        !grid%sav(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) =  & !f_hyb2d_inter_mod(mcoh,hp1_loc,coh_endo_2dim,grid%hk_grid,grid_sav2dim(:,:) ,vals2x,inds2x,valsh,indsh) 
                                        !    func_sav(coh_exo(xc),net_wage,pol%lab(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                        !    pol%cons(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) )
                                            
                                        
                                        pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_vfun_tinv_eval(pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%v_nokid(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc))
                                        
                                        
                                        ! assets, given coh
                                        grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage,ben_p,agg%tau_k(tc),ec,dc,jc,1)
                                        !grid%assmin(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(1),ben_p,agg%tau_k(tc),ec,dc,jc,1)                                        
                                        !grid%assmax(dc,xc,pc,yc,ec,ic,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_grid_ass(coh_exo(xc),bq(sc,jc),R,net_wage*demo%grid_epsi(nw),ben_p,agg%tau_k(tc),ec,dc,jc,1)                                        


                                        ! save rhs of euler equation, for computing euler errors later
                                        pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = f_hyb2d_inter_mod(mcoh ,hp1_loc, & 
                                            coh_endo_2dim,grid%hk_grid,evp_coh_tp1_vec_2dim**(-1.0_dp),vals2x,inds2x,valsh,indsh) 
                                        pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) =  pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp)
                                        
                                        !print*, ev_tp1_vec_2dim_trans(10,:),trans_2dim
                                        !pause
                                        
                                        !if (abs(grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) - bc_sav)>epsi )then
                                        !    if (abs(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp) - betta*f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * &
                                        !        pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) *(1.0_dp + tau_c) ) > epsi)then
                                        !        print*, "solve errrorrrr",jc, pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)**(-1.0_dp),  betta* f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) *pol%evtp1_coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), hp1_loc, & 
                                        !        pol%t1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) , bc_sav
                                        !
                                        !        ! pause
                                        !    else
                                        !            !           print*, "no errorrrr",jc
                                        !        !            pause
                                        !    endif
                                        !endif
                                        
                                        
                                        ! inverse derivative of vfun w.r.t. coh
                                        call sub_muc(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,mu_c)
                                        ! inverse of derivative of value function w.r.t. coh 
                                        pol%inv_vp_x(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)=f_inv_vp_coh(mu_c)    
                                            
                                        call sub_checkbudget_parent(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            grid%sav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            bq(sc,jc),net_wage,wage_totax,ben_p,R,ic,ec,dc,.false.,gc,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc))  
                                            
                                        call sub_incsav(grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),R,gr_wage,gammafe(sc,kc),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc, & 
                                            pol%grwage(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%grwageinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%grinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%netinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%netsav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), &
                                            pol%labinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%capinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%constaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
                                            pol%penstaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),gc,sc,jc,tc,1,ben_u, & 
                                            demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),tau_k,pol%hsvtmp1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%hsvtmp2(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%net_trr(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc)) !,agg%inc_thrs(tc))
                                            
                                    enddo
                                           
                                    if (opt_chk_inv .and. sc==2)then
                                        print*, "pol m", pol%m(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),dc,pc,hc
                                        pause
                                        print*, "pol t", pol%t1(dc,:,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                                        pause
                                            
                                        call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%cons(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'cons','cons')
                                        call execplot()
                                        call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%m(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'m','m')
                                        call execplot()
                                        call plot_xy(grid%coh(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%t1(dc,1:nx,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'t1','t1')
                                        call execplot()
                                    endif
                                        
                                enddo
                                    
                                        
                                ! END TBC
                                grid%coh(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = grid%coh_exo(dc,:,:,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)        
                            endif
                                        
                            
                          !  endif
                                    
                                
                        
                        enddo ! enddo dc
                   
                        deallocate(dc_vec)   
                          
                        ! if (opt_chk .and. (jc<(jr(tc)-1)) ) then
                        if ( opt_chk .and. gen_id==1 .and. jc==jf-2 .and. sc<3 ) then
                        ! if (opt_chk) then
                             
                            call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(gc,jc,sc),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),sc,kc,dc,ec,jc,gen_id,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
                                
                            nx0=nx   
                            !print*, 'counters: ', yc,ec,ic,jc,kc,sc, net_wage
                            pc=1
                            dc=1! for plotting
                            pc=1
                    
                            print*, 'assets: ', sc
                            print*, grid%ass(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                            print*, 'coh: '
                            print*, grid%coh(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                            print*, 'cons: '
                            print*, pol%cons(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                            print*, 'lab: '
                            print*, pol%lab(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                            print*, 'sav: '
                            print*, grid%sav(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)
                            !pause
                                    
                            !call plot_xy(grid%coh(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%cons(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'cons','cons')
                            !call execplot()
                            !        
                            !call plot_xy(grid%coh(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%leis(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'leis','leis')
                            !call execplot()
                            !        
                            !call plot_xy(grid%coh(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%inv_vp_x(dc,1:nx0,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),'vp-coh','vp-coh')
                            !call execplot()
                        endif
                        
                    end do  ! end do ec
                end do  ! end do yc
            enddo
           
        enddo
    enddo
        
                    
endif   ! end if jc


contains

    ! -------------------------------------------------------------------
    subroutine sub_wagepens()

    implicit none
    
    ! compute wages and pension benefits today 
    gr_wage=0.0_dp
    net_wage=0.0_dp                                                
    ben_p=0.0_dp
                            
    av_prod = demo%ageprod(gc,jr(tc)-1,sc )                   
    if (ec==1) then 
       
            
            if (sc>2 .and. jc<jstud(sc) ) then
           
                call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(1,tc),demo%ageprod(gc,jc,1),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),1,kc,dc,ec,jc,gen_id,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                
            else
                
                call sub_wage(gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),demo%ageprod(gc,jc,sc),demo%grid_y(yc),grid%hk_grid(hc),agg%tau_p(tc),sc,kc,dc,ec,jc,gen_id,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc))
                
            endif
            
            gr_wage = gr_wage * demo%grid_epsi(wc)
            net_wage = net_wage * demo%grid_epsi(wc)
            wage_totax = wage_totax * demo%grid_epsi(wc)
                              
                              
    else 
        call sub_pens_new(gr_ben_p,agg%rho_p(tc),agg%tau_p(tc),0.0_dp,agg%avwage(tc),agg%pia0(gc,1,sc,tc),av_prod , & 
            demo%grid_y(yc),0.0_dp,jc,gc,dc,ec,tc,sc,kc,yc,nd)
        ben_p = fun_netpens(gr_ben_p,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn) 
    endif
    
    end subroutine sub_wagepens
    ! -------------------------------------------------------------------


    ! -------------------------------------------------------------------
    subroutine sub_max_vfun(grid_b,grid_w,w_max,b_max,opt_acc)
    ! pick max of grid_w, evaluated on grid_b

    implicit none

    real(dp),intent(in)::grid_b(:),grid_w(:)
    real(dp),intent(out)::w_max,b_max
    integer::bc
    real(dp)::ax,bx,cx,stp,w_alt,sav_check(nx), v1, v2, v3, bx2, stp1, stp2,cons,sav
    integer::ns, bc2,ind_bc
    real(dp),parameter::stp_min=1.0e-04
    integer,parameter:: case_meth=2
    logical,intent(in):: opt_acc ! for vfi
    logical :: fflag

    ! pick and index maximum
    w_max=maxval(grid_w)
    bc=indmax(grid_w,w_max)

    b_max=grid_b(bc)
    
    if (not(opt_acc) ) then
        return
    else    
        ! proceed with solver in interval:
        ns=size(grid_w,1)
    
        if (bc==ns) then
            ! check if there is an increase of objective for marginal change:
            bx=grid_b(ns)-stp_min
            if (opt_solve_parent==1)then
                w_alt=f_vfun_ivt(bx)
            elseif (opt_solve_parent==2)then
                w_alt=f_vfun_tinv(bx)
            endif
            
            if (w_alt<w_max) return            
            
            ! check if v is increasing between grid points that is if grid was too coarse
            stp=(grid_b(ns)-grid_b(ns-1))/2.0_dp  
            bx=grid_b(ns)-stp
            ax=grid_b(ns)
            cx=grid_b(ns-1)
        
        elseif (bc==1) then
            ! check if there is an increase of objective for marginal change:
            bx=grid_b(1)+stp_min
            if (opt_solve_parent==1)then
	            w_alt=f_vfun_ivt(bx)     
            elseif (opt_solve_parent==2)then
                w_alt=f_vfun_tinv(bx)
            endif
            
            if (w_alt<w_max) return
            
            stp=(grid_b(2)-grid_b(1))/2.0  
            bx=grid_b(1)+stp
            ax=grid_b(2)
            cx=grid_b(1)
        else
            bx=grid_b(bc)
            ax=grid_b(bc+1)
            cx=grid_b(bc-1)   
        endif
    
        ! compute solution in between grid points
        stp1=bx-ax
        stp2=cx-bx
 
        if (case_meth==1) then
            w_max=golden(ax,bx,cx,func_minw,tolf,b_max)
        elseif (case_meth==2) then
            w_max=brent(ax,bx,cx,func_minw,tolf,b_max,fflag)
        endif
    
        w_max=-w_max
        
    endif
       
    end subroutine sub_max_vfun
    ! -------------------------------------------------------------------
    
    ! -------------------------------------------------------------------
    function func_minw(b)
    
    implicit none
    
    real(dp)::func_minw
    real(dp),intent(in)::b
    real(dp):: cons,sav
    integer:: ind_bc
    
    if (opt_solve_parent==1)then
    
        func_minw=-f_vfun_ivt(b)
    elseif (opt_solve_parent==2)then
        func_minw=-f_vfun_tinv(b)
    endif
    
    
    end function func_minw
    ! -------------------------------------------------------------------


    ! -------------------------------------------------------------------
    function f_vfun_ivt(bx)
    ! compute for b on as well as outside the b-grid

    USE CSHER_INT
    USE UMACH_INT
    USE CSVAL_INT
    use pchip_module
    
    implicit none

    real(dp):: f_vfun_ivt
    real(dp),intent(in):: bx
    real(dp):: mcoh,cons_temp,sav_temp,util_temp, ev_tp1_temp,prob_edu_kid(ns),ev_kid(ns),v_kid,v_kid_dc(ndl),logsum_kid,prob_kid(ndl),logsum_kid_educ,prob_kid_educ(ns),coh_kid,net_wage_kid,wage_totax_kid,gr_wage_kid,bnet,val_sc(ns)
    integer:: gc_kid,sc_kid_min,sc_kid_max,dc_kid,sc_kid,kc_kid,yc_kid,xc_loc,xc_sw,sc_loc,ierr,wc_kid, ind_below
    real(dp):: ass_kid,ass_kid_netfee, sav_temp2,high_sh(nx),coh_loc,vkid_s_intp(ns),prob_s_loc(ns),logsum_loc, dist_test,csbreak_sh(nx),cscoeff_sh(4,nx),v_lin,xe(1),fe(1),prob_fin(2),prob_fin_hs(2),val_col,prob_col(2),ev_kid_choice(2),probfe(nk),lab_eff,lab
    logical:: skip
    
    mcoh = coh_exo(xc) -bx * demo%numchild_gsj(gc,sc,jc,tc)
    
    lab  = func_intp(coh_endo_2dim(:,pc),lab_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    lab_eff  = func_intp(coh_endo_2dim(:,pc),labeff_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    
    pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = lab
    pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc) = lab_eff
    
    if (lab * net_wage <= agg%inc_thrs(tc) ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
    
    cons_temp  = func_intp(coh_endo_2dim(:,pc),cons_endo_2dim(:,pc),mcoh,vals,inds,.false.,.false.)
    !cons_temp = cons_temp**(-1.0_dp)
    !cons_temp =( cons_temp *  f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * betta*demo%sr(jc,tc)   )**(-1.0_dp)
    sav_temp =func_sav(coh_exo(xc),net_wage,wage_totax,lab,cons_temp,bx,demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below) !,agg%inc_thrs(tc) ) 
    
    bc_sav = grid_sav2dim(1,pc) 
    
    if (sav_temp < bc_sav)then
        cons_temp = coh_exo(xc) - bc_sav -  net_wage*(biggam-lab) &
            - fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,ind_below )  &  !,avearn_base,ind_below
        - bx *  demo%numchild_gsj(gc,sc,jc,tc)
        
        sav_temp = bc_sav
    endif
  
                                          
    if (mcoh < coh_endo_2dim(1,pc) .or. sav_temp < grid_sav2dim(1,pc) )then
        f_vfun_ivt = -90000.0_dp
    else
         
        dist_test = abs(sav_temp - func_intp(coh_endo_2dim(:,pc),grid_sav2dim(:,pc),mcoh,vals,inds,.false.,.false.) ) 
        
        ass_kid_netfee =  (bx )/f_aftertaxR(R,tau_k)
        sc_kid_max  = ns
            
        util_temp = f_util(cons_temp,lab_eff,0.0_dp,gc,ec,dc,sc,gen_id,0)
                                            
            ev_tp1_temp = func_intp(grid_sav2dim(:,pc),ev_tp1_vec_2dim(:,pc),sav_temp,vals,inds,.false.,.false.)
            !ev_tp1_temp = func_intp(grid_sav,ev_tp1_vec,sav_temp,vals,inds,.false.,.false.)
          !  ev_tp1_temp = func_intp(grid_sav,ev_tp1_vec_trans,sav_temp,vals,inds,.false.,.false.)
          !  ev_tp1_temp = log(ev_tp1_temp * trans_1dim)
                          
            f_vfun_ivt = f_vfun(util_temp,ev_tp1_temp,betta_til,demo%sr(jc,tc) )
        
            ev_kid = 0.0_dp
            
            do gc_kid = 1,ng
            
            do sc_kid=1,sc_kid_max
                
            do yc_kid=1,ny
                do wc_kid = 1,nw
                    
                    if (nk>1)then
                        probfe(nk) = f_probfe(grid%hk_grid(pc),sc_kid,agg%gammah_s(sc_kid,tc))
                        probfe(1) = 1.0_dp -probfe(nk) 
                    else
                        probfe(1) = 1.0_dp
                    endif 
                    
                do kc_kid=1,nk
                        
                    val_sc = 0.0_dp 
                        
                    !do sc_kid=1,sc_kid_max
                        ! transfer net 
                        bnet = bx
                        
                        do dc_kid=1,ndl
                            
                            ! wage of kid: ALL kids work for no wages
                            call sub_wage(gr_wage_kid,net_wage_kid,wage_totax_kid,agg%wage_s(1,tc),demo%ageprod(gc_kid,js,1),demo%grid_y(yc_kid),grid%hk_grid(pc),agg%tau_p(tc),1,kc_kid,dc_kid,1,js,2,agg%gammah_s(1,tc),agg%abgrad_s(:,1),agg%hk_cutoff(:,tc))
                            
                            gr_wage_kid = gr_wage_kid * demo%grid_epsi(wc_kid)
                            net_wage_kid = net_wage_kid * demo%grid_epsi(wc_kid)
                            wage_totax_kid = wage_totax_kid * demo%grid_epsi(wc_kid)
                            
                            ! kids coh
                            coh_kid = bnet + bq(sc_kid,js+1) + net_wage_kid
                  
                                
                                if ( coh_kid < grid%coh_child(dc_kid,1,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc) )then
                                    v_kid_dc(dc_kid) = -90000.0_dp
                                    v_lin = v_kid_dc(dc_kid)
                                else
                                    
                                
                                v_kid_dc(dc_kid) = func_intp_mod(grid%coh_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc),pol%v_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc),coh_kid  ,vals,inds) !,.false.,.false.)
                                
                                v_lin = v_kid_dc(dc_kid)
                                
                                if (opt_pchip==1)then
                                
                                    if (grid%coh_child(dc_kid,np1_aux-1,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc) == grid%coh_child(dc_kid,np1_aux-2,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc) ) then
                                        
                                        !call d_csher(grid%coh_child(dc,np1_aux:nx,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),pol%v_child(dc,np1_aux:nx,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                        !    pol%cons_child(dc,np1_aux:nx,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),grid%break_sh_child_sh(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                        !    grid%coeff_sh_child_sh(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc))
                                        !    
                                        !v_kid_dc(dc_kid) =  d_csval(coh_kid,grid%break_sh_child_sh(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                        !    grid%coeff_sh_child_sh(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc))
                                        !
                                        !print*, "so1", v_lin, v_kid_dc(dc_kid)
                                    
                                        call dpchim (nx-np1_aux+1, grid%coh_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            pol%v_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), pol%vder_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            1, ierr)
                                    
                                        xe(1) = coh_kid
                                        skip = .true.
                                    
                                        call dpchfe (nx-np1_aux+1, grid%coh_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            pol%v_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), pol%vder_child(dc_kid,np1_aux:nx,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            1, skip, 1, xe, fe, ierr)
                                        
                                        v_kid_dc(dc_kid) = fe(1)
                                    
                                        !print*, "so2", v_lin, v_kid_dc(dc_kid)
                                        !pause
                                    else
                                        
                                                                            
                                        !call d_csher(grid%coh_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),pol%v_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                        !    pol%cons_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc),grid%break_sh_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                        !    grid%coeff_sh_child(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc))
                                        !    
                                        !csbreak_sh = grid%break_sh_child(dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc)
                                        !cscoeff_sh = grid%coeff_sh_child(:,dc,:,1,yc_kid,1,ickid,gckid,kc_kid,pc,sc_kid,1,sc,js,tc)
                                        !
                                        !v_kid_dc(dc_kid) =  d_csval(coh_kid,csbreak_sh,cscoeff_sh)
                                    
                                        call dpchim (nx, grid%coh_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            pol%v_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), pol%vder_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            1, ierr)
                                    
                                        xe(1) = coh_kid
                                        skip = .true.
                                    
                                        call dpchfe (nx, grid%coh_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            pol%v_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), pol%vder_child(dc_kid,:,1,yc_kid,1,wc_kid,gc_kid,kc_kid,pc,sc_kid,1,sc,js,tc), & 
                                            1, skip, 1, xe, fe, ierr)
                                        
                                        v_kid_dc(dc_kid) = fe(1)
                                        
                                    endif
                                    
                                    
                                endif
                                
                                
                            
                            !if (opt_pchip==1)then
                            !    if (v_kid_dc(dc_kid) < vals(1) .or. v_kid_dc(dc_kid) > vals(2) ) v_kid_dc(dc_kid) = v_lin
                            !endif
                            
                            endif
                            
                            
                        enddo
                        
                        call sub_logsumprob(v_kid_dc,logsum_kid,prob_kid,ndl,sigma_emp)
                        
                        val_sc(sc_kid)  =logsum_kid 
                                              
                        !print*, "evkid",ev_kid
                        !pause
                    !enddo
                    
                    
                    
                    
                    !print*, "child1", pol%v_guess(dc,7,1,yc_kid,1,ickid,gckid,kc_kid,:,1,1,1,tc)
                    !pause
                    !print*, "child2", pol%v_guess(dc,7,1,yc_kid,1,ickid,gckid,kc_kid,:,3,1,1,tc)
                    !pause
                        
                    ev_kid(sc_kid) = ev_kid(sc_kid) + val_sc(sc_kid) *  demo%pini(yc_kid)  * demo%prob_epsi(wc_kid) * probfe(kc_kid) * ng**(-1.0_dp)
                enddo
                enddo
            enddo 
            
            enddo
            enddo
            
            if (opt_corr==1)then
    			prob_kid_educ = 0.0_dp 
    			! first DROPOUT DECISION, with taste shock
    			call sub_logsumprob(ev_kid(ns-1:ns),logsum_kid_educ,prob_col,2,sigma_emp*0.0_dp)
    			! now, to those willing to graduate, apply lottery
    			prob_fin(1) = f_probfin(grid%hk_grid(pc)) ! CL completion prob
    			prob_fin(2) = 1.0_dp - prob_fin(1) ! CL dropout prob

    			prob_fin(1) = prob_col(2) * prob_fin(1)
    			prob_fin(2) = 1.0_dp - prob_fin(1)

            
               prob_fin_hs(1) = f_probfin_hs(grid%hk_grid(pc))
               prob_fin_hs(2) = 1.0_dp - prob_fin_hs(1)
            
               val_col = prob_fin(1) * ev_kid(ns) + prob_fin(2) * ev_kid(ns-1)
               prob_col = 0.0_dp
            
               ev_kid_choice(1)= ev_kid(2)
               ev_kid_choice(2) = val_col
                        
               call sub_logsumprob(ev_kid_choice,logsum_kid_educ,prob_col,2,sigma_emp)
                    
               prob_kid_educ(1) = prob_fin_hs(2) ! hs dropout
               prob_kid_educ(2)=prob_fin_hs(1) * prob_col(1) ! hs graduate: choose no college
               prob_kid_educ(3)=prob_fin_hs(1) *prob_col(2) * prob_fin(2) ! dropout (hs grad, chose college, dropout)
               prob_kid_educ(4)=prob_fin_hs(1) *prob_col(2) * prob_fin(1) ! finish (hs grad, chose college, finish)

               logsum_kid_educ = 0.0_dp
               do sc_kid=1,ns
                  logsum_kid_educ = logsum_kid_educ + prob_kid_educ(sc_kid) * ev_kid(sc_kid)
               enddo
           
               vfun_nokid =f_vfun_ivt
               f_vfun_ivt = f_vfun_ivt + nu_param * logsum_kid_educ
           
            else
           
                prob_kid_educ = 0.0_dp
                prob_fin(1) = f_probfin(grid%hk_grid(pc))
                prob_fin(2) = 1.0_dp - prob_fin(1)
                val_col = prob_fin(1) * ev_kid(ns) + prob_fin(2) * ev_kid(ns-1)
                prob_col = 0.0_dp
            
                ev_kid_choice(1)= ev_kid(1)
                ev_kid_choice(2) = val_col
                        
                call sub_logsumprob(ev_kid_choice,logsum_kid_educ,prob_col,2,sigma_emp)
                    
                prob_kid_educ(1)=prob_col(1)
                prob_kid_educ(2)=prob_col(2) * prob_fin(2) ! dropout
                prob_kid_educ(3)=prob_col(2) * prob_fin(1) ! finish
           
                vfun_nokid =f_vfun_ivt
                f_vfun_ivt = f_vfun_ivt + nu_param * logsum_kid_educ
             
            
           endif
        
        
    endif
   
    end function f_vfun_ivt
    ! -------------------------------------------------------------------

    !real(dp):: func_sav
    !real(dp),intent(in):: coh,netwage,lab,cons,expend
    !
    !func_sav = coh - cons*(1.0_dp+tau_c) - netwage*(biggam-lab) - fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn )  & 
    !    - expend *  demo%numchild_gsj(gc,sc,jc,tc)
    !
    !end function func_sav
    !! -------------------------------------------------------------------
    
    ! -------------------------------------------------------------------
    function f_vfun_tinv(tx)
    ! compute value function for given time input
    use pchip_module
    
    implicit none

    real(dp):: f_vfun_tinv
    real(dp),intent(in):: tx
    real(dp):: mcoh,cons_temp,sav_temp,util_temp, ev_tp1_temp,prob_edu_kid(ns),ev_kid,v_kid,v_kid_dc(ndl),logsum_kid,prob_kid(ndl),coh_kid,net_wage_kid,gr_wage_kid,bnet,mloc,muc,mul,t1,t2,hp1,val_tinv(ntinv),val_t_str
    integer:: sc_kid_min,sc_kid_max,dc_kid,sc_kid,kc_kid,yc_kid,tinv_loc,ierr,hcloc,ind_below
    real(dp):: logsum_t1,prob_t1(ntinv),mx,bc_sav,fe(1),xe(1),vder_temp_h(nx,np),vder_temp(np),ev_tp1_temp_h(np),lab,lab_eff
    logical:: skip
    
    ! correction for infeasible choices
    if (max_m <=0.0_dp  )then
        mx = 0.0_dp 
        mcoh = coh_exo(xc)
        
    else
        ! compute mon inv, given time, using FOCs
        call sub_solvemoney(grid%hk_grid(pc),coh_exo(xc),net_wage,wage_totax,mx,tx,muc,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc), & 
            demo%numchild_gsj(gc,sc,jc,tc),mcoh,hp1,cons_temp,grid%hk_grid,cons_endo_2dim,lab_endo_2dim,labeff_endo_2dim,coh_endo_2dim,evp_coh_tp1_vec_2dim,grid_sav2dim,f_aftertaxR(1.0_dp + agg%ret(tc),tau_k),gc,ec,dc,wc,jc,tc,min_m,max_m,flag_conv)  
      
    endif    
    
    ! next period HK, given inputs
    hp1 = f_hh_tp1(grid%hk_grid(pc),1.0_dp,mx,tx,jc-jf+1,inv_ces) 
    
    cons_temp  = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,cons_endo_2dim)
    !cons_temp = cons_temp**(-1.0_dp)
    !cons_temp =( cons_temp *  f_aftertaxR(1.0_dp + agg%ret(tc),tau_k) * betta*demo%sr(jc,tc)  )**(-1.0_dp)
    
    lab_eff = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,labeff_endo_2dim)
    lab = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,lab_endo_2dim)
    
    if (lab * net_wage <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
    sav_temp =func_sav(coh_exo(xc),net_wage,wage_totax,lab,cons_temp,mx,demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below) !,agg%inc_thrs(tc) ) 
    
    bc_sav = func_intp(grid%hk_grid,grid_sav2dim(1,:),hp1,vals,inds,.false.,.false.)
    
    
    
    if (lab * net_wage <=agg%inc_thrs(tc) ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
        
    
    if (sav_temp < bc_sav)then
        cons_temp = coh_exo(xc) - bc_sav -  net_wage*(biggam-lab) &
            - fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,ind_below )  &  !,avearn_base,ind_below
        - mx *  demo%numchild_gsj(gc,sc,jc,tc)
        
        sav_temp = bc_sav
        
        
        cons_temp  = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,cons_endo_2dim)
        sav_temp =func_sav(coh_exo(xc),net_wage,wage_totax,lab,cons_temp,mx,demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below) !,agg%inc_thrs(tc) ) 
        
    endif
    
    ! after inv coh
    mcoh = coh_exo(xc) - mx * demo%numchild_gsj(gc,sc,jc,tc)  
    
    if ( mcoh < minval(coh_endo_2dim) )then
        ! violation of borrowing constraint => infeasible solution
        f_vfun_tinv = -900000.0_dp
    else      
        ! current period utility       
        util_temp = f_util_parent_single(cons_temp,lab_eff,tx,demo%numchild_gsj(gc,sc,jc,tc), gc,ec,dc,jc-jf+1,sc)
             
        if (sav_temp < bc_sav )then
            
            ! make sure that borrowing constraint is not violated
            f_vfun_tinv = -900000.0_dp
            
        else
            
            ! evaluate continuation value by interpolation
            ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)  
            
            if (opt_pchip==1)then
            
                do hcloc = 1,np
                
                    if (coh_endo_2dim(np1_aux-2,hcloc) == coh_endo_2dim(np1_aux-1,hcloc) ) then
            
                        call dpchim (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                            ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                            1, ierr)
                                    
                        xe(1) = mcoh
                        skip = .true.
                                    
                        call dpchfe (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                            ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                            1, skip, 1, xe, fe, ierr)
                                        
                        ev_tp1_temp_h(hcloc) = fe(1)
                    
                    else
                    
                        call dpchim (nx, coh_endo_2dim(:,hcloc), & 
                            ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                            1, ierr)
                                    
                        xe(1) = mcoh
                        skip = .true.
                                    
                        call dpchfe (nx, coh_endo_2dim(:,hcloc), & 
                            ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                            1, skip, 1, xe, fe, ierr)
                                        
                        ev_tp1_temp_h(hcloc) = fe(1)
                    
                    endif

                
                
                enddo
            
                call dpchim (np, grid%hk_grid, & 
                    ev_tp1_temp_h, vder_temp, & 
                    1, ierr)
                                    
                xe(1) = hp1
                skip = .true.
                                    
                call dpchfe (np, grid%hk_grid, & 
                    ev_tp1_temp_h, vder_temp, & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp = fe(1)
            
            endif
            
            
            !print*, "sav1", sav_temp
            !ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)
            !print*, "sav2", f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,grid_sav2dim,vals2x,inds2x,valsh,indsh),bc_sav  
            !pause
            
           !  ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_trans,vals2x,inds2x,valsh,indsh) 
           !  ev_tp1_temp = log( ev_tp1_temp * trans_2dim) 
                          
            ! value function
            f_vfun_tinv = f_vfun(util_temp,ev_tp1_temp,betta_til,demo%sr(jc,tc) )
            
            !print*, mx,hp1,ev_tp1_temp,cons_temp,t1,mcoh,util_temp
            !pause
            
            ! make sure that HK does not fall below min_h (i.e. punish extrapolation)
            if (hp1 < min_h .or. flag_conv==0) f_vfun_tinv = -900000.0_dp
        endif
         
        if (isnan(f_vfun_tinv))then
            print*, "puff", f_hyb2d_inter_mod(sav_temp,hp1,grid_sav2dim,grid%hk_grid,ev_tp1_vec_2dim_trans,vals2x,inds2x,valsh,indsh) 
            pause
        endif
        
        
    endif
    
    end function f_vfun_tinv
    ! -------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------
    function f_vfun_tinv_eval(tx,mx,cons,lab,lab_eff,vfun_tinv_eval_nokid)
    ! compute value function for given time input
    use pchip_module
    
    implicit none

    real(dp):: f_vfun_tinv_eval,vfun_tinv_eval_nokid
    real(dp),intent(in):: tx
    real(dp),intent(in):: mx,cons,lab,lab_eff
    real(dp):: mcoh,cons_temp,sav_temp,util_temp, ev_tp1_temp,prob_edu_kid(ns),ev_kid,v_kid,v_kid_dc(ndl),logsum_kid,prob_kid(ndl),coh_kid,net_wage_kid,gr_wage_kid,bnet,mloc,muc,mul,t1,t2,hp1,val_tinv(ntinv),val_t_str,ev_tp1_temp_nokid 
    integer:: sc_kid_min,sc_kid_max,dc_kid,sc_kid,kc_kid,yc_kid,tinv_loc,hcloc,ierr
    real(dp):: logsum_t1,prob_t1(ntinv),bc_sav,fe(1),xe(1),ev_tp1_temp_h(np),vder_temp_h(nx,np),vder_temp(np)
    logical:: skip
    
    ! after inv coh
    mcoh = coh_exo(xc) - mx * demo%numchild_gsj(gc,sc,jc,tc)
    
    ! next period HK, given inputs
    hp1 = f_hh_tp1(grid%hk_grid(pc),1.0_dp,mx,tx,jc-jf+1,inv_ces) 
    
    !cons_temp  = f_hyb2d_inter(mcoh,hp1,coh_endo_2dim,grid%hk_grid,evp_coh_tp1_vec_2dim(:,:)**(-1.0_dp))
    !cons_temp = cons_temp**(-1.0_dp)
    !cons_temp =( cons_temp *  (1.0_dp + agg%ret(tc) ) * betta*demo%sr(jc,tc)  )**(-1.0_dp)
    
    if (lab * net_wage <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif
    
    cons_temp = cons
    
    sav_temp =func_sav(coh_exo(xc),net_wage,wage_totax,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),cons_temp,mx,demo%numchild_gsj(gc,sc,jc,tc),gc,sc,ec,dc,ic,jc,tc,ind_below ) 
    
    !bc_sav = func_intp(grid%hk_grid,grid_sav2dim(1,:),hp1,vals,inds,.false.,.false.)
    !
    !if (sav_temp < bc_sav)then
    !    cons_temp = coh_exo(xc) - bc_sav -  net_wage*(biggam-pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc)) &
    !        - fun_hsv_new(wage_totax*pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,gckid,jc,tc),0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn )  & 
    !    - mx *  demo%numchild_gsj(gc,sc,jc,tc)
    !    
    !    sav_temp = bc_sav
    !    !print*, "how",coh_exo(xc),coh_min,func_intp(grid%hk_grid,coh_endo_2dim(np1_aux,:),hp1,vals,inds,.false.,.false.)
    !    !pause
    !endif
    
    ! current period utility       
    util_temp = f_util_parent_single(cons_temp,lab_eff,tx,demo%numchild_gsj(gc,sc,jc,tc), gc,ec,dc,jc-jf+1,sc)
    
        
    ! evaluate continuation value by interpolation
    ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)  
    ev_tp1_temp_nokid =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_nokid,vals2x,inds2x,valsh,indsh)
    if (opt_pchip==1)then
    
        do hcloc = 1,np
                
            if (coh_endo_2dim(np1_aux-2,hcloc) == coh_endo_2dim(np1_aux-1,hcloc) ) then
            
                call dpchim (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), &  
                    1, ierr)
                                    
                xe(1) = mcoh 
                skip = .true.
                                    
                call dpchfe (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            else
                    
                call dpchim (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, ierr)
                                    
                xe(1) = mcoh
                skip = .true.
                                    
                call dpchfe (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            endif

                
                
        enddo
            
        call dpchim (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, ierr)
                                    
        xe(1) = hp1
        skip = .true.
                                    
        call dpchfe (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, skip, 1, xe, fe, ierr)
                                        
        ev_tp1_temp = fe(1)
    
        ev_tp1_temp_nokid =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_nokid,vals2x,inds2x,valsh,indsh)  
    
        do hcloc = 1,np
                
            if (coh_endo_2dim(np1_aux-2,hcloc) == coh_endo_2dim(np1_aux-1,hcloc) ) then
            
                call dpchim (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim_nokid(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                    1, ierr)
                                    
                xe(1) = mcoh
                skip = .true.
                                    
                call dpchfe (nx-np1_aux+1, coh_endo_2dim(np1_aux:nx,hcloc), & 
                    ev_tp1_vec_2dim_nokid(np1_aux:nx,hcloc), vder_temp_h(np1_aux:nx,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            else
                    
                call dpchim (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim_nokid(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, ierr)
                                    
                xe(1) = mcoh
                skip = .true.
                                    
                call dpchfe (nx, coh_endo_2dim(:,hcloc), & 
                    ev_tp1_vec_2dim_nokid(:,hcloc), vder_temp_h(:,hcloc), & 
                    1, skip, 1, xe, fe, ierr)
                                        
                ev_tp1_temp_h(hcloc) = fe(1)
                    
            endif

                
                
        enddo
            
        call dpchim (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, ierr)
                                    
        xe(1) = hp1
        skip = .true.
                                    
        call dpchfe (np, grid%hk_grid, & 
            ev_tp1_temp_h, vder_temp, & 
            1, skip, 1, xe, fe, ierr)
                                        
        ev_tp1_temp_nokid = fe(1)
        
    endif
    
            
    !print*, "sav1", sav_temp
    !ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim,vals2x,inds2x,valsh,indsh)
    !print*, "sav2", f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,grid_sav2dim,vals2x,inds2x,valsh,indsh),bc_sav  
    !pause
            
    !  ev_tp1_temp =f_hyb2d_inter_mod(mcoh,hp1,coh_endo_2dim,grid%hk_grid,ev_tp1_vec_2dim_trans,vals2x,inds2x,valsh,indsh) 
    !  ev_tp1_temp = log( ev_tp1_temp * trans_2dim) 
                          
    ! value function
    f_vfun_tinv_eval = f_vfun(util_temp,ev_tp1_temp,betta_til,demo%sr(jc,tc) )
    
    vfun_tinv_eval_nokid = f_vfun(util_temp,ev_tp1_temp_nokid,betta_til,demo%sr(jc,tc) )
    
    end function f_vfun_tinv_eval
    ! -------------------------------------------------------------------
    
    
    ! ------------------------------------------------------------------- 
    function f_grid_sav(xc0)
    ! computes saving grid
    use nr
    use nrutil
    implicit none
    real(dp)::f_grid_sav(nx)
    integer,intent(out)::xc0
    real(dp)::max_pstk0,max_netw_tp1,max_inc_tp1
    real(dp)::min_sav0,max_sav0,min_ass0,max_ass0
    real(dp),parameter::epsi=1.0e-08_dp
    real(dp)::distx_1,distx_2,pd
    real(dp),parameter::tolbr=2.0_dp*min_x     ! set tolbr to 2.0*min_x so that solver converges
    real(dp),parameter::errabs=tolbr*100.0_dp
    real(dp):: minass1,minass2,maxass1,maxass2,minass3,maxass3,minass4,maxass4
    integer,allocatable:: ic_vec(:),dc_vecs(:),ickid_vec(:),gckid_vec(:),ass_temp_min(:,:,:,:,:),asshc_temp_min(:,:,:,:,:,:),ass_temp_max(:,:,:,:,:),asshc_temp_max(:,:,:,:,:,:)
    integer:: dcl, dc_nxt,ecp1_grd,ecp1,xc0cpl,ickid_loc,hckid_locp,gckid_loc,gckid_loc_temp,ickid_loc_temp,ic_loc,ic_loc_temp,yc_loc,dc_loc_temp,dc_loc,hckid_loc
    real(dp):: hkid_intp,gckid_loc_tem
   
    allocate(ic_vec(1))
    ic_vec = (/ic/)         
    
    if (jc+1<jr(tcp1))then
        
        if (ec==1)then
            if ( dc<nd)then
                ecp1 = 1
            else
                ecp1 = 2
            endif
        else
            ecp1=2
        endif
       
        if (ecp1==1  ) then ! not retired next period as a state
            if ( opt_test_noer==1)then ! no enodogenous retirement
           
                allocate(dc_vecs(ndl)) 
                do dcl=1,ndl
                    dc_vecs(dcl)=dcl
                enddo
          
            elseif ( opt_test_noer==0)then 
            
                if ( (jc+1)>=jer .and. (jc+1)<jr(tcp1))then
            
                    allocate(dc_vecs(ndl+1))
                    do dcl=1,ndl
                        dc_vecs(dcl)=dcl
                    enddo
                    dc_vecs(ndl+1)=nd            
                
                else
                
                    allocate(dc_vecs(ndl)) 
                    do dcl=1,ndl
                        dc_vecs(dcl)=dcl
                    enddo
                endif
                            
          
            endif
            
            
            allocate(ickid_vec(1))
            allocate(gckid_vec(1))
                
            ickid_vec(1) = 1  ! this is always one
            gckid_vec(1) = 1
            
             
            !maxass1 =min( minval(grid%assmin(dc_vecs,nx,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) , minval(grid%assmax(dc_vecs,nx,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) ) 
            if (sc>2 .and. jc==js)then
                maxass1 =minval(grid%ass(dc_vecs,nx,pc_tp1,:,ecp1,:,gc,:,1,sc,ickid_vec,gckid_vec,jc+1,tcp1))
            elseif (jc==jf-2 .and. probmar_s(2,gc,sc) > 0.0_dp .and. gen_id==1 .and. opt_cpl_comp==-1)then
                if (gc==1)then
                    maxass1 =min( minval(grid%ass(dc_vecs,nx,pc_tp1,:,ecp1,:,:,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)),  & 
                        minval(grid%ass_cpl(dc_vecs,nx,pc,:,:,ecp1,:,:,kc,:,sc,:,jc+1,tcp1)) - sav_max(2,tc)*0.0_dp )
                else
                    maxass1 =min( minval(grid%ass(dc_vecs,nx,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)), & 
                        minval(grid%ass_cpl(dc_vecs,nx,pc_tp1,:,:,ecp1,:,:,:,kc,:,sc,jc+1,tcp1)) - sav_max(1,tc)*0.0_dp )
                endif
                    
            elseif (jc==jf-1 .and. gc==2)then
                maxass1 =minval(grid%ass(dc_vecs,nx,pc_tp1_min:pc_tp1_max,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1))  
            else
                maxass1 =minval(grid%ass(dc_vecs,nx,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1))
            endif
            
                               
            max_ass0=maxass1 
          
            !minass1 =max ( maxval(grid%assmin(dc_vecs,1,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) , maxval(grid%assmax(dc_vecs,1,pc_tp1,:,ecp1,ic_vec,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) )
            if (sc>2 .and. jc==js)then
                minass1 = maxval(grid%ass(dc_vecs,1,pc_tp1,:,ecp1,:,gc,:,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)) + (fee_flow - col_subs_flow)
            
            elseif (jc==jf-2 .and. probmar_s(2,gc,sc) > 0.0_dp .and. gen_id==1 .and. opt_cpl_comp==-1)then
                if (gc==1)then
                    !if (sav_min(2,tc)<0.0_dp)then
                    !    minass1 =max( maxval(grid%ass(dc_vecs,1,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)),  & 
                    !        maxval(grid%ass_cpl(dc_vecs,1,pc_tp1,:,:,ecp1,:,:,kc,:,sc,:,jc+1,tcp1)) -sav_min(2,tc)*1.0_dp )
                    !else
                        minass1 =max( maxval(grid%ass(dc_vecs,1,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)),  & 
                            maxval(grid%ass_cpl(dc_vecs,1,pc_tp1,:,:,ecp1,:,:,kc,:,sc,:,jc+1,tcp1))  )
                    !endif
                    
                else
                    !if (sav_min(1,tc)<0.0_dp)then
                    !    minass1 =max( maxval(grid%ass(dc_vecs,1,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)), & 
                    !        maxval(grid%ass_cpl(dc_vecs,1,pc_tp1,:,:,ecp1,:,:,:,kc,:,sc,jc+1,tcp1)) -sav_min(1,tc)*1.0_dp )
                    !else
                        minass1 =max( maxval(grid%ass(dc_vecs,1,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1)), & 
                            maxval(grid%ass_cpl(dc_vecs,1,pc_tp1,:,:,ecp1,:,:,:,kc,:,sc,jc+1,tcp1))  )
                    !endif
                    
                        
                endif
            elseif (jc==jf-1 .and. gc==2)then
                minass1 = maxval(grid%ass(dc_vecs,1,pc_tp1_min:pc_tp1_max,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1))  
            else
                minass1 = maxval(grid%ass(dc_vecs,1,pc_tp1,:,ecp1,:,gc,kc,1,sc,ickid_vec,gckid_vec,jc+1,tcp1))
            endif
            
           
            min_ass0=minass1        
            
            
        else ! ecp1=2
            
            allocate(ickid_vec(1))
            allocate(gckid_vec(1))
                
            ickid_vec(1) = ickid  ! this stays constant after jf
            gckid_vec(1) = gckid              
        
            if (opt_test_noer)then
                print*, "should be possible only with end ret" ! tbc put assert here
                pause
                
            else
                
                if (jc>=jer )then ! early
                    max_ass0=minval(grid%ass(nd-1,nx,pc,yc,ecp1,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) 
                    min_ass0=maxval(grid%ass(nd-1,1,pc,yc,ecp1,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) 
                elseif (jc>=jrr)then ! regular
                    max_ass0=minval(grid%ass(nd,nx,pc,yc,ecp1,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) 
                    min_ass0=maxval(grid%ass(nd,1,pc,yc,ecp1,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) 
                else
                    print*, "what are we missing?"
                    pause
                endif
                
            endif
                    
     
        endif
       
    
    else ! jc+1>=jr next period
        
        allocate(ickid_vec(1))
        allocate(gckid_vec(1))
                
        ickid_vec(1) = 1  ! this stays constant after jf
        gckid_vec(1) = 1           
        
        if (opt_test_noer)then 
            max_ass0=minval(grid%ass(nd,nx,1,yc,ne,1,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1))
            min_ass0=maxval(grid%ass(nd,1, 1,yc,ne,1,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1))
        else
            
            if (jc+1==jr(tc))then        
                if (ec==1)then ! go in ret at mand age jr
                    max_ass0=minval(grid%ass(nd-2,nx,pc,yc,ne,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1))
                    min_ass0=maxval(grid%ass(nd-2,1,pc,yc,ne,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1) )
                else
                    max_ass0=minval(grid%ass(dc,nx,pc,yc,ne,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) 
                    min_ass0=maxval(grid%ass(dc,1,pc,yc,ne,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1)) 
                endif
            else
                max_ass0=minval(grid%ass(dc,nx,pc,yc,ne,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1))
                min_ass0=maxval(grid%ass(dc,1,pc,yc,ne,wc,gc,kc,hc,sc,ickid_vec,gckid_vec,jc+1,tcp1) )
            endif
        endif
        
                    
    endif
   
    deallocate(ic_vec,ickid_vec,gckid_vec)
    
    ! bound on max_x0, min_x0: avoid making grids too large
    !if (gen_id ==1)then
    !    max_ass0=min(max_ass0,max_x)
    !    min_ass0=max(min_ass0,min_x)
    !else
        max_ass0=min(max_ass0,max_x)
        min_ass0=max(min_ass0,grid%amin_age(gc,sc,gen_id,jc))
    !endif
    
    ! compute bounds of saving grid:
    max_sav0=max_ass0 * (1.0_dp + lamb)
    min_sav0=min_ass0 * (1.0_dp + lamb)
   
    if (min_sav0>max_sav0) then
        print*, 'there is a problem with flexible grid: increase parameter max_x',jc, min_ass0,max_ass0,maxass1
        print*, 'counters: ', ec,dc,ic,gc,jc+1,kc,sc,jr(sc),dc_vecs,maxass1,ickid,gckid,pc_tp1
        !print*, grid%ass(ec1(jc+1,tcp1):ec2(jc+1,tcp1),nx,pc,:,ec1(jc+1,tcp1):ec2(jc+1,tcp1),ic,jc+1,kc,sc,tcp1)
        pause
    endif
     
    if (min_sav0<epsi .and. max_sav0<epsi)then
        print*, "there is another problem"
        pause
    endif
   
    if (jc+1<jr(tc) .and. ecp1==1)then
        deallocate(dc_vecs)
    endif
   
    xc0=np1_aux   ! default: throw in point in region of binding borrowing constraint 
   
    !if (gen_id==1)then
    !
    !    ! build saving grid: 
    !    if (xc0==1) then
    !        if (max_x_fac>epsi) then
    !            f_grid_sav(1:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-1,3.0_dp)
    !            f_grid_sav(nx)=max_sav0
    !        else
    !            f_grid_sav(1:nx)=makegrid(min_sav0,max_sav0,nx,3.0_dp)
    !        endif
    !    else    
    !        f_grid_sav(1:np1_aux-1)=0.0_dp
    !        if (max_x_fac>epsi) then
    !            f_grid_sav(np1_aux:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-np1_aux,3.0_dp)
    !            f_grid_sav(nx)=max_sav0
    !        else
    !            f_grid_sav(np1_aux:nx)=makegrid(min_sav0,max_sav0,nx-np1_aux+1,3.0_dp)
    !        endif
    !    endif
    !    
    !else
    !    
        ! build saving grid: 
        if (xc0==1) then
            if (max_x_fac>epsi) then
                f_grid_sav(1:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-1,3.0_dp)
                f_grid_sav(nx)=max_sav0
            else
                f_grid_sav(1:nx)=makegrid(min_sav0,max_sav0,nx,3.0_dp)
            endif
        else    
            f_grid_sav(1:np1_aux-1)=grid%amin_age(gc,sc,gen_id,jc)
            if (max_x_fac>epsi) then
                f_grid_sav(np1_aux:nx-1)=makegrid(min_sav0,max_sav0/max_x_fac,nx-np1_aux,3.0_dp)
                f_grid_sav(nx)=max_sav0
            else
                f_grid_sav(np1_aux:nx)=makegrid(min_sav0,max_sav0,nx-np1_aux+1,3.0_dp)
            endif
        endif
        
   ! endif
   
    end function f_grid_sav
    ! ------------------------------------------------------------------- 

    
    ! -------------------------------------------------------------------
    subroutine sub_prepintp_ret(sav,v_tp1,vp_coh_tp1,ind_bc)
    ! compute continuation value and rhs_foc in retirement
    
    implicit none
    real(dp),intent(in):: sav
    logical,intent(in):: ind_bc
    real(dp),intent(out):: v_tp1,vp_coh_tp1
                                        
    ! pension payments (linked to respective aggregate wage)
    av_prod_tp1 =demo%ageprod(gc,jr(tcp1)-1,sc )  
    call sub_pens_new(gr_pens_tp1,agg%rho_p(tcp1),agg%tau_p(tcp1),0.0_dp,agg%avwage(tcp1),agg%pia0(gc,1,sc,tcp1), & 
        av_prod_tp1 ,demo%grid_y(ycp1),0.0_dp,jc+1,gc,dcp1,ecp1,tcp1,sc,kc,ycp1,nd)
    pens_tp1 = fun_netpens(gr_pens_tp1,0.0_dp,0.0_dp,2,gc,ecp1,dcp1,1,jc+1,tcp1,avearn_tp1) 
                                            
    ! cash on hand tomorrow: based on maximum income
    coh_tp1=f_coh_tp1(sav,b_til_tp1(sc,jc+1),R_til_tp1,net_wage_tp1,pens_tp1,demo%lamt(tcp1),agg%tau_k(tcp1),ecp1,dcp1)
    
    call sub_chkbd(grid%coh(dcp1,:,pc_tp1:pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1),coh_tp1,lb_viol,ub_viol,dc,ec,dcp1,ecp1,ind_bc)   ! correction in case of bound violation                
    
    !if (ub_viol )then
    !    print*, "ret ub viol", jc,hc,sc
    !    pause
    !endif
    
    
    ! interpolation: value function and rhs_foc
    v_tp1=func_intp(grid%coh(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1),pol%v(dcp1,:,1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
    
    if (isnan(v_tp1))then
        print*, "ret",jc, grid%coh(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1),pol%v(dcp1,:,1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1)
        pause
    endif
                                     
    vp_coh_tp1=func_intp(grid%coh(dcp1,:,1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1),pol%inv_vp_x(dcp1,:,1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid,gckid,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
        
    
     
    vp_coh_tp1=f_reinv(vp_coh_tp1,0.0_dp)
                           
    end subroutine sub_prepintp_ret
    ! -------------------------------------------------------------------
    
   
    ! -------------------------------------------------------------------
    subroutine sub_prepintp_wrk_cpl(sav,v_tp1,v_tp1_nokid,vp_coh_tp1,ind_bc)
    ! compute continuation value and rhs_foc for workers
    
    implicit none
    real(dp),intent(in):: sav
    logical,intent(in):: ind_bc
    real(dp),intent(out):: v_tp1,vp_coh_tp1,v_tp1_nokid
    real(dp):: vals_h(2),valsx_h(np,2),hp1_hs,sav_tot
    integer:: inds_h(2),indsx_h(np,2)
    real(dp):: gr_wage_tp1,net_wage_tp1,wage_totax_tp1,gr_wage_tp1_p,net_wage_tp1_p,wage_totax_tp1_p,wage_cpl(2)
    integer:: yc1p1,yc2p1,wc1p1,wc2p1,kc1,kc2,sc1,sc2
                                        
    ! wages, pension benefits tomorrow [would be relevant for endogenous retirement]
   
    call sub_wage(gr_wage_tp1_p,net_wage_tp1_p,wage_totax_tp1_p,agg%wage_s(sc_p,tcp1),demo%ageprod(gc_p,jc+1,sc_p),demo%grid_y(ycp1_p),grid%hk_grid(1),agg%tau_p(tcp1),sc_p,kc_p,dcp1,ecp1,jc+1,gen_id,agg%gammah_s(sc_p,tc),agg%abgrad_s(:,sc_p),agg%hk_cutoff(:,tc)) 
    gr_wage_tp1_p = gr_wage_tp1_p * demo%grid_epsi(wcp1_p)
    net_wage_tp1_p = net_wage_tp1_p * demo%grid_epsi(wcp1_p)
    wage_totax_tp1_p = wage_totax_tp1_p * demo%grid_epsi(wcp1_p)
    
    call sub_wage(gr_wage_tp1,net_wage_tp1,wage_totax_tp1,agg%wage_s(sc,tcp1),demo%ageprod(gc,jc+1,sc),demo%grid_y(ycp1),grid%hk_grid(1),agg%tau_p(tcp1),sc,kc,dcp1,ecp1,jc+1,gen_id,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc)) 
    gr_wage_tp1 = gr_wage_tp1 * demo%grid_epsi(wcp1)
    net_wage_tp1 = net_wage_tp1 * demo%grid_epsi(wcp1)
    wage_totax_tp1 = wage_totax_tp1 * demo%grid_epsi(wcp1)
             
    ! cash on hand tomorrow: based on maximum income
    if (jc==jf-2)then
        sav_tot = sav + sav_p
        sav_tot = sav_tot !* 0.5_dp
    else
        sav_tot = sav
    endif
    
    if (gc==1)then
        wage_cpl(1) = net_wage_tp1
        wage_cpl(2) =net_wage_tp1_p
        yc1p1 = ycp1
        wc1p1 = wcp1
        yc2p1 = ycp1_p
        wc2p1 = wcp1_p
        sc1 = sc
        sc2 = sc_p
        kc1 = kc
        kc2 = kc_p
    else
        wage_cpl(1) = net_wage_tp1_p
        wage_cpl(2) =net_wage_tp1
        yc2p1 = ycp1
        wc2p1 = wcp1
        yc1p1 = ycp1_p
        wc1p1 = wcp1_p
        sc2 = sc
        sc1 = sc_p
        kc2 = kc
        kc1 = kc_p
    endif
    
        
    coh_tp1=f_coh_tp1_cpl(sav_tot,b_til_tp1(sc,jc+1) + b_til_tp1(sc_p,jc+1),R_til_tp1,wage_cpl,wage_cpl,demo%lamt(tcp1),agg%tau_k(tcp1),ecp1,dcp1)
    
    !print*, "cohtp1",coh_tp1
    ! interpolation: value function
     !call sub_chkbd(grid%coh_cpl(dcp1,:,pc_tp1:pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,lb_viol,ub_viol,dc,ec,dcp1,ecp1,ind_bc)   ! correction in case of bound violation
        
    !if (ub_viol )then
    !    print*, "w ub viol", jc,dcp1,pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid_tp1(gen_id,jc+1,ickid,ickid_init),gckid_tp1(gen_id,jc+1,gckid,gckid_init),jc+1,tcp1,gen_id
    !    !pause
    !endif
    
    v_tp1=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%v_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals ,inds ,.false.,.false.)
        
    v_tp1_nokid=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1), & 
        pol%v_nokid_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals ,inds ,.false.,.false.)
        
        
    if (isnan(v_tp1))then
        print*, "aha", v_tp1,coh_tp1,pol%v_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1)
        pause
    endif
                                                        
    vp_coh_tp1=func_intp(grid%coh_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),pol%inv_vp_x_cpl(dcp1,:,pc_tp1,yc1p1,yc2p1,ecp1,wc1p1,wc2p1,kc1,kc2,sc1,sc2,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
   
   
        
        
    vp_coh_tp1=f_reinv(vp_coh_tp1,0.0_dp)
    
    !if (jc==jt-1)then
    !    print*, vp_coh_tp1, v_tp1
    !    pause
    !endif
    
    
    end subroutine sub_prepintp_wrk_cpl
    ! -------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------
    subroutine sub_prepintp_wrk(sav,v_tp1,v_tp1_nokid,vp_coh_tp1,ind_bc)
    ! compute continuation value and rhs_foc for workers
    
    implicit none
    real(dp),intent(in):: sav
    logical,intent(in):: ind_bc
    real(dp),intent(out):: v_tp1,vp_coh_tp1,v_tp1_nokid
    real(dp):: vals_h(2),valsx_h(np,2),hp1_hs,sav_tot
    integer:: inds_h(2),indsx_h(np,2)
                                        
    ! wages, pension benefits tomorrow [would be relevant for endogenous retirement]
   
    call sub_wage(gr_wage_tp1,net_wage_tp1,wage_totax_tp1,agg%wage_s(sc,tcp1),demo%ageprod(gc,jc+1,sc),demo%grid_y(ycp1),grid%hk_grid(hc),agg%tau_p(tcp1),sc,kcp1,dcp1,ecp1,jc+1,gen_id,agg%gammah_s(sc,tc),agg%abgrad_s(:,sc),agg%hk_cutoff(:,tc)) 
    gr_wage_tp1 = gr_wage_tp1 * demo%grid_epsi(wcp1)
    net_wage_tp1 = net_wage_tp1 * demo%grid_epsi(wcp1)
    wage_totax_tp1 = wage_totax_tp1 * demo%grid_epsi(wcp1)
   
    ! pension payments (linked to respective aggregate wage)
    av_prod_tp1 = demo%ageprod(gc,jr(tcp1)-1,sc )  
    call sub_pens_new(gr_pens_tp1,agg%rho_p(tcp1),agg%tau_p(tcp1),0.0_dp,agg%avwage(tcp1),agg%pia0(gc,1,sc,tcp1), & 
        av_prod_tp1 ,demo%grid_y(ycp1),0.0_dp,jc+1,gc,dcp1,ecp1,tcp1,sc,kcp1,ycp1,nd)
    pens_tp1 = fun_netpens(gr_pens_tp1,0.0_dp,0.0_dp,2,gc,ecp1,dcp1,icp1,jc+1,tcp1,avearn_tp1) 
                     
    ! cash on hand tomorrow: based on maximum income
    coh_tp1=f_coh_tp1(sav,b_til_tp1(sc,jc+1),R_til_tp1,net_wage_tp1,pens_tp1,demo%lamt(tcp1),agg%tau_k(tcp1),ecp1,dcp1)
    
    ! interpolation: value function
    ! call sub_chkbd(grid%coh(dcp1,:,pc_tp1:pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid_tp1(gen_id,jc+1,ickid,ickid_init),gckid_tp1(gen_id,jc+1,gckid,gckid_init),jc+1,tcp1),coh_tp1,lb_viol,ub_viol,dc,ec,dcp1,ecp1,ind_bc)   ! correction in case of bound violation
        
    !if (ub_viol )then
    !    print*, "w ub viol", jc,dcp1,pc_tp1,ycp1,ecp1,wcp1,gc,kc,hc,sc,ickid_tp1(gen_id,jc+1,ickid,ickid_init),gckid_tp1(gen_id,jc+1,gckid,gckid_init),jc+1,tcp1,gen_id
    !    !pause
    !endif
    
    v_tp1=func_intp(grid%coh(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kcp1,1,sc,1,1,jc+1,tcp1),pol%v(dcp1,:,pc_tp1,ycp1,ecp1,icp1,gc,kcp1,1,sc,1,1,jc+1,tcp1),coh_tp1,vals ,inds ,.false.,.false.)
        
    v_tp1_nokid=func_intp(grid%coh(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kcp1,1,sc,1,1,jc+1,tcp1), & 
        pol%v_nokid(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kcp1,1,sc,1,1,jc+1,tcp1),coh_tp1,vals ,inds ,.false.,.false.)
        
        
    if (isnan(v_tp1))then
        print*, "aha", v_tp1,coh_tp1,pol%v(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kcp1,hc,sc,1,1,jc+1,tcp1),dcp1,pc_tp1,ycp1,ecp1,icp1,gc,kcp1,hc,sc,jc+1,tcp1
        pause
    endif
                                                        
    vp_coh_tp1=func_intp(grid%coh(dcp1,:,pc_tp1,ycp1,ecp1,wcp1,gc,kcp1,1,sc,1,1,jc+1,tcp1),pol%inv_vp_x(dcp1,:,pc_tp1,ycp1,ecp1,icp1,gc,kcp1,1,sc,1,1,jc+1,tcp1),coh_tp1,vals,inds,.false.,.false.)
   
   
        
        
    vp_coh_tp1=f_reinv(vp_coh_tp1,0.0_dp)
    
    !if (jc==jt-1)then
    !    print*, vp_coh_tp1, v_tp1
    !    pause
    !endif
    
    
    end subroutine sub_prepintp_wrk
    ! -------------------------------------------------------------------
   
    ! ------------------------------------------------------------------- 
    subroutine sub_solvehh_sav(sav,ind_bc,flag_invalid,ext_flg,opt_input)
    ! computes solution of household problem on current savi2 grid point
    implicit none
    real(dp),intent(in)::sav
    integer,intent(in):: flag_invalid,ext_flg,opt_input
    logical,intent(in)::ind_bc
    real(dp)::check
    real(dp),dimension(ni)::evp_coh_tp1_ni,evp_grpstk_tp1_ni,ev_tp1_ni,ev_tp1_ni_nokid
    real(dp):: vp_coh_tp1_test,pimar
    integer:: dc_max,dc_locc
    real(dp),allocatable:: prob(:),vp_coh_tp1_temp(:),v_tp1_temp(:),v_tp1_temp_nokid(:)
    real(dp):: logsum,logsum_nokid,logsum_emp,prob_empret(2),v_tp1_empret(2),probfe(nk),wght_phi,prob_yp, sort_param_tmp, tmp_distr
    integer::ecp1_min,ecp1_max,dcp1_temp,dc_loc,dc_temp_loc
    integer,allocatable:: dc1_vec(:)
    integer:: dcl,ecp1_temp,n_ep1
    integer,allocatable:: vec_ecp1(:)
    integer:: icp1_temp,qcp1_max,ickid_init_min,ickid_init_max,gckid_init_min,gckid_init_max,ycp1_max,wcp1_max,kcp1_min,kcp1_max,dc2,dc2_max
    real(dp),allocatable:: ic1_vec(:)
    integer:: sc2,xc2,sc_p_min,sc_p_max,kc_p_min,kc_p_max,xc2_min,xc2_max,ycp1_p_max,wcp1_p_max,mcp1,mcp1_min,mcp1_max
    
    
    ! compute wages and pension benefits TODAY 
    call sub_wagepens()
                    
    ! initialization of expected value function and its derivatives
    evp_coh_tp1_ni(:)=0.0_dp
    evp_grpstk_tp1_ni(:)=0.0_dp
    ev_tp1_ni(:)=0.0_dp
    ev_tp1_ni_nokid = 0.0_dp
   
    ycp1_max = ny
    wcp1_max = nw
    
    if (sc>2 .and. jc==js)then
        kcp1_min = 1
        kcp1_max = nk
         if (nk>1)then
            probfe(nk) = f_probfe(grid%hk_grid(hc),sc,agg%gammah_s(sc,tc))
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
    
    
    if (jc+1<jr(tcp1)) then ! working period (before mandatory retirement age)
       
        allocate(ic1_vec(1)) ! child innate ability next period
        ic1_vec = (/1/)                 
        
        do icp1_temp = 1,1 !size(ic1_vec) ! innate ability parent
             
            icp1 = 1 !ic1_vec(icp1_temp)
             
            if (ec==1)then
                if ( dc<nd)then 
                    ecp1 = 1
                else
                    ecp1 = 2
                endif
            else
                ecp1=2
            endif
            
            
            if (ecp1<ne)then ! not retired tomorrow (as a state)
                
                if (jc==jf-2 .and. gen_id ==1 .and. probmar_s(2,gc,sc) > 0.0_dp .and. opt_cpl_comp==1 )then
                    mcp1_min = 1
                    mcp1_max = 2
                    
                else
                    mcp1_min = 1
                    mcp1_max = 1
                    pimar = 1.0_dp
                endif
                
                do mcp1 = mcp1_min,mcp1_max
                
                if (jc==jf-2 .and. mcp1==2)then
                    sc_p_min = 1
                    sc_p_max = ns
                    
                    kc_p_min = 1
                    kc_p_max = nk
                    
                    xc2_min = 1
                    xc2_max = nx
                    
                    ycp1_p_max = ny
                    wcp1_p_max = nw 
                    
                    dc2_max = nd
                    
                    pimar = probmar_s(mcp1,gc,sc)
                else
                    if (jc==jf-2)then
                        pimar = probmar_s(mcp1,gc,sc)
                    endif
                    dc2_max = 1
                    
                    sc_p_min = 1
                    sc_p_max = 1
                    
                    kc_p_min = 1
                    kc_p_max = 1
                    
                    xc2_min = 1
                    xc2_max = 1
                    
                    ycp1_p_max = 1
                    wcp1_p_max = 1
                endif
                
                do sc_p = sc_p_min,sc_p_max
                    if (jc==jf-2 .and. mcp1==2)then
                        pimar =0.5_dp * ( probmar_s(mcp1,gc,sc) + probmar_s(mcp1,gc_p,sc_p) )
                    else
                        if (jc==jf-2)then
                            pimar = probmar_s(mcp1,gc,sc)
                            
                        endif
                    endif
                    
                        
                      
                    do kc_p = kc_p_min,kc_p_max
                        
                        do xc2 = xc2_min,xc2_max
                            do dc2=1,dc2_max
                            !gcp1 = 1
                           
                            !if (jc==jf-2 .and. mcp1==2)then
                            !    if (probmar(mcp1)==0.0_dp) cycle
                            !    !if (grid%Phi2(xc2,kc_p,sc_p,gc_p,jc)==0.0_dp) cycle
                            !endif
                            
                            
                
                
                do kcp1 = kcp1_min,kcp1_max
                
                do ycp1=1,ycp1_max !ny     ! income state tomorrow
                    do yc_p = 1,ycp1_p_max
                    do ycp1_p = 1,ycp1_p_max
                    do wcp1 = 1,wcp1_max
                        do wc_p = 1,wcp1_p_max
                        do wcp1_p = 1,wcp1_p_max
                            
                            if (mcp1==2)then
                                !if (sav_p+ sav < 40.0_dp)then
                                    sav_p = grid%sav2guess(dc2,xc2,yc_p,wc_p,gc_p,kc_p,sc_p,tc) !* 0.5_dp !grid%sav2(xc2,kc_p,sc_p,gc_p,jc)
                                !else
                                !    sav_p = 0.0_dp
                                !endif
                                if (sc==sc_p)then
                                   perfsort_flg = 1.0_dp
                                else
                                   perfsort_flg = 0.0_dp
                                endif
                                tmp_distr = (grid%Phi_s(sc_p,gc_p,tc)- sort_param * grid%Phi_s(sc_p,gc,tc)  ) /  (sum(grid%Phi_s(:,gc_p,tc))- sort_param * sum(grid%Phi_s(:,gc,tc))  )
                                sort_param_tmp = sort_param
                                if (tmp_distr<=0.0_dp) sort_param_tmp = 1.0_dp
                                wght_phi =grid%Phi2guess(dc2,xc2,yc_p,kc_p,gc_p,wc_p,sc_p,tc)/grid%Phi_s(sc_p,gc_p,tc) * ( (1.0_dp - sort_param) *( tmp_distr + sort_param * perfsort_flg ) )
                                wght_phi = wght_phi/0.5_dp
				                prob_yp =demo%pini(yc_p) * demo%prob_epsi(wc_p)
                                if (grid%Phi2guess(dc2,xc2,yc_p,kc_p,gc_p,wc_p,sc_p,tc)==0.0_dp) cycle
                            else
                               
                                wght_phi = 1.0_dp 
                                prob_yp =1.0_dp
                            endif
                                          
                    ! number of discrete choices available
                    if (opt_test_noer==1)then ! no endogenous retirement
                            
                        allocate(dc1_vec(ndl))
                        do dcl=1,ndl
                            dc1_vec(dcl)=dcl
                        enddo
                     
                   
                    elseif (opt_test_noer==0)then ! yes endogenous retirement
                                
                        if ( (jc+1)>=jer .and. (jc+1)<jr(tcp1)) then
                            
                            allocate(dc1_vec(ndl+1))
                            do dcl=1,ndl
                                dc1_vec(dcl)=dcl
                            enddo
                            dc1_vec(ndl+1)=nd 
                                 
                        else
                            allocate(dc1_vec(ndl))
                            do dcl=1,ndl
                                dc1_vec(dcl)=dcl
                            enddo
                        endif
                                                                
                                             
                    endif
                        
                    nd_glob=size(dc1_vec)
                        
                    allocate(prob(nd_glob))
                    allocate(v_tp1_temp(nd_glob))
                    allocate(v_tp1_temp_nokid(nd_glob))
                    allocate(vp_coh_tp1_temp(nd_glob))
                            
                    if (nd_glob<1)then
                        print*, "what happened?"
                        pause
                    endif
                    
                    if (gen_id==1 .and. jc+1==jf)then
                            
                        ickid_init_min = 1
                        ickid_init_max = ni    
                        gckid_init_min = 1
                        gckid_init_max =1
                        
                    else
                        
                        ickid_init_min = 1
                        ickid_init_max = 1    
                        gckid_init_min = 1
                        gckid_init_max =1
                        
                    endif
                    
                    do pc_tp1 = pc_tp1_min,pc_tp1_max
                    
                    do gckid_init = 1,gckid_init_max
                    
                        do ickid_init = 1,ickid_init_max                    
                    
                            ! determine probabilities
                            do dcp1_temp=1,size(dc1_vec)
                       
                                dcp1 = dc1_vec(dcp1_temp)  
                        
                                v_tp1_temp(dcp1_temp)=0.0_dp
                                v_tp1_temp_nokid(dcp1_temp)=0.0_dp
                                vp_coh_tp1_temp(dcp1_temp)=0.0_dp
                        
                                if (mcp1==1)then
                                    call sub_prepintp_wrk(sav,v_tp1,v_tp1_nokid,vp_coh_tp1,ind_bc)
                                else
                                    call sub_prepintp_wrk_cpl(sav,v_tp1,v_tp1_nokid,vp_coh_tp1,ind_bc)
                                endif
                                
                                v_tp1_temp(dcp1_temp)=v_tp1
                                v_tp1_temp_nokid(dcp1_temp)=v_tp1_nokid
                                vp_coh_tp1_temp(dcp1_temp)=vp_coh_tp1
                        
                            enddo                          
                            
                            if (size(dc1_vec)==ndl+1)then
                                call sub_logsumprob(v_tp1_temp(1:ndl),logsum_emp,prob(1:ndl),nd_glob-1,sigma_emp)
                                v_tp1_empret(1) = logsum_emp
                                v_tp1_empret(2) = v_tp1_temp(ndl+1)
                                call sub_logsumprob(v_tp1_empret,logsum,prob_empret,2,sigma_ret)
                                do dc_temp_loc = 1,ndl
                                    prob(dc_temp_loc)= prob(dc_temp_loc) * prob_empret(1)
                                enddo
                                prob(ndl + 1) = prob_empret(2)
                                                
                            else
                                if (nd_glob>1)then
                                    call sub_logsumprob(v_tp1_temp,logsum,prob,nd_glob,sigma_emp)
                                else
                                    logsum = v_tp1_temp(1)
                                    prob = 1.0_dp
                                endif
                                
                            endif
                    
                            prob_dc=0.0_dp
        
                            do dc_temp_loc=1,size(dc1_vec)
                                dc_loc=dc1_vec(dc_temp_loc)
            
                                do dc_full = 1,nd
                                    if (dc_loc==dc_full)then
                                        prob_dc(dc_full)=prob(dc_temp_loc)
                                    endif
                                enddo
                            enddo 
                        
                            if ( flag_stud>0 .and. jc+1<=jstud(sc))then
                               
                                ev_tp1_ni(icp1)=ev_tp1_ni(icp1)+demo%prob_epsi(wcp1)*demo%pini(ycp1)*logsum * probfe(kcp1)

								logsum_nokid = 0.0_dp
                                do dc_locc = 1,size(dc1_vec)
                                    dcp1 = dc1_vec(dc_locc)
                                    logsum_nokid = logsum_nokid + v_tp1_temp_nokid(dcp1) * prob(dcp1)
                                enddo
                                                                
                                ev_tp1_ni_nokid(icp1)=ev_tp1_ni_nokid(icp1)+demo%prob_epsi(wcp1)*demo%pini(ycp1)*logsum_nokid* probfe(kcp1)
                                 
                                
                            else
                                    
                                ev_tp1_ni(icp1)=ev_tp1_ni(icp1)+demo%prob_epsi(wcp1)*demo%prob_y(yc,ycp1)*logsum* prob_ni(pc_tp1) * wght_phi* prob_yp * pimar
                                    
                                logsum_nokid = 0.0_dp
                                do dc_locc = 1,size(dc1_vec)
                                    dcp1 = dc1_vec(dc_locc)
                                    logsum_nokid = logsum_nokid + v_tp1_temp_nokid(dcp1) * prob(dcp1)
                                enddo
                                                                
                                ev_tp1_ni_nokid(icp1)=ev_tp1_ni_nokid(icp1)+demo%prob_epsi(wcp1)*demo%prob_y(yc,ycp1)*logsum_nokid* prob_ni(pc_tp1) * wght_phi * prob_yp* pimar
                                    
                            endif
                            
                        
                            do dcp1_temp=1,size(dc1_vec)
                    
                                dcp1=dc1_vec(dcp1_temp)
                            
                                if ( flag_stud>0 .and. jc+1<=jstud(sc))then
                                   
                                    evp_coh_tp1_ni(icp1)=evp_coh_tp1_ni(icp1)+demo%prob_epsi(wcp1)*demo%pini(ycp1)*prob_dc(dcp1)*vp_coh_tp1_temp(dcp1_temp) * probfe(kcp1)
                                   
                                else
                                        
                                
                                    evp_coh_tp1_ni(1)=evp_coh_tp1_ni(1)+demo%prob_epsi(wcp1)*demo%prob_y(yc,ycp1)*prob_dc(dcp1)*vp_coh_tp1_temp(dcp1_temp)* prob_ni(pc_tp1) * wght_phi* prob_yp* pimar
                                endif
                                   
                            enddo
                        
                        enddo
                    enddo
                    
                    enddo
                    
                    
                    deallocate(prob,v_tp1_temp,v_tp1_temp_nokid,vp_coh_tp1_temp,dc1_vec)
                enddo 
                enddo
                    enddo
                    enddo
                    enddo
                    
                                    
                end do  ! end do ycp1 
                
                enddo ! kcp1
                enddo
                
                enddo ! sav2 
             
                enddo ! kc2p1
           
                enddo ! sc2p1
               enddo
            else ! ecp1 = ne => retired (before mandatory age)
                    
                ycp1=yc
                wcp1 = wc
                 
                if (jc>=jer .and. jc<jrr)then ! early ret
                    dcp1=nd-1
                elseif (jc>=jrr)then ! reg
                    dcp1=nd
                else
                    print*, "should not be here3!" ! tbc put assert statement
                    pause
                endif                                                                                                                       
               
                call sub_prepintp_ret(sav,v_tp1,vp_coh_tp1,ind_bc)
               
               
                ev_tp1_ni(icp1)=v_tp1
                ev_tp1_ni_nokid(icp1)=v_tp1
                evp_coh_tp1_ni(icp1)=vp_coh_tp1
               
            endif                                
           
        end do  ! end do icp1
        
        deallocate(ic1_vec)
       
        ev_tp1=ev_tp1_ni(1)
        ev_tp1_nokid=ev_tp1_ni_nokid(1)
        evp_coh_tp1=evp_coh_tp1_ni(1)
       
        if (evp_coh_tp1<0.0_dp)then
            print*, "uuupps", evp_coh_tp1,ec,dc,jc, v_tp1_temp, logsum
            pause
        endif
        
        
    else ! jc+1>=jr => retirement after mand age
              
        allocate(ic1_vec(1))
        ic1_vec = (/ic/)         
        
        
        do icp1_temp=1,size(ic1_vec) ! citizenship tomorrow
            icp1 = ic1_vec(icp1_temp)
           
            ycp1=yc
            wcp1 = wc
            ecp1=ne
            
            if (jc+1==jr(tc))then 
                if (ec==1)then
                    
                    if (opt_test_noer)then
                        dcp1=nd
                    else                        
                        dcp1=nd-2 ! late retirement
                    endif
                    
                else
                    dcp1=dc
                endif
            else
                dcp1=dc
            endif
           
           
            call sub_prepintp_ret(sav,v_tp1,vp_coh_tp1,ind_bc)
            
            
            ev_tp1_ni(icp1)=v_tp1
            ev_tp1_ni_nokid(icp1)=v_tp1
            evp_coh_tp1_ni(icp1)=vp_coh_tp1
           
        end do  ! end do icp1
        
        deallocate(ic1_vec)
            
      
        ev_tp1=ev_tp1_ni(ic)
        ev_tp1_nokid=ev_tp1_ni_nokid(ic)
        evp_coh_tp1=evp_coh_tp1_ni(ic)
       
    endif
    
    if (isnan(ev_tp1))then
        print*, "NaN cont value, how can this be?",tc,jc,ec,dc,ic,v_tp1,ben_p
        pause
    endif
    
    
    if (evp_coh_tp1<0.0_dp)then
        print*, "here uuupps", evp_coh_tp1,ec,dc,jc
        pause
    endif
                     
    ! solution of household problem given expected and interpolated values
    
    if (ind_bc)then
        pol%indbc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)=1
    else
        pol%indbc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc)=0
        df=betta_til *demo%sr(jc,tc)              ! discount factor: notice: sr is prob to survive from jc to jc+1 
        rhs_foc=df*f_aftertaxR(R_til_tp1,tau_k)*evp_coh_tp1         ! term in FOC(c)
        
        if (rhs_foc<=0.0_dp .and. df/=0.0_dp)then
            print*, "messed up rhs",jc,ec,dc
            pause
        endif        
        
    endif
                            
    call sub_solvehh_dec(sav,pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), & 
        pol%leis(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%lab_eff(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),time_cost, &
        grid%coh(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), & 
        grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), assmin, assmax,&
        pol%grwage(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%grwageinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), pol%grinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        pol%netinc(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), pol%netsav(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        grpstk, & 
        pstk, savp, & 
        pol%v(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%v_nokid(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        pol%inv_vp_x(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%inv_vp_pgr(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), & 
        gr_wage,net_wage,wage_totax,agg%wage_s(sc,tc),ben_p,R,rhs_foc,pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),ind_bc, &
        pol%labinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%hsvtmp1(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%hsvtmp2(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        pol%capinctaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        pol%constaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        pol%penstaxrv(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),bq(sc,jc),ben_u, &
        demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc),0.0_dp,flag_stud,flag_invalid,pol%net_trr(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,1,1,jc,tc),ext_flg,opt_input) 
    
    
    ! check budget constraint
    call sub_checkbudget(pol%cons(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),pol%lab(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc), &
        sav,grid%ass(dc,xc,pc,yc,ec,wc,gc,kc,hc,sc,ickid,scpar,jc,tc),bq(sc,jc),net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,gc,sc,jc,tc,demo%numchild_gsj(gc,sc,jc,tc),avearn,agg%tau_c(tc),agg%tau_k(tc),flag_stud)  
                                        
    end subroutine sub_solvehh_sav
    ! ------------------------------------------------------------------- 
           
    
    ! ------------------------------------------------------------------- 
    subroutine sub_solvehh_dec(sav,cons,leis,lab_eff,tcost,coh,ass,assmingr,assmaxgr,grwage,grwageinc,grinc,netinc,netsav,grpstk,pstk,savp,v,v_nokid,inv_vp_x,inv_vp_pgr, & 
        gr_wage,net_wage,wage_totax,agg_wage,ben_p,R,rhs_foc,lab,ind_bc,labinctaxrv,hsvtmp1,hsvtmp2,capinctaxrv,constaxrv,penstaxrv,tr,sa_tr,numchld,avearn,tau_c,tau_k,tau_h, & 
        flag_stud,flag_invalid,net_trr,dc,opt_input)
    ! solution of household problem at given saving grid point / coh grid point
    use nr
    use nrutil
    implicit none
    
    real(dp),intent(inout)::cons,leis,lab_eff,lab,coh,ass,assmingr,assmaxgr,grwage,grwageinc,grinc,netinc,netsav,grpstk,pstk,savp,v,inv_vp_x,inv_vp_pgr,rhs_foc,v_nokid
    real(dp),intent(out):: labinctaxrv,capinctaxrv,constaxrv,penstaxrv,sa_tr,hsvtmp1,hsvtmp2
    integer,intent(in):: flag_stud,opt_input,dc
    integer,intent(out):: net_trr
    real(dp),intent(in)::gr_wage,net_wage,wage_totax,agg_wage,ben_p,R,numchld,avearn,tau_c,tau_k,tau_h,tcost
    real(dp),intent(in):: sav,tr
    integer,intent(in):: flag_invalid
    logical,intent(in)::ind_bc
    real(dp)::util,mu_c,mu_leis,dist,lab_u,util_u,util_dc(2),cons_u
    real(dp)::c0,c1,f0,f1
    real(dp),parameter::epsi=1.0e-06 
    
    
    if (jc<jr(tc)) then ! working period (before mandatory retirement age)


        
        if  (ec==1 ) then  ! employment
            if (opt_input==1)then

                coh = x_grid(xc)
                lab = lab_dc_in_intp(dc,xc)
                cons = cons_dc_in_intp(dc,xc)
                lab_eff = lab
                
                leis=biggam - lab_eff
                

            else
            
            if (ind_bc .and. flag_invalid==0 ) then        ! case of binding borrowing constraint

                if (dc==2)then
                    lab =0.0_dp ! ft_lab * 0.1_dp
                    lab_eff = lab
                    leis=biggam - lab_eff 
                    ! GROSS consumption from BC            
                    cons=f_cons_constr(coh,sav,net_wage,wage_totax,lab,gc,sc,ec,dc,ic,jc,tc,numchld,flag_stud,1) !,agg%inc_thrs(tc))
                else
                    ind_below = 1
                    if (gen_id==1 )then 
                        call sub_corner_cons_lab(cons,sav,lab,coh,net_wage,wage_totax,gc,sc,jc,tc,numchld,flag_stud,agg%tau_c(tcp1),ind_below)     
                    endif
                    lab_eff = lab
                    
                        
                    if (gen_id==1 )then     
                        
                        if (opt_mt==1)then
                        
                            if (lab <= (agg%inc_thrs(tc) / net_wage) ) then
                                ! fine
                                ind_below = 1
                            else
                                ind_below = 0
                                !call sub_corner_cons_lab(cons,sav,lab,coh,net_wage,wage_totax,gc,jc,tc,numchld,flag_stud,agg%tau_c(tcp1),ind_below) 
                                !lab_eff = lab
                                !lab = lab**1.4_dp
                                !
                                !if (lab < (agg%inc_thrs(tc) / net_wage) ) then
                                !    lab = (agg%inc_thrs(tc) / net_wage)
                                !    lab_eff = lab**(1.0_dp/1.4_dp)
                                !    ind_below = 1
                                !endif
                            
                            
                            
                            endif    
                            
                        endif
                        
                    
                    endif
                    
                    
                    
                    
                    leis = biggam - lab_eff
                    !lab =ft_lab! grid%lab(1,1,1)
                    cons=f_cons_constr(coh,sav,net_wage,wage_totax,lab,gc,sc,ec,dc,ic,jc,tc,numchld,flag_stud,ind_below)
                    
                endif
               
                ! NET consumption
                cons = f_netcons(cons,tau_c)
             
                !if (cons<0.0_dp)then
                !    print*, "neg cons bc",jc,pc
                !    pause
                !endif
               
            else    ! no binding constraint, solution for interior labor supply                
                
                ! leisure is fixed
                if (dc==2)then ! zero lab
                    lab =0.0_dp !  ft_lab * 0.1_dp
                    lab_eff = lab
                   
                    leis=biggam - lab 
                else
                    !if (gen_id==1 )then
                        ind_below = 1
                        lab = func_lab(rhs_foc,net_wage,gc,jc,flag_stud,agg%tau_c(tcp1),ind_below)
                        lab_eff = lab
                     
                            
                        if (opt_mt==1)then
                        
                            if (lab <= (agg%inc_thrs(tc) / net_wage) ) then
                                ! fine
                                ind_below = 1
                            else
                                ind_below = 0
                                !lab = func_lab(rhs_foc,net_wage,gc,jc,flag_stud,agg%tau_c(tcp1),ind_below)
                                !lab_eff = lab
                                !lab = lab**1.4_dp
                                !
                                !if (lab < (agg%inc_thrs(tc) / net_wage) ) then
                                !    lab = (agg%inc_thrs(tc) / net_wage)
                                !    lab_eff = lab**(1.0_dp/1.4_dp)
                                !    ind_below = 1
                                !endif
                            
                            
                            
                            endif
                            
                        endif
                        
                        
                            
                    !endif
                    
                    leis=biggam - lab
                endif 
                
                     
              
                ! closed form solution for consumption: interior solution
                cons=f_cons_dc(rhs_foc,0.0_dp,ec,dc)
                ! NET consumption
                !cons = f_netcons(cons,tau_c)
                
                !if (cons<0.0_dp)then
                !    print*, "neg cons",jc
                !    pause
                !endif
                
                
                ! check cons FOC
                call sub_muc(cons,0.0_dp,mu_c)
                dist=abs(mu_c/rhs_foc-1.0_dp)
                if ( dist>epsi .and. cons>epsi ) then
                    print*, 'first-order condition for consumption violated singles',mu_c,rhs_foc,cons,leis,rhs_foc**(-1.0_dp)*pheye * leis**( (1.0-pheye)*(1.0-tetta) )
                    pause
                endif
            
                ! endogenous coh grid:
                coh=f_grid_coh(sav,cons,lab,net_wage,wage_totax,ic,ec,dc,gc,sc,jc,tc,numchld,avearn,tau_c,flag_stud)
                
             endif   
                
            endif
        
         
endif
       
    else    ! if (jc>=jr(tc)): retirement (after mand age)
        leis=biggam
        
        !print*, "ret", lab, lab_eff
        !pause
        
        !lab = 0.0_dp
        !lab_eff = 0.0_dp
        if (ind_bc .and. flag_invalid==0 ) then
            cons=coh + sav
            cons = f_netcons(cons,tau_c)
        else
            cons=f_cons_dc(rhs_foc,0.0_dp,ec,dc)
            ! NET consumption
           ! cons = f_netcons(cons,tau_c)
           
            coh=f_grid_coh(sav,cons,lab,net_wage,wage_totax,ic,ec,dc,gc,sc,jc,tc,numchld,avearn,tau_c,flag_stud)
        
            ! check cons first-order condition:
            call sub_muc(cons,0.0_dp,mu_c)
            dist=abs(mu_c/rhs_foc-1.0_dp)
         
            if ( dist>epsi .and. cons>epsi  ) then
               
                print*, 'first-order condition for consumption violated ret mand singles', jc,ec,dc,ecp1,dcp1,net_wage,ben_p,agg%wage(tc)
                pause
            endif

        endif
    endif
    
    ! endogenous assets
    ass=f_grid_ass(coh,bq(sc,jc),R,net_wage,ben_p,tau_k,ec,dc,jc,ic)
    assmingr=f_grid_ass(coh,bq(sc,jc),R,net_wage*demo%grid_epsi(1),ben_p,tau_k,ec,dc,jc,ic)
    assmaxgr=f_grid_ass(coh,bq(sc,jc),R,net_wage*demo%grid_epsi(nw),ben_p,tau_k,ec,dc,jc,ic)

    
    !print*, ic,sc,gc,ass,coh
    !pause
    
    ! compute net income and net saving
    call sub_incsav(ass,tr,cons,lab,R,gr_wage,gammafe(sc,kc),net_wage,wage_totax,ben_p,gr_ben_p,ec,dc,grwage,grwageinc,grinc,netinc,netsav, &
        labinctaxrv,capinctaxrv,constaxrv,penstaxrv,gc,sc,jc,tc,1,sa_tr,numchld,avearn,tau_c,tau_k,hsvtmp1,hsvtmp2,net_trr) !,agg%inc_thrs(tc))
                         
    ! value function: 
    util=f_util(cons,lab_eff,tcost,gc,ec,dc,sc,gen_id,flag_stud)
    v=f_vfun(util,ev_tp1,betta_til,demo%sr(jc,tc) )
    v_nokid=f_vfun(util,ev_tp1_nokid,betta_til,demo%sr(jc,tc) )
                                
    ! inverse derivative of vfun w.r.t. coh
    call sub_muc(cons,0.0_dp,mu_c)
    ! inverse of derivative of value function w.r.t. coh 
    inv_vp_x=f_inv_vp_coh(mu_c) 
     
    end subroutine sub_solvehh_dec 
    ! ------------------------------------------------------------------- 
        
                
end subroutine sub_solvehh
! ------------------------------------------------------------------- 
 

! ------------------------------------------------------------------- 
subroutine sub_checkbudget(cons,lab,sav,ass,tr,net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,gc,sc,jc,tc,numchld,avearn,tau_c,tau_k,flag_stud)
                                        
implicit none
                                        
real(dp),intent(in)::cons,lab,sav,ass,tr,net_wage,wage_totax,ben_p,R,numchld,avearn,tau_c,tau_k
integer,intent(in)::ic,ec,dc,gc,jc,tc,flag_stud,sc
logical,intent(in)::ind_bc
real(dp)::tot_res,tot_exp,dist,grinc
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc, ind_below

ndlloc = ndl
                                    
tot_res=(ass+tr)* f_aftertaxR(R,tau_k)

if (lab * net_wage <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif

if (ec==1 ) then
  
    tot_res=tot_res+net_wage * biggam
    

else !then
    tot_res=tot_res+ben_p
endif
                                        
tot_exp=cons*(1.0_dp+tau_c) 
if (ec==1 ) then
    
    grinc = net_wage* lab
    if (grinc<=inc_thrs_param ) then
        ind_below=1
    else
        ind_below=0
    endif
    
    
    tot_exp=tot_exp+net_wage*(biggam-lab) + fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
   
endif

!if (ind_bc==0) then   ! add expenditures on savings
    ! recall the dynamic budget constraint from standard model:
    ! S+C+w*Leis = A(1+r)+w
    tot_exp=tot_exp+sav
!endif
    
if (flag_stud>0)then
    if (flag_stud==2)then
        tot_exp = tot_exp + (fee_flow - col_subs_flow)
    else
        tot_exp = tot_exp + (fee_flow - col_subs_flow)*0.5_dp
    endif
    
endif
                                        
dist=abs(tot_exp-tot_res)   ! take absolute rather than relative distance measure b/c you may have very small numbers

if ( dist>epsi ) then
    print*, 'budget constraint does not hold sing',ind_bc,ec,dc,dist,tot_exp,tot_res,tr,ben_p,cons,sav,lab,sc,jc,jt
    pause
endif
                                        
end subroutine sub_checkbudget
! ------------------------------------------------------------------- 



! ------------------------------------------------------------------- 
subroutine sub_checkbudget_cpl(cons,lab1,lab2,sav,ass,tr,net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,jc,tc,numchld,avearn,tau_c,tau_k)
                                        
implicit none
                                        
real(dp),intent(in)::cons,lab1,lab2,sav,ass,tr,net_wage(:),wage_totax(:),ben_p(:),R,numchld,avearn,tau_c,tau_k
integer,intent(in)::ic,ec,dc,jc,tc
logical,intent(in)::ind_bc
real(dp)::tot_res,tot_exp,dist,grinc
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc, ind_below

ndlloc = ndl*ndl
                                    
tot_res=(ass+tr)* f_aftertaxR(R,tau_k)

if (lab1 * net_wage(1) + lab2 * net_wage(2) <= inc_thrs_param ) then
        ind_below = 1
    else
        ind_below = 0
    endif

if (ec==1 ) then
  
    tot_res=tot_res+net_wage(1) * biggam + net_wage(2) * biggam
    

else !then
    tot_res=tot_res+ben_p(1) + ben_p(2)
endif
                                        
tot_exp=cons*(1.0_dp+tau_c) 
if (ec==1 ) then
    
    grinc = net_wage(1)* lab1 +net_wage(2)* lab2  
    if (grinc<=inc_thrs_param ) then
        ind_below=1
    else
        ind_below=0
    endif
    
    
    tot_exp=tot_exp+net_wage(1)*(biggam-lab1) + net_wage(2)*(biggam-lab2) + fun_hsv_new(wage_totax(1)*lab1+wage_totax(2)*lab2,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
   
endif

!if (ind_bc==0) then   ! add expenditures on savi2s
    ! recall the dynamic budget constraint from standard model:
    ! S+C+w*Leis = A(1+r)+w
    tot_exp=tot_exp+sav
!endif

                                        
dist=abs(tot_exp-tot_res)   ! take absolute rather than relative distance measure b/c you may have very small numbers

if ( dist>epsi ) then
    print*, 'budget constraint does not hold couple',ind_bc,ec,dc,dist,tot_exp,tot_res,tr,ben_p,cons,sav,lab1,jc,jt
    pause
endif
                                        
end subroutine sub_checkbudget_cpl
! ------------------------------------------------------------------- 

! ------------------------------------------------------------------- 
subroutine sub_checkbudget_parent(cons,expend,lab,sav,ass,tr,net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,gc,jc,tc,numchld,avearn,tau_c,tau_k)
                                        
implicit none
                                        
real(dp),intent(in)::cons,lab,sav,ass,tr,net_wage,ben_p,R,numchld,avearn,tau_c,tau_k,expend,wage_totax
integer,intent(in)::ic,ec,dc,gc,jc,tc
logical,intent(in)::ind_bc
real(dp)::tot_res,tot_exp,dist, grinc
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc, ind_below

ndlloc = ndl
                                    
tot_res=(ass+tr)* f_aftertaxR(R,tau_k)

if (ec==1) then
  
    tot_res=tot_res+net_wage * biggam
    

else !then
    tot_res=tot_res+ben_p
endif
                                        
tot_exp=cons*(1.0_dp+tau_c) + expend *numchld

if (ec==1 ) then
    
    grinc = net_wage *lab
    if (grinc<=inc_thrs_param ) then
        ind_below=1
    else
        ind_below=0
    endif
    
    tot_exp=tot_exp+net_wage*(biggam-lab) + fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
   
endif

!if (ind_bc==0) then   ! add expenditures on savings
    ! recall the dynamic budget constraint from standard model:
    ! S+C+w*Leis = A(1+r)+w
    tot_exp=tot_exp+sav
!endif
                                        
dist=abs(tot_exp-tot_res)   ! take absolute rather than relative distance measure b/c you may have very small numbers

if ( dist>epsi ) then
    print*, 'budget constraint does not hold parent',ind_bc,ec,dc,dist,tot_exp,tot_res,tr,ben_p,cons,sav,lab,jc,jt
    pause
endif
                                        
end subroutine sub_checkbudget_parent
! ------------------------------------------------------------------- 


! ------------------------------------------------------------------- 
subroutine sub_checkbudget_parent_cpl(cons,expend,lab1,lab2,sav,ass,tr,net_wage,wage_totax,ben_p,R,ic,ec,dc,ind_bc,jc,tc,numchld,avearn,tau_c,tau_k)
                                        
implicit none
                                        
real(dp),intent(in)::cons,lab1,lab2,sav,ass,tr,net_wage(:),ben_p(:),R,numchld,avearn,tau_c,tau_k,expend,wage_totax(:)
integer,intent(in)::ic,ec,dc,jc,tc
logical,intent(in)::ind_bc
real(dp)::tot_res,tot_exp,dist, grinc
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc, ind_below

ndlloc = ndl*ndl
                                    
tot_res=(ass+tr)* f_aftertaxR(R,tau_k)

if (ec==1 ) then
  
    tot_res=tot_res+net_wage(1) * biggam + net_wage(2) * biggam
    

else !then
    tot_res=tot_res+ben_p(1) + ben_p(2)
endif
                                        
tot_exp=cons*(1.0_dp+tau_c) + expend *numchld

if (ec==1 ) then
    
    grinc = net_wage(1) *lab1 + net_wage(2) *lab2
    if (grinc<=inc_thrs_param ) then
        ind_below=1
    else
        ind_below=0
    endif
    
    tot_exp=tot_exp+net_wage(1)*(biggam-lab1) +net_wage(2)*(biggam-lab2) + fun_hsv_new(wage_totax(1)*lab1+wage_totax(2)*lab2,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
   
endif

!if (ind_bc==0) then   ! add expenditures on savi2s
    ! recall the dynamic budget constraint from standard model:
    ! S+C+w*Leis = A(1+r)+w
    tot_exp=tot_exp+sav
!endif
                                        
dist=abs(tot_exp-tot_res)   ! take absolute rather than relative distance measure b/c you may have very small numbers

if ( dist>epsi ) then
    print*, 'budget constraint does not hold parent cpl',ind_bc,ec,dc,dist,tot_exp,tot_res,tr,ben_p(1),cons,sav,lab1,jc,jt
    pause
endif
                                        
end subroutine sub_checkbudget_parent_cpl
! ------------------------------------------------------------------- 

! -------------------------------------------------------------------------------
function f_cons_constr(coh,sav,net_wage,wage_totax,lab,gc,sc,ec,dc,ic,jc,tc,numchld,flag_stud,ind_below)

implicit none
real(dp):: f_cons_constr
real(dp),intent(in):: coh,sav,net_wage,lab,numchld,wage_totax
integer,intent(in):: gc,ec,dc,ic,jc,tc,flag_stud,ind_below,sc
!integer:: ind_below
real(dp):: grinc

grinc = net_wage * lab


f_cons_constr = coh-net_wage*(biggam - lab) - fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,ind_below )  - sav

if (flag_stud>0)then
    if (flag_stud==2)then
        f_cons_constr = f_cons_constr - (fee_flow - col_subs_flow)
    else
        f_cons_constr = f_cons_constr - (fee_flow - col_subs_flow)*0.5_dp
    endif
    
endif

end function f_cons_constr
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
function f_cons_constr_cpl(coh,sav,net_wage,wage_totax,lab1,lab2,ec,dc,ic,jc,tc,numchld,ind_below)

implicit none
real(dp):: f_cons_constr_cpl
real(dp),intent(in):: coh,sav,net_wage(:),lab1,lab2,numchld,wage_totax(:)
integer,intent(in):: ec,dc,ic,jc,tc,ind_below
!integer:: ind_below
real(dp):: grinc

grinc = net_wage(1) * lab1 + net_wage(2) * lab2


f_cons_constr_cpl = coh-net_wage(1)*(biggam - lab1)-net_wage(2)*(biggam - lab2)  - fun_hsv_new(wage_totax(1)*lab1+wage_totax(2)*lab2,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below )  - sav


end function f_cons_constr_cpl
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
function f_coh_tp1(sav,tr_tp1,R_til_tp1,net_wage_tp1,pens_tp1,lamt,tauk_tp1,ecp1,dcp1)
! law of motion for cash-on-hand                            
implicit none
                            
real(dp)::f_coh_tp1
real(dp),intent(in)::sav,tr_tp1,R_til_tp1,net_wage_tp1,pens_tp1,lamt,tauk_tp1
integer,intent(in)::ecp1,dcp1

                        
f_coh_tp1=(sav+tr_tp1*(1.0_dp+lamt))*f_aftertaxR(R_til_tp1,tauk_tp1)
if ( ecp1==1 .and. dcp1<=ndl) then
   
    f_coh_tp1=f_coh_tp1+net_wage_tp1 * biggam
    
elseif ( (ecp1==1 .and. dcp1==ndl+1) )then ! unemployment
    print*, "should not be here6"
    pause
elseif ( (ecp1==1 .and. dcp1==nd) .or. ecp1==ne)then
     f_coh_tp1=f_coh_tp1+pens_tp1
else
    print*, "other cases?"
endif 
    
end function f_coh_tp1
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
function f_coh_tp1_cpl(sav,tr_tp1,R_til_tp1,net_wage_tp1,pens_tp1,lamt,tauk_tp1,ecp1,dcp1)
! law of motion for cash-on-hand                            
implicit none
                            
real(dp)::f_coh_tp1_cpl
real(dp),intent(in)::sav,tr_tp1,R_til_tp1,net_wage_tp1(:),pens_tp1(:),lamt,tauk_tp1
integer,intent(in)::ecp1,dcp1

                        
f_coh_tp1_cpl=(sav+tr_tp1*(1.0_dp+lamt))*f_aftertaxR(R_til_tp1,tauk_tp1)
if ( ecp1==1 ) then
   
    f_coh_tp1_cpl=f_coh_tp1_cpl+net_wage_tp1(1) * biggam+net_wage_tp1(2) * biggam

elseif ( (ecp1==1 .and. dcp1==nd_cpl) .or. ecp1==ne)then
     f_coh_tp1_cpl=f_coh_tp1_cpl+pens_tp1(1)+pens_tp1(2)
else
    print*, "other cases?"
endif
    
end function f_coh_tp1_cpl
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
function f_grid_coh(sav,cons,lab,net_wage,wage_totax,ic,ec,dc,gc,sc,jc,tc,numchld,avearn,tau_c,flag_stud)
! grid of cash on hand
implicit none
real(dp)::f_grid_coh
real(dp),intent(in)::sav,cons,lab,net_wage,numchld,avearn,tau_c,wage_totax
real(dp):: temp
integer,intent(in)::ic,ec,dc,gc,jc,tc,flag_stud,sc
real(dp),parameter::min_x=1.0e-08_dp
integer:: ndlloc, ind_below
real(dp):: grinc


ndlloc = ndl
                                
f_grid_coh=sav+cons*(1.0_dp+tau_c)
if ( ec==1  ) then    

    grinc = net_wage * lab
    if (grinc<=inc_thrs_param ) then
        ind_below=1
    else
        ind_below=0
    endif
    f_grid_coh=f_grid_coh+net_wage*(biggam - lab) + fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,1,jc,tc,numchld,avearn,ind_below ) 
  
endif

if (flag_stud >0)then
    if (flag_stud==2)then
        f_grid_coh = f_grid_coh +fee_flow - col_subs_flow
    else
        f_grid_coh = f_grid_coh +(fee_flow - col_subs_flow)*0.5_dp
    endif
    
endif

!if ( f_grid_coh<0.0_dp ) then
!    print*, 'something is wrong with endogenous coh grid sng', f_grid_coh,net_wage,leis
!    pause
!endif

if (isnan(f_grid_coh))then
    print*, "nan coh", net_wage,lab,sav,cons,jc,wage_totax,lab !,fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,2,gc,ec,dc,1,jc,tc,numchld,avearn,ind_below ) ,numchld
    pause
endif


end function f_grid_coh
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
function f_grid_coh_cpl(sav,cons,lab1,lab2,net_wage,wage_totax,ic,ec,dc,jc,tc,numchld,avearn,tau_c)
! grid of cash on hand
implicit none
real(dp)::f_grid_coh_cpl
real(dp),intent(in)::sav,cons,lab1,lab2,net_wage(:),numchld,avearn,tau_c,wage_totax(:)
real(dp):: temp
integer,intent(in)::ic,ec,dc,jc,tc
real(dp),parameter::min_x=1.0e-08_dp
integer:: ndlloc, ind_below
real(dp):: grinc


ndlloc = ndl*ndl
                                
f_grid_coh_cpl=sav+cons*(1.0_dp+tau_c)
if ( ec==1  ) then    

    grinc = net_wage(1) * lab1 + net_wage(2) * lab2 
    if (grinc<=inc_thrs_param ) then
        ind_below=1
    else
        ind_below=0
    endif
    f_grid_coh_cpl=f_grid_coh_cpl+net_wage(1)*(biggam - lab1)+net_wage(2)*(biggam - lab2) + & 
        fun_hsv_new(wage_totax(1)*lab1+wage_totax(2)*lab2,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,1,jc,tc,numchld,avearn,ind_below ) 
  
endif

!if ( f_grid_coh<0.0_dp ) then
!    print*, 'somethi2 is wro2 with endogenous coh grid s2', f_grid_coh,net_wage,leis
!    pause
!endif

end function f_grid_coh_cpl
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
function f_grid_ass(coh,tr,R,net_wage,ben_p,tau_k,ec,dc,jc,ic)
! compute assets from coh and income
implicit none

real(dp)::f_grid_ass
real(dp),intent(in)::coh,tr,R,net_wage,ben_p,tau_k
integer,intent(in)::ec,dc,jc,ic
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc

ndlloc = ndl
    
if (ec==1 ) then
   
    f_grid_ass=coh-net_wage * biggam 
    

else
    f_grid_ass=coh-ben_p
endif
f_grid_ass=f_grid_ass/f_aftertaxR(R,tau_k)-tr
    
end function f_grid_ass
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
function f_grid_ass_cpl(coh,tr,R,net_wage,ben_p,tau_k,ec,dc,jc,ic)
! compute assets from coh and income
implicit none

real(dp)::f_grid_ass_cpl
real(dp),intent(in)::coh,tr,R,net_wage(:),ben_p(:),tau_k
integer,intent(in)::ec,dc,jc,ic
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc

ndlloc = ndl*ndl
    
if (ec==1 ) then
   
    f_grid_ass_cpl=coh-net_wage(1) * biggam-net_wage(2) * biggam
    

else
    f_grid_ass_cpl=coh-ben_p(1) - ben_p(2)
endif
f_grid_ass_cpl=f_grid_ass_cpl/f_aftertaxR(R,tau_k)-tr
    
end function f_grid_ass_cpl
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
function f_coh_cpl(ass,tr,R,net_wage,ben_p,ec,dc,ic,tau_k)
! compute coh from assets and income
implicit none

real(dp)::f_coh_cpl
real(dp),intent(in)::ass,tr,R,net_wage(:),ben_p(:),tau_k
integer,intent(in)::ec,dc,ic
real(dp),parameter::epsi=1.0e-06
real(dp)::coh
integer:: ndlloc
 
ndlloc = ndl*ndl
    
coh=(ass+tr)* f_aftertaxR(R,tau_k)
if (ec==1 ) then
   
    f_coh_cpl=coh+net_wage(1) * biggam+net_wage(2) * biggam

else        
    f_coh_cpl=coh+ben_p(1) + ben_p(2)
endif
    
end function f_coh_cpl
! -------------------------------------------------------------------------------


! -------------------------------------------------------------------------------
function f_coh(ass,tr,R,net_wage,ben_p,ec,dc,ic,sc,tau_k)
! compute coh from assets and income
implicit none

real(dp)::f_coh
real(dp),intent(in)::ass,tr,R,net_wage,ben_p,tau_k
integer,intent(in)::ec,dc,ic,sc
real(dp),parameter::epsi=1.0e-06
real(dp)::coh
integer:: ndlloc
 
ndlloc = ndl
    
coh=(ass+tr)* f_aftertaxR(R,tau_k)
if (ec==1) then
   
    f_coh=coh+net_wage * biggam

else        
    f_coh=coh+ben_p
endif
    
end function f_coh
! -------------------------------------------------------------------------------

! -------------------------------------------------------------------------------
subroutine sub_incsav(ass,tr,cons,lab,R,gr_wage,fe,net_wage,wage_totax,ben_p,gr_ben_p,ec,dc,grwage,grwageinc,grinc,netinc,netsav, &
    labinctaxrv,capinctaxrv,constaxrv,penstaxrv,gc,sc,jc,tc,ic,sa_tr,numchld,avearn,tau_c,tau_k,hsvtmp1,hsvtmp2,net_trr) !,inc_thrs)
! compute net income and net saving 
implicit none

real(dp),intent(in)::ass,tr,cons,lab,R,gr_wage,net_wage,wage_totax,ben_p,gr_ben_p,numchld,avearn,tau_c,tau_k,fe
integer,intent(out):: net_trr
integer,intent(in)::ec,dc,gc,jc,tc,ic,sc
real(dp),intent(out)::grinc,grwage,grwageinc,netinc,netsav,labinctaxrv,capinctaxrv,constaxrv,penstaxrv,sa_tr,hsvtmp1,hsvtmp2
real(dp):: equiv
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc, ind_below

ndlloc = ndl
    
grinc=(ass+tr)*(R-1.0_dp) ! gross capital income    
netinc=grinc - f_captax(ass+tr,R,tau_k)        ! net of capital income taxes   
grwageinc=0.0_dp
labinctaxrv=0.0_dp
hsvtmp1 = 0.0_dp
hsvtmp2 = 0.0_dp
capinctaxrv=f_captax(ass+tr,R,tau_k)
constaxrv=cons*tau_c
penstaxrv=0.0_dp

sa_tr = 0.0_dp

net_trr = 0

if (ec==1)then
     grwageinc=gr_wage *lab
        
        grwage = gr_wage
        
        !print*, "grwageinc", grwageinc, gr_wage
        !pause
        
        grinc=grinc+gr_wage*lab
        
        if ( (lab * net_wage) <= inc_thrs_param)then
            ind_below=1
        else
            ind_below=0
        endif
         
        
        netinc=net_wage*lab - fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
        
        equiv =1.0_dp
        if (jc>=jf .and. jc<=jt .and. gc==2)then
            equiv = equiv + 0.3_dp * numchld
        endif
        netinc = netinc / equiv
        
        labinctaxrv = fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,ind_below) !,avearn_base,ind_below ) 
        
 
        
        call sub_hsv_tmp(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,hsvtmp1,hsvtmp2,ind_below )
        
        if ( (fun_hsv_new(wage_totax*lab,0.0_dp,0.0_dp,lab,0.0_dp,0.0_dp,2,gc,sc,ec,dc,ic,jc,tc,numchld,avearn,ind_below ))<0.0_dp) then
            net_trr = 1
        else
            net_trr = 0
        endif
      
else
    netinc=netinc+ben_p
    penstaxrv = fun_hsv_pens(gr_ben_p,0.0_dp,0.0_dp,2,gc,ec,dc,ic,jc,tc,avearn)
endif

netsav=netinc-cons
    
end subroutine sub_incsav
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
subroutine sub_incsav_cpl(ass,tr,cons,lab1,lab2,R,gr_wage,fe,net_wage,wage_totax,ben_p,gr_ben_p,ec,dc,grwage,grwageinc,grinc,netinc,netsav, &
    labinctaxrv,capinctaxrv,constaxrv,penstaxrv,jc,tc,ic,sa_tr,numchld,avearn,tau_c,tau_k,hsvtmp1,hsvtmp2,net_trr) !,inc_thrs)
! compute net income and net savi2 
implicit none

real(dp),intent(in)::ass,tr,cons,lab1,lab2,R,gr_wage(:),net_wage(:),wage_totax(:),ben_p(:),gr_ben_p(:),numchld,avearn,tau_c,tau_k,fe
integer,intent(out):: net_trr
integer,intent(in)::ec,dc,jc,tc,ic
real(dp),intent(out)::grinc,grwage,grwageinc,netinc,netsav,labinctaxrv,capinctaxrv,constaxrv,penstaxrv,sa_tr,hsvtmp1,hsvtmp2
real(dp):: equiv
real(dp),parameter::epsi=1.0e-06
integer:: ndlloc, ind_below

ndlloc = ndl*ndl
    
grinc=(ass+tr)*(R-1.0_dp) ! gross capital income    
netinc=grinc - f_captax(ass+tr,R,tau_k)        ! net of capital income taxes   
grwageinc=0.0_dp
labinctaxrv=0.0_dp
hsvtmp1 = 0.0_dp
hsvtmp2 = 0.0_dp
capinctaxrv=f_captax(ass+tr,R,tau_k)
constaxrv=cons*tau_c
penstaxrv=0.0_dp 

sa_tr = 0.0_dp
 
net_trr = 0

if (ec==1)then
       
        grwageinc=gr_wage(1)*lab1 + gr_wage(2)*lab2
        
        grwage = gr_wage(1) + gr_wage(2)
        
        !print*, "grwageinc", grwageinc, gr_wage
        !pause
        
        grinc=grinc+gr_wage(1)*lab1+gr_wage(2)*lab2
        
        !if ( (lab * net_wage) <= inc_thrs_param)then
        !    ind_below=1
        !else
        !    ind_below=0
        !endif
         
        
        netinc=net_wage(1)*lab1 + net_wage(2)*lab2 - fun_hsv_new(wage_totax(1)*lab1 +wage_totax(2)*lab2 ,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
        
        equiv = 1.5_dp
        if (jc>=jf .and. jc<=jt)then
            equiv = equiv + 0.3_dp * numchld
        endif
        netinc = netinc / equiv
        
        labinctaxrv = fun_hsv_new(wage_totax(1)*lab1 +wage_totax(2)*lab2 ,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) 
        
 
        
        call sub_hsv_tmp(wage_totax(1)*lab1+wage_totax(1)*lab1,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,hsvtmp1,hsvtmp2,ind_below )
        
        if ( (fun_hsv_new(wage_totax(1)*lab1 +wage_totax(2)*lab2 ,wage_totax(1)*lab1,wage_totax(2)*lab2,lab1+lab2,lab1,lab2,1,1,1,ec,dc,ic,jc,tc,numchld,avearn,ind_below ) )<0.0_dp) then
            net_trr = 1
        else
            net_trr = 0
        endif
        
    
else
    netinc=netinc+ben_p(1) + ben_p(2)
    penstaxrv = fun_hsv_pens(gr_ben_p(1),0.0_dp,0.0_dp,2,1,ec,dc,ic,jc,tc,avearn)
endif

netsav=netinc-cons
    
end subroutine sub_incsav_cpl
! -------------------------------------------------------------------------------





! **************************************************************************************
! Utility, derivative of utility, value function
! **************************************************************************************
    
! --------------------------------------------------------------------------------------
function f_util(cons,lab,tcost,gc,ec,dc,sc,gen_id,flag_stud)
! Utility Function for non-parents / non investing
implicit none
real(dp)::f_util
real(dp),intent(in)::cons,lab,tcost
integer,intent(in):: flag_stud
integer,intent(in):: gc,ec,dc,gen_id,sc
real(dp),parameter::epsi=1.0e-06
real(dp)::cons0,leis0,cfac,pccons,temp,util_cons,disutil_lab,disutil_t,lab0
real(dp),parameter::pen_scale=100000

! adjustment to deal with small numbers
cons0=max(cons,epsi)    

cfac=1.0_dp 
pccons=cons0/cfac	! per scaled capita consumption
 
util_cons = log(pccons)

if (gc==1)then
    lab0=lab
else
    lab0=lab !/2.0_dp ! equivalence scale corretion for couples
endif

if (ec==1 )then
    disutil_lab = lab**(1.0_dp+pssi(gc)) / (1.0_dp+pssi(gc))
else
    disutil_lab = 0.0_dp
endif



f_util = util_cons -  pheye(1,gc) * disutil_lab 

if (lab>0.0_dp * ft_lab)  f_util = f_util - psi_ls

if (flag_stud>0)then
    f_util = f_util - tcost
endif


! correction in case of violation of lower bound
if (cons<epsi) then
    f_util=f_util*(1.0_dp-pen_scale*((cons-epsi)/cfac)**2.0_dp) 
    if (f_util>0.0_dp) f_util=-f_util
endif

if (isnan(f_util))then
    print*,"util", util_cons , pheye(gen_id,gc) , disutil_lab, lab
endif


end function f_util
! --------------------------------------------------------------------------------------
    
! --------------------------------------------------------------------------------------
function f_util_cpl(cons,lab1,lab2,ec,dc,sc1,sc2,gen_id)
! Utility Function for non-parents / non investi2
implicit none
real(dp)::f_util_cpl
real(dp),intent(in)::cons,lab1,lab2
integer,intent(in):: ec,dc,gen_id,sc1,sc2
real(dp),parameter::epsi=1.0e-06
real(dp)::cons0,leis0,cfac,pccons,temp,util_cons,disutil_lab,disutil_t,lab0
real(dp),parameter::pen_scale=100000

! adjustment to deal with small numbers
cons0=max(cons,epsi)    

cfac=1.0_dp
pccons=cons0/cfac	! per scaled capita consumption
 
util_cons = log(pccons)

if (ec==1 )then
    disutil_lab =pheye(1,1) * lab1**(1.0_dp+pssi(1)) / (1.0_dp+pssi(1)) +pheye(1,2) * lab2**(1.0_dp+pssi(2)) / (1.0_dp+pssi(2))
else
    disutil_lab = 0.0_dp
endif



f_util_cpl = util_cons -   disutil_lab 

if (lab1>0.0_dp * ft_lab)  f_util_cpl = f_util_cpl - psi_ls
if (lab2>0.0_dp * ft_lab)  f_util_cpl = f_util_cpl - psi_ls

! correction in case of violation of lower bound
if (cons<epsi) then
    f_util_cpl=f_util_cpl*(1.0_dp-pen_scale*((cons-epsi)/cfac)**2.0_dp) 
    if (f_util_cpl>0.0_dp) f_util_cpl=-f_util_cpl
endif

if (isnan(f_util_cpl))then
    print*, "util cpl nan", util_cons , pheye(gen_id,1) , disutil_lab, lab1,lab2
endif


end function f_util_cpl
! -------------------------------------------------------------------------------------- 
    
! --------------------------------------------------------------------------------------
function f_util_parent_single(cons,lab,t,nkid,gc,ec,dc,jc_kid,sc)
! Utility Function for parents, when investing in children
implicit none
real(dp)::f_util_parent_single
real(dp),intent(in)::cons,lab,t,nkid
integer,intent(in):: gc,ec,dc,jc_kid,sc
real(dp),parameter::epsi=1.0e-06
real(dp)::cons0,cfac,pccons,temp,util_cons,disutil_labt,labt,tinv,disutil_tinv 
real(dp),parameter::pen_scale=100000

! adjustment to deal with small numbers
cons0=max(cons,epsi)    

cfac=1.0_dp
pccons=cons0/cfac	

util_cons = log(pccons)  

! combine labor and time inv
labt = lab !+ nkid * t *kappat_param
tinv = nkid * t 
! for married equivalence scale adjustment
!if (gc==2) labt = labt / 2.0_dp

! disutility
disutil_labt = labt**(1.0_dp+pssi(gc)) / (1.0_dp+pssi(gc))
disutil_tinv = tinv**(1.0_dp+pssi(gc)) / (1.0_dp+pssi(gc))

! full utility
f_util_parent_single = util_cons - pheye(1,gc) * disutil_labt - kappat_param * disutil_tinv 

if (lab>0.0_dp * ft_lab)  f_util_parent_single = f_util_parent_single - psi_ls


! correction in case of violation of lower bound
if (cons<epsi) then
    f_util_parent_single=f_util_parent_single*(1.0_dp-pen_scale*((cons-epsi)/cfac)**2.0_dp) 
    if (f_util_parent_single>0.0_dp) f_util_parent_single=-f_util_parent_single
endif

end function f_util_parent_single 
! --------------------------------------------------------------------------------------    
    

  
! --------------------------------------------------------------------------------------
function f_util_parent_cpl(cons,lab1,lab2,t1,t2,nkid,ec,dc,jc_kid,sc1,sc2)
! Utility Function for parents, when investi2 in children
implicit none
real(dp)::f_util_parent_cpl
real(dp),intent(in)::cons,lab1,lab2,t1,t2,nkid
integer,intent(in):: ec,dc,jc_kid,sc1,sc2
real(dp),parameter::epsi=1.0e-06
real(dp)::cons0,cfac,pccons,temp,util_cons,disutil_labt,tinv,disutil_tinv ,tinv1,tinv2
real(dp),parameter::pen_scale=100000

! adjustment to deal with small numbers
cons0=max(cons,epsi)    

cfac=1.0_dp
pccons=cons0/cfac	

util_cons = log(pccons)  

! combine labor and time inv
!labt = lab  !+ nkid * t *kappat_param
tinv1 = nkid * t1
tinv2 = nkid *t2
! for married equivalence scale adjustment
!if (gc==2) labt = labt / 2.0_dp

! disutility
disutil_labt =pheye(1,1) * lab1**(1.0_dp+pssi(1)) / (1.0_dp+pssi(1)) + pheye(1,2) * lab2**(1.0_dp+pssi(2)) / (1.0_dp+pssi(2))
disutil_tinv =kappat_param * tinv1**(1.0_dp+pssi(1)) / (1.0_dp+pssi(1)) +kappat_param * tinv2**(1.0_dp+pssi(2)) / (1.0_dp+pssi(2))

! full utility
f_util_parent_cpl = util_cons -  disutil_labt -  disutil_tinv 

if (lab1 > 0.0_dp * ft_lab)  f_util_parent_cpl = f_util_parent_cpl - psi_ls
if (lab2 > 0.0_dp * ft_lab)  f_util_parent_cpl = f_util_parent_cpl - psi_ls


! correction in case of violation of lower bound
if (cons<epsi) then
    f_util_parent_cpl=f_util_parent_cpl*(1.0_dp-pen_scale*((cons-epsi)/cfac)**2.0_dp) 
    if (f_util_parent_cpl>0.0_dp) f_util_parent_cpl=-f_util_parent_cpl
endif

end function f_util_parent_cpl 
! --------------------------------------------------------------------------------------    
    


! --------------------------------------------------------------------------------------
subroutine sub_muc(cons,n_child,mu_c)
! Marginal Utility of Consumption 

implicit none

real(dp),intent(in)::cons,n_child
real(dp),intent(out)::mu_c

real(dp)::cfac,cons0
real(dp),parameter::epsi=1.0e-12
real(dp),parameter::pen_scale=100000

! child utility
cfac=1.0_dp+zetta*n_child

! marginal utility of scaled per capita consumption
cons0=max(cons,epsi)

mu_c=cons0**(-1.0_dp)

! correction in case of violation of lower bound
if (cons<epsi) then
    mu_c=mu_c*(1.0_dp+pen_scale*((cons-epsi)/cfac)**2.0_dp) 
endif

end subroutine sub_muc
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_vfun(util,evfun,beta,sr)
! Utility Function
implicit none
real(dp)::f_vfun
real(dp),intent(in)::util,evfun,beta,sr

f_vfun=util+beta*sr*evfun

end function f_vfun
! --------------------------------------------------------------------------------------

! --------------------------------------------------------------------------------------
function f_inv_vp_coh(mu_c)
! inverse of derivative of value function w.r.t coh
implicit none
real(dp)::f_inv_vp_coh
real(dp)::mu_c
                            
f_inv_vp_coh=mu_c**(-1.0_dp)                    
      
end function f_inv_vp_coh
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function f_reinv(vp,c)
! reinverts inverted derivative of value function
implicit none
real(dp)::f_reinv
real(dp),intent(in)::vp,c
                                            
f_reinv=vp**(-1.0_dp)-c
                                            
end function f_reinv
! --------------------------------------------------------------------------------------

                                            
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Functions for computing consumption, labor supply
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! --------------------------------------------------------------------------------------
function f_cons_dc(rhs_foc,n_child,ec,dc)
! Inverse of the marginal utility function
! returns total consumption expenditures
implicit none
real(dp)::f_cons_dc
real(dp),intent(in)::rhs_foc,n_child
integer,intent(in):: ec,dc
real(dp)::cfac,afac,help
real(dp),parameter::epsi=1.0e-06

! take into account child dependency
cfac=1.0_dp+zetta*n_child
!afac=(1.0_dp/cfac)**(pheye*(1.0-tetta))
        
! endogenous labor supply => cases
if ( rhs_foc<=0.0 ) then
    print*,'Trouble Inverting for Consumption',rhs_foc,ec,dc
	pause
endif

f_cons_dc=rhs_foc**(-1.0_dp)

end function f_cons_dc
! --------------------------------------------------------------------------------------   


! --------------------------------------------------------------------------------------
subroutine sub_chkbd(grid_x,x,lb_viol,ub_viol,dc,ec,dcp1,ecp1,indbc)
! correction in case of bound violation                                    
implicit none
                                    
real(dp),dimension(:,:),intent(in)::grid_x
real(dp),intent(inout)::x
logical,intent(out)::ub_viol,lb_viol
integer,intent(in):: dcp1,ecp1,dc,ec
logical,intent(in):: indbc
real(dp)::dist,max_x
real(dp),parameter::epsi=1.0e-04

ub_viol=.false.
lb_viol=.false.

! check upper bound violation
max_x=maxval(grid_x)
dist=x-max_x
if ( dist>0.0_dp ) then
    if ( dist>epsi ) then     ! warning message only for serious bound violation
        print*, 'x too large, how can this be?', ec,dc,ecp1,dcp1,x,max_x
       ! pause
        ub_viol=.true.
        if (opt_pause) pause
    endif
    x=maxval(grid_x)
    return
endif

! check lower bound violation
if (opt_flex_grid==1)then
    dist=x-minval(grid_x)
    if (dist<0.0_dp) then
        if ( abs(dist)>epsi .and. indbc==.false. ) then     ! warni2 message only for serious bound violation
            print*, 'x too small, how can this be?', ec,dc,ecp1,dcp1,x,minval(grid_x)
            !pause
            lb_viol=.true.
            if (opt_pause) pause
        endif
        x=minval(grid_x) 
    endif
endif
                                    
end subroutine sub_chkbd
! --------------------------------------------------------------------------------------

! ----------------------------------------------------------
function func_lab(x,net_wage,gc,jc,flag_stud,tauc,ind_below)
! computes labor supply as function of x
! interior solution: x=rhs_foc
! corner solution: x=1.0
implicit none
real(dp),intent(in)::x,net_wage,tauc
integer,intent(in):: gc,flag_stud,jc,ind_below
real(dp)::func_lab
real(dp):: temp, avearn_loc
if (flag_stud>0)then
    temp =  1.0_dp / ( tau_pr + pssi(gc) )
else
    temp =  1.0_dp / ( tau_pr  + pssi(gc) )
endif

!if (ind_below==1)then
!    avearn_loc = avearn_base
!else
    avearn_loc = avearn
!endif


func_lab = ( 1.0_dp/pheye(1,gc) * x / (1.0_dp + tauc ) * (1.0_dp - tau_pr) * (lambda_pr ) * net_wage**(1.0_dp-tau_pr)  ) ** temp

if (jc==js .and. flag_stud>0)then

    func_lab =max(0.0_dp,  min(pt_lab,func_lab) )
    
else
    
    func_lab =max(0.0_dp,  min(1.0_dp,func_lab) )

endif

if (isnan(func_lab))then
    print*, "nan lab", jc,flag_stud,x,pheye(1,gc),net_wage
    pause
endif


end function func_lab
! ----------------------------------------------------------




! ----------------------------------------------------------
function func_lab_cpl(x,net_wage,gc,jc,tauc,ind_below)
! computes labor supply as function of x
! interior solution: x=rhs_foc
! corner solution: x=1.0
implicit none
real(dp),intent(in)::x,net_wage,tauc
integer,intent(in):: gc,jc,ind_below
real(dp)::func_lab_cpl
real(dp):: temp, avearn_loc

temp =  1.0_dp / ( tau_pr  + pssi(gc) )


!if (ind_below==1)then
!    avearn_loc = avearn_base
!else
    avearn_loc = avearn
!endif


func_lab_cpl = ( 1.0_dp/pheye(1,gc) * x / (1.0_dp + tauc ) * (1.0_dp - tau_pr) * (lambda_pr ) * net_wage**(1.0_dp-tau_pr)  ) ** temp


    
func_lab_cpl =max(0.0_dp,  min(1.0_dp,func_lab_cpl) )



end function func_lab_cpl
! ----------------------------------------------------------

! ----------------------------------------------------------
subroutine sub_corner_cons_lab(cons,sav,lab,coh,net_wage,wage_totax,gc,sc,jc,tc,numchld,flag_stud,tauc,ind_below)
! organizes corner solution for consumption and leisure in borrowing constraint region
use nrutil
use nr
implicit none
    
real(dp),intent(inout)::cons,lab
real(dp),intent(in):: coh,net_wage,sav,numchld,wage_totax,tauc
integer,intent(in):: gc,flag_stud,jc,tc,ind_below,sc

real(dp)::leis
real(dp)::cons_min,cons_max,fv_min,fv_max
real(dp),parameter::epsi=1.0e-06_dp
real(dp),parameter::tolf=1.0e-06_dp
    
! minimum consumption: some small parameter
! maximum consumption: coh - cons_min
cons_min = epsi * coh
cons_max = f_cons_constr(coh,sav,net_wage,wage_totax,2.0_dp,gc,sc,1,2,1,jc,tc,numchld,flag_stud,ind_below)
      
! check bounds:
fv_min = func_cons_corner(cons_min)
fv_max = func_cons_corner(cons_max)
if (fv_min*fv_max>0.0) then
	cons_min = epsi
    fv_min = func_cons_corner(cons_min)
	if (fv_min*fv_max>0.0) then
		!print*, 'no sign change: how can this be?'
        !print*, fv_min,fv_max,jc
        !pause
		cons = cons_min/2.0_dp
		leis = ( coh - cons ) / net_wage
		lab = 1.0_dp - leis 
		return
	endif
endif
cons = zbrent(func_cons_corner,cons_min,cons_max,tolf)
    
! given solution for consumption, compute labor
lab = func_lab_corner(cons)

if (isnan(lab))then
    print*, "nan lab constr", jc,flag_stud,cons,coh,sav,net_wage
    pause
endif

contains


    ! ----------------------------------------------------------
    function func_cons_corner(cons)
    ! distance function for optimal consumption in corner solution

    implicit none
    real(dp)::func_cons_corner
    real(dp),intent(in)::cons

    real(dp)::lab

    ! labor in corner solution for given consumption
    lab = func_lab_corner(cons)

    ! distance function
    !func_cons_corner = coh - net_wage * (1.0_dp - lab) - cons
    func_cons_corner =(f_cons_constr(coh,sav,net_wage,wage_totax,lab,gc,sc,1,2,1,jc,tc,numchld,flag_stud,ind_below))/(1.0_dp+tauc) - cons

    end function func_cons_corner 
    ! ----------------------------------------------------------
     
    
    ! ----------------------------------------------------------
    function func_lab_corner(cons)
    ! computes labor supply in corner solution 
    implicit none 
    real(dp),intent(in)::cons
    real(dp)::func_lab_corner

    real(dp)::lab,temp

    ! call function for interior solution, passing input 1.0:
    lab = func_lab(1.0_dp,net_wage,gc,jc,flag_stud,tauc,ind_below)

    ! append consumption term:
    if (flag_stud>0)then
        temp = - 1.0_dp / ( pssi(gc) + tau_pr  )
    else
        temp = - 1.0_dp / ( pssi(gc) + tau_pr  )
    endif
     
    func_lab_corner = lab * cons**temp
    
    if (jc==js .and. flag_stud>0)then
        
        func_lab_corner = max(0.0_dp, min(pt_lab,func_lab_corner) )
         
    else
        
        func_lab_corner = max(0.0_dp, min(1.0_dp,func_lab_corner) )
        
    endif
    
 
    end function func_lab_corner
    ! ----------------------------------------------------------
 
    
end subroutine sub_corner_cons_lab 
! ----------------------------------------------------------


! ----------------------------------------------------------
subroutine sub_corner_cons_lab_cpl(cons,sav,lab1,lab2,coh,net_wage,wage_totax,jc,tc,numchld,tauc,ind_below,gcmin,gcmax)
! organizes corner solution for consumption and leisure in borrowi2 constraint region
use nrutil
use nr
implicit none
    
real(dp),intent(out)::cons,lab1,lab2
real(dp),intent(in):: coh,net_wage(:),sav,numchld,wage_totax(:),tauc
integer,intent(in):: jc,tc,ind_below,gcmin,gcmax

real(dp)::leis1,leis2,lab(2)
real(dp)::cons_min,cons_max,fv_min,fv_max
real(dp),parameter::epsi=1.0e-06_dp
real(dp),parameter::tolf=1.0e-06_dp
    
! minimum consumption: some small parameter
! maximum consumption: coh - cons_min
cons_min = epsi * coh
cons_max = f_cons_constr_cpl(coh,sav,net_wage,wage_totax,2.0_dp,2.0_dp,1,2,1,jc,tc,numchld,ind_below)
      
! check bounds:
fv_min = func_cons_corner(cons_min)
fv_max = func_cons_corner(cons_max)
if (fv_min*fv_max>0.0) then
	cons_min = epsi
    fv_min = func_cons_corner(cons_min)
	if (fv_min*fv_max>0.0) then
		!print*, 'no sign cha2e: how can this be?'
        !print*, fv_min,fv_max,jc
        !pause
		cons = cons_min/2.0_dp
		leis1 = ( coh - cons ) / net_wage(1)
		lab1 = 1.0_dp - leis1
        leis2 = ( coh - cons ) / net_wage(2)
		lab2 = 1.0_dp - leis2
		!return
	endif
endif
cons =max(epsi, zbrent(func_cons_corner,cons_min,cons_max,tolf) )
    
! given solution for consumption, compute labor
lab = func_lab_corner(cons)
lab1 =lab(1)
lab2 =lab(2) !0.2_dp  ! max(0.2_dp,lab(2))

contains


    ! ----------------------------------------------------------
    function func_cons_corner(cons)
    ! distance function for optimal consumption in corner solution

    implicit none
    real(dp)::func_cons_corner
    real(dp),intent(in)::cons

    integer:: gc
    ! labor in corner solution for given consumption
    lab =func_lab_corner(cons) 
    !if (gcmin==gcmax)then
    !    if (gcmin==1)then
    !        lab(2)=0.1_dp*ft_lab
    !    else
    !        lab(1)=0.1_dp*ft_lab
    !    endif
    !endif
    
    !do gc = 1,2
    !    lab(gc) = max(0.0_dp,lab(gc))
    !enddo
    ! distance function
    !func_cons_corner = coh - net_wage * (1.0_dp - lab) - cons
    func_cons_corner =(f_cons_constr_cpl(coh,sav,net_wage,wage_totax,lab(1),lab(2),1,2,1,jc,tc,numchld,ind_below))/(1.0_dp+tauc) - cons

    end function func_cons_corner
    ! ----------------------------------------------------------
      
    
    ! ----------------------------------------------------------
    function func_lab_corner(cons)
    ! computes labor supply in corner solution 
    implicit none 
    real(dp),intent(in)::cons
    real(dp)::func_lab_corner(2)

    real(dp)::temp
    integer:: gc
    lab(:)=0.0_dp
    func_lab_corner = 0.0_dp !0.1_dp * ft_lab
    do gc=gcmin,gcmax
        ! call function for interior solution, passi2 input 1.0:
        lab(gc) = max(0.0_dp,func_lab_cpl(1.0_dp,net_wage(gc),gc,jc,tauc,ind_below) )

        ! append consumption term:
        temp = - 1.0_dp / ( pssi(gc) + tau_pr  )
    
     
        func_lab_corner(gc) = lab(gc) !* cons**temp
    
        func_lab_corner(gc) = max(0.0_dp, min(1.0_dp,func_lab_corner(gc)) )
        !func_lab_corner(gc) = max(0.0_dp,func_lab_corner(gc)) 

    enddo
    
    end function func_lab_corner
    ! ----------------------------------------------------------
 
    
end subroutine sub_corner_cons_lab_cpl 
! ----------------------------------------------------------


end module solvehhdlabpar_mod 

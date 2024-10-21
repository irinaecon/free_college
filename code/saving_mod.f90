module saving_mod
    
    use nrtype
    use params_mod
    use types_mod
    use funcs_mod,only: func_intp,func_intp_mod,makegrid
    use inst_mod,only: fun_cev
    use dc_egm_mod
   
    implicit none
    
    contains
    

    
! ------------------------------------------------------------------- 
subroutine sub_welfare(agg,demo,grid,pol,stat,lc,t0,t1,scen_sav,opt_rfg)  
! saving of output
use esplot
implicit none
type(t_agg),intent(inout)::agg 
type(t_demo),intent(inout)::demo
type(t_grid),intent(inout)::grid
type(t_pol),intent(inout)::pol
type(t_lc),intent(inout)::lc
type(t_stat),intent(inout)::stat
integer,intent(in)::t0,t1,scen_sav,opt_rfg
real(dp)::gr_rho_p(nt),cev_loc,afcev,v1_int,welf_aggdist_phi,coh,prodret,ass,v2_int
real(dp):: coh_glb_min,coh_glb_max,coh_grid(nx),v1_int_ds(nd,ns),v2_int_ds(nd,ns),logsum,logsum_base,prob_kid(ns),prob_kid_base(ns),welf_agg
integer::inds(2)
real(dp)::vals(2)
integer::xc,pc,yc,ec,ic,jc,kc,sc,tc,tcc,tccc,dc,gc,scc,icc,jcc,jc_temp,tc_temp,sc1,sc2,yc2,kc1,kc2,zc,gckid,ickid,hc
character(1)::i_char
real(dp),parameter::epsi=1.0e-06

    
end subroutine sub_welfare
! ------------------------------------------------------------------- 

end module saving_mod
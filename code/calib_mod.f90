module calib_mod
    
    ! this module organizes exogenous part of calibration
    
    use nrtype
    use params_mod
    use types_mod
    use rlse_int
    
	implicit none
    
contains
    
    ! ---------------------------------------------------------------------------------------------------------
    subroutine sub_calib(agg,demo,grid,lc,pol,pol_st,stat)
    ! organizes calibration
    type(t_agg),intent(inout)::agg
    type(t_demo),intent(inout)::demo
    type(t_grid),intent(inout)::grid
    type(t_lc),intent(inout)::lc
    type(t_pol),intent(inout)::pol
    type(t_pol_st),intent(inout)::pol_st
    type(t_stat),intent(inout)::stat
   
    character(:) ,allocatable ::calib_name
    logical::exit_main_loop
    
    ! get exogenous calibration parameters
    call get_calibration_name(calib_name, exit_main_loop)
    call ReadCalibration(trim(adjustl(calib_name)))
   
    ! determine number of discrete choices available
    call sub_nd(nd)
    call sub_nd_cpl(nd_cpl)
        
    ! compute derived calibration parameters
    ! compute age bounds
    call sub_agetime(jsp,njp,jrp,t1p,t2p,t1gp,t2gp,tbp)
        
    ! compute rhhos in aggregate production function
    call sub_rhho(se_kl,se_skills)
    
    ! allocation of all objects:    
    call sub_alloc(agg,demo,grid,lc,pol,pol_st,stat) 
    
    ! set up tax rates
    call sub_socins(demo,agg)
    
          
    ! setup HK grids and investment grids
    call sub_hkgrids(grid)
    
    ! ability gradient
    call sub_abgrad(agg)
        
    ! input of initial distribution
    call distr_input(demo,grid)
  
    ! determine discrete labor choices
    call sub_gridlab(grid,demo)

  
    ! get age productivity profiles
    call sub_demo(demo,njp)            
    
     ! calibration of income process
    call sub_inc_proc(demo)        
    
    ! TBC: calibration of productivity
    demo%frac_prod(:,:)=nk**(-1.0_dp) !1.0_dp              
    
    ! technology level
    call sub_tech(1.0_dp,demo%lamt,agg%At)
        
    call sub_techrate(demo)
    
    call sub_poprate(demo)
    

    end subroutine sub_calib
    ! ---------------------------------------------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------------------------------------------
    subroutine get_calibration_name(calib_name_out, exit_main_loop)
        character(:), allocatable, intent(out) :: calib_name_out
        logical, intent(out) :: exit_main_loop
        character(len=100) :: calib_name
        integer, save :: this_calibration_line = 1
        integer :: line, error_stat

        exit_main_loop = .false.

        open(unit=21, file='input/calibr/select_calibration_here.txt', status='OLD', action='READ', iostat=error_stat)

openif: if (error_stat == 0) then

	        ! fast forward to line before the current line
	        do line = 1, this_calibration_line - 1
	            read(21,'(a)')
	        enddo

	        ! read next calibration
stupid:     do ! this stupid do-loop is only here to allow for comments (preceded by an exclamation mark)
	            read(21,'(a)',iostat=error_stat) calib_name
	            if (error_stat==0) then
	               this_calibration_line = this_calibration_line + 1
		           if (index(calib_name,'!!!')>0) then
		               exit_main_loop = .true.
		           elseif (index(calib_name,'!')>0 .or. len_trim(calib_name)== 0) then
		              cycle
	               endif
		        else
		           print '(a,i6)', '- main: ERROR reading calibration name in line ', line
		           exit_main_loop = .true.
	            endif

	            exit stupid

            enddo stupid

	    else openif
	        print '(a,i6)', '- main: ERROR opening select_calibration_here.txt ,IOSTAT=',error_stat
	        exit_main_loop = .true.
	    endif openif

	    close(unit=21)

	    if (error_stat > 0) then
		    open (unit=21, file='output/CRITICAL_ERROR.txt', status = 'replace')
	        write(21,'(a)') 'A critical error occurred in main while reading a calibration file.'
	        write(21,'(a)') 'Delete this file after reading to avoid confusion.'
	        close(21)
        endif

        calib_name_out = trim(adjustl(calib_name))

    end subroutine get_calibration_name
    ! ---------------------------------------------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------------------------------------------
    subroutine sub_demo(demo,njp)
    
    use esplot
    implicit none
    
    integer,intent(in)::njp
    type(t_demo),intent(inout)::demo   
    real(dp),dimension(6,8):: dat_ap_new       ! data file with age productivity profiles 
    real(dp),dimension(2,nt)::dat_ass_fac       ! assimilation factor
    integer::sc,ic,tc,jc,gc,oc,tcp1,jcj
    character(1)::i_char,i_char_ic,i_char_gc
    character(5):: pil_char,pir_char,pih_char
    logical,parameter::opt_check=.false.
    real(dp):: pop_g(2,nj,nt),ass_fac,assrow_fac,dist_proj,dat_prodfun(3,1),ap_lev_annual(2,njp+1,ns),temp,base_fr
    real(dp),parameter::epsi=1.0e-08
    real(dp):: dat_ap_lev_annual(9,65-20+1),ap_annual(2,njp+1,ns),dat_ap(9,(65-20+1)/delta_j),usmr(111),surv(111)
    real(dp):: frac_j(nj)
    open(99,file='input/usmr.txt')
    read(99,*) usmr
    close(99)
    surv =1.0_dp -  usmr
    demo%sr = 1.0_dp
    jcj=(jt+1)*delta_j-2
    do jc=jt+1,nj
        demo%sr(jc,1)=sum(surv(jcj:jcj+delta_j-1))/delta_j     ! model     
        ! surv(jc)=survpop(jcc)     ! model     
        jcj=jcj+delta_j
        demo%sr(jc,:) = demo%sr(jc,1)
    end do
    
    !demo%sr(3:nj-1,1)=1.0-usmr(1:23)    ! survival to next period
                                   
    !print*, demo%sr(2:nj-1,1)
    !pause
    
    ! adjustment factor: total to adult population
    !totpopfac=sum(survpop(1:njpd))/sum(survpop(ja0:njpd))       ! required to compute pcout
    
    ! ******************************************************************************
    ! ******* INPUT of productivity profiles ****** !
    ! ******************************************************************************
  
    if (opt_test_flap)then
        demo%ageprod_dat(:,:,:) = lvl_ref
        
    else
    
        ! read yearly level profiles
        open(unit=19, file='input/ageprofiles4.txt', status='OLD', action='READ')   
        read(19,*) dat_ap_lev_annual
    
        ic=2
        do sc=ns,1,-1
            do gc=1,2
                ap_lev_annual(gc,20:65,sc) = dat_ap_lev_annual(ic,:)
            
                ! for ages before and after
                ap_lev_annual(gc,1:20,sc) = ap_lev_annual(gc,20,sc)
                ap_lev_annual(gc,66:njp+1,sc) = ap_lev_annual(gc,65,sc)
                !print*, ap_lev_annual(gc,:,sc)
                !pause
                ic=ic+1
            enddo
        enddo
        
        !ap_lev_annual(:,:,3) =   ap_lev_annual(:,:,2) 
     
        ap_annual = ap_lev_annual
    
        ! go to delta_j frequency
        do jc=1,nj 
            if (jc<=js)then
                jcj = js*5
            
            elseif (jc>=16 ) then
                
                jcj = 16* delta_j +2
            else
                jcj =jc* delta_j +2 !,delta_j * (jc-1)+1 + delta_j-1    
                
            endif
            
            !print*, jcj
            do sc=1,ns
                do gc=1,2
            
            
                    demo%ageprod_dat(gc,jc,sc) = sum(ap_annual(2,jcj:jcj+delta_j-1,sc) ) / delta_j
                enddo
               
            enddo
          
        enddo

        demo%ageprod_dat(:,:,1) = demo%ageprod_dat(:,:,2) * 0.7_dp

        temp = demo%ageprod_dat(1,1,1)
        demo%ageprod_dat(:,:,:) = demo%ageprod_dat(:,:,:) / temp 

      
        
    endif   
   
    demo%ageprod(1,:,:) = demo%ageprod_dat(1,:,:)
    demo%ageprod(2,:,:) = 0.8_dp *demo%ageprod_dat(1,:,:)
    
    !demo%ageprod(1,:,2) = demo%ageprod(1,:,3) * 0.8_dp
    !demo%ageprod(1,:,1) = demo%ageprod(1,:,3) * 0.6_dp
    
    !do sc=1,ns
    !    demo%ageprod(1,jr0-1,sc) = demo%ageprod(1,jr0-2,sc)
    !enddo

    !print*, "ageprod1", demo%ageprod(1,:,1)
    !print*, "ageprod2", demo%ageprod(1,:,2)
    !pause
    
    ! time specific retirement age
    call sub_retage(demo,jr,jr0)
     
    print*, "av prod 1", sum(demo%ageprod(1,js+1:jr(1)-1,1))/(jr(1)-(js+1) - 1)
    print*, "av prod 2", sum(demo%ageprod(1,js+1:jr(1)-1,2))/(jr(1)-(js+1) - 1)
    print*, "av prod 3", sum(demo%ageprod(1,js+1:jr(1)-1,3))/(jr(1)-(js+1) - 1) 
    print*, "av prod 4", sum(demo%ageprod(1,js+1:jr(1)-1,4))/(jr(1)-(js+1) - 1)

    !pause
    
   ! frac_jt
    
    ! compute implied fertility
    frac_j = 0.0_dp
    frac_j(js) = 1.0_dp
    do jc=js+1,nj
        frac_j(jc) = frac_j(jc-1) / (popgr + 1.0_dp) * demo%sr(jc-1,1)
    enddo
    
    !frac_j = frac_j / sum(frac_j)
    
    print*, "fracj", frac_j
    
    demo%frac_jt(:,1) = frac_j
    
    ! initialize for all other periods
    do tc = 2,nt
        demo%frac_jt(:,tc) = frac_j
    enddo
    
    demo%nkids_input(:,:) = frac_j(js) / frac_j(jt)
    
    base_fr =demo%nkids_input(2,1)/( clfrac_str + fr_grad * (1.0_dp - clfrac_str) )
    base_fr = base_fr / (0.5_dp / 0.75_dp)
    
    demo%numchild_gsj(:,:,:,:) = 0.0_dp
    do tc=1,nt
        do jc = jf,jt
            do sc = 1,ns
                if (sc<ns)then
                    demo%numchild_gsj(2,sc,jc,tc) =base_fr*fr_grad
                    demo%nkids_input(2,sc) = base_fr*fr_grad
                else
                    demo%numchild_gsj(2,sc,jc,tc) =base_fr*1.0_dp
                    demo%nkids_input(2,sc) = base_fr*1.0_dp
                endif
                
            enddo
            
        enddo
    enddo
    
    print*, "nkid", demo%nkids_input(2,:)
    print*, "popgr",(0.5_dp / 0.75_dp) *( clfrac_str * demo%nkids_input(2,ns) +  (1.0_dp - clfrac_str) * demo%nkids_input(2,ns-1) ),((0.5_dp / 0.75_dp) *( (clfrac_str * demo%nkids_input(2,ns) +  (1.0_dp - clfrac_str) * demo%nkids_input(2,ns-1))))**(1.0_dp/(jt-js)) - 1.0_dp
    !pause
    
    
    
    end subroutine sub_demo
    ! ---------------------------------------------------------------------------------------------------------
   
    ! ---------------------------------------------------------------------------------------------------------
    subroutine sub_nd(nd)
    
    implicit none
    integer,intent(out):: nd
    
    nd = ndl + 1 ! non zero labor states + usr states
   
    end subroutine sub_nd
    ! ---------------------------------------------------------------------------------------------------------
    
    ! ---------------------------------------------------------------------------------------------------------
    subroutine sub_nd_cpl(nd_cpl)
    
    implicit none
    integer,intent(out):: nd_cpl
    
    nd_cpl = ndl + 1 ! non zero labor states + usr states 
    
    end subroutine sub_nd_cpl
    ! ---------------------------------------------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------------------------------------------
    subroutine sub_gridlab(grid,demo)
    
    type(t_grid),intent(inout):: grid
    type(t_demo),intent(inout):: demo
    real(dp),dimension(6):: gridfull
    integer:: ic,sc,gc
    real(dp):: ftt,ptt, meanlab,meanlab_ld,deltalab,meantemp
    
    ft_lab = ft_lab * biggam
    ft_lab = ft_lab * biggam
    
    if (ndl==1)then
               
        do sc=1,ns
            grid%lab(1,1:ndl,sc) = (/ft_lab * 1.831730769_dp /) ! (/ft_lab * 0.898076923_dp /)
            grid%lab(2,1:ndl,sc) = (/ft_lab * 1.831730769_dp /)
        enddo
      
        
        grid%lab_ge = ft_lab
        
    elseif (ndl==2)then
        
        do sc=1,ns
            grid%lab(1,1:ndl,sc) = (/ft_lab * 1.831730769_dp, pt_lab * 1.831730769_dp /) ! (/ft_lab * 0.898076923_dp /)
            grid%lab(2,1:ndl,sc) = (/ft_lab * 1.831730769_dp, ft_lab * 1.831730769_dp /)
        enddo
       
        grid%lab_ge = ft_lab
        
    else
        
        print*, "in this version only EXO lab supply available!"
        pause
    endif
    
    
    end subroutine sub_gridlab
    ! ---------------------------------------------------------------------------------------------------------    
    
    
    
    ! ----------------------------------------------------------------------------------------------------------------
    subroutine sub_socins(demo,agg)
    ! read in parameters of sosical insurance systems: algII, pension, health
    
    implicit none
    type(t_demo),intent(inout)::demo
    type(t_agg),intent(inout)::agg
    real(dp)::dat_gyr(1,49)
    integer:: tc,tc_max
    
    tc_max=1
   
    do tc=1,nt !tr-1
        agg%tau_p(tc) = tau_p  !taupparam
    enddo
   
    ! ---------------------------------
    ! capial income tax
    do tc=1,nt
        agg%tau_k(tc) = tau_k
    enddo
    
    ! consumption tax in steady state
    do tc=1,nt
        agg%tau_c(tc) = tau_c
    enddo
   
    end subroutine sub_socins
    ! ----------------------------------------------------------------------------------------------------------------


     
    ! --------------------------------------------------------------------------------- -------------------------------
    subroutine sub_inc_proc(demo)
    ! realizations and probabilities of income processes 
    implicit none
    type(t_demo),intent(inout)::demo
    real(dp):: rhoy,vary,vary_uncond,sigy,varw
    real(dp):: deltap,meany1,meany2, meany,delta_y2,meany1_ld,meany2_ld,deltapini,frac_CL,frac_nCL,y_CL,y_nCL,delta_y_asym,delta_y2_asym,deltap_asym,deltapini_asym
    real(dp),parameter:: delta_y  = 0.0229_dp * xprod! reduction of income (biannial)
    integer::jc,sc,yc
    real(dp):: frac_s(ns),y_s(ns),mean_inc_high,mean_inc_high_j(nj),y_sj(ns,nj),meany_test,deltap_s(ns),meany_j(nj),meany2_test,meany1_test,meany2_j(nj),meany1_j(nj)
    
    
    if (ny==2) then
        
      
        demo%pini(:)=ny**(-1.0_dp)
      
        rhoy = 0.9510_dp
        vary = 0.0109_dp
        varw = 0.0637_dp
        
        vary_uncond = vary / (1.0_dp - rhoy**2.0_dp)
        vary=(1.0+rhoy**2+rhoy**4+rhoy**6)*vary + varw !(1.0+rhoy**2)*vary
        
        rhoy=rhoy**delta_j
        rhoy = rhoy / (1.0_dp + varw / vary_uncond)
        
        !call discretize_AR(rhoy, 0.0_dp, vary, demo%grid_y, demo%prob_y, demo%pini)
        !print*, "row", demo%grid_y, demo%prob_y
        !pause
   
        ! transition probability 
	    demo%prob_y(:,:)=transm(rhoy)   
    
        
        if (nw==2)then        
            demo%prob_epsi = 0.5_dp
            demo%grid_epsi(1) = 1.0_dp - 0.5_dp * sqrt(0.0566_dp)
            demo%grid_epsi(2) = 1.0_dp + 0.5_dp * sqrt(0.0566_dp)
        else
            demo%prob_epsi = 1.0_dp
            demo%grid_epsi(1) =1.0_dp
        endif
        

        !print*, "demo espi", demo%grid_epsi
        !pause
        
	    ! States
        sigy=sqrt(vary)
	    demo%grid_y(1)=exp(1.0-sigy) 
	    demo%grid_y(2)=exp(1.0+sigy)
	    demo%grid_y=2.0*demo%grid_y/(sum(demo%grid_y))
        
        
        !if (opt_asymm_shk==1)then
        !
        !    ! LHS skilled
        !    demo%prob_ld(2,1,1) = 
        !    
        !    ! HS skilled
        !    demo%prob_ld(2,1,1) = 
        !    
        !    ! CL skilled
        !    demo%prob_ld(2,1,1) = 
        !endif
        
        
               
        demo%prob_y_dat = demo%prob_y
        demo%pini_dat = demo%pini 
        
        !print*, "gridy", demo%grid_y
        !print*, demo%prob_y(:,:)
        !pause
        
    elseif (ny==1) then
        demo%grid_y(1)=1.0_dp
        demo%prob_y(1,1)=1.0_dp
        demo%pini(1)=1.0_dp
        demo%prob_y_dat = demo%prob_y
        demo%pini_dat = demo%pini 
    else
        print*, 'no such case'
        return
    endif
    
    
    contains
    
    
        ! --------------------------------------------------------------------------------------
        ! 2-state Markov process
        function transm(rho) 

        implicit none
        real(dp)::transm(2,2),rho

        transm=(1.0+rho)/2.0
        transm(1,2)=1.0-transm(2,2)
        transm(2,1)=1.0-transm(1,1)

        end function transm
        ! --------------------------------------------------------------------------------------
        
        
        ! -------------------------------------------------------------------------------------
        function mean_inc_edu(frac_edu,inc_edu,prob_y) 
        ! average income across education groups
        
        implicit none
        real(dp)::mean_inc_edu(nj)
        real(dp),intent(in):: frac_edu(:),prob_y(:),inc_edu(:,:)
        integer:: yc,sc
        real(dp):: mean_inc_j(nj)
        
        mean_inc_edu = 0.0_dp
        mean_inc_j = 0.0_dp

        do jc=js,jr(1)
            do sc=1,ns
                do yc=1,ny
                    mean_inc_j(jc) = mean_inc_j(jc) + frac_edu(sc) * prob_y(yc) * inc_edu(sc,jc) * demo%grid_y(yc)             
                enddo
            enddo
        enddo
        
        
        mean_inc_edu = mean_inc_j
        

        end function mean_inc_edu
        ! --------------------------------------------------------------------------------------
        
        
        ! -------------------------------------------------------------------------------------
        function mean_inc_state(frac_edu,inc_edu,gridy) 
        ! average income across education groups
        
        implicit none
        real(dp)::mean_inc_state(nj)
        real(dp),intent(in):: frac_edu(:),gridy,inc_edu(:,:)
        integer:: yc,sc
        real(dp):: mean_inc_j(nj)
        
        mean_inc_state = 0.0_dp
        mean_inc_j = 0.0_dp

        do jc=js,jr(1)
            do sc=1,ns
                mean_inc_j(jc) = mean_inc_j(jc) + frac_edu(sc) * inc_edu(sc,jc) * gridy             
            enddo
        enddo
        
        mean_inc_state = (mean_inc_j) 
        

        end function mean_inc_state
        ! --------------------------------------------------------------------------------------
        
        
        
    end subroutine sub_inc_proc
    ! ----------------------------------------------------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------------------------------------------------
    subroutine sub_rhho(se_kl,se_s)
    ! computes parameters used in aggregate production function from s.e.
    implicit none
    real(dp),intent(in)::se_kl,se_s

    rhho_s=1.0_dp-1.0_dp/se_s
   
    rhho_kl = 1.0_dp - 1.0_dp/se_kl
    
    end subroutine sub_rhho
    ! ----------------------------------------------------------------------------------------------------------------
    
    
    ! ----------------------------------------------------------------------------------------------------------------
    subroutine sub_agetime(jsp,njp,jrp,t1p,t2p,t1gp,t2gp,tbp)
    ! translates age bounds from population data to model ages
    implicit none
    integer,intent(in)::jsp,njp,t1p,t2p,jrp,t1gp,t2gp,tbp
    
    nj=njp+1 ! 100 , biol
    nj = (nj -2) /delta_j    
    
    js=jsp+1-2 ! 16 
    js=js/delta_j
    
    jr0=jrp+1-2 ! 66 
    jr0 = jr0/delta_j
    
    jf = (jfp+1-2)/delta_j
    jt = (jtp+1-2)/delta_j
    
    jstud(1)=0
    jstud(2)=0
    jstud(3)=(jhsp+1-2)/delta_j
    jstud(4)=(jclp+1-2)/delta_j
    
    ! total number of simulation periods
    if (opt_test_laptop==1)then
        nt=2 !47 !2 !7 !27 !55 !(t2p-t1p+1)/delta_j
    else
        nt=47
    endif
    
    ! first and last year of gini computation 
    t1g=1!t1gp-t1p+1
    t2g=1 !t2gp-t1p+1
     
    ! year of welfare evaluation (=storing of utility function)
    tb=1 !tbp-t1p+1
    
    ! calibraiton
    tclb =1 ! tclb1 - t1p+1
    
   
    end subroutine sub_agetime 
    ! ----------------------------------------------------------------------------------------------------------------
    
    
    ! ----------------------------------------------------------------------------------------------------------------
    subroutine distr_input(demo,grid)
    
    use funcs_mod,only: makegrid
    
    implicit none
    type(t_demo),intent(inout):: demo
    type(t_grid),intent(inout):: grid
    real(dp):: dat_marstatus(1,2),dat_education(2,4),dat_nkids(4,2),dat_assets(5,6),av_prod,dat_h0prop(4,2),dat_moninv(17,6),dat_tinv(17,6)
    integer:: sc,qc,mc,ic,hc,tc,jc,gc,jc_temp
    real(dp):: time_sng,mon_sng,time_mrg,mon_mrg,time_gsj_temp(2,4,17),mon_gsj_temp(2,4,17),time_gsj(2,4,4),mon_gsj(2,4,4),time_gs(2,4),mon_gs(2,4),time_s(4),mon_s(4)
    real(dp):: time_j_sng(4),mon_j_sng(4),time_j_mrg(4),mon_j_mrg(4),time_j(4),mon_j(4),mon_j_lin(4),tinv_slope(3),moninv_slope(3)
    
    open(unit=99, file='input/marstatus_input.txt', status='OLD', action='READ')   
    read(99,*) dat_marstatus
    
    open(unit=98, file='input/education_input.txt', status='OLD', action='READ')   
    read(98,*) dat_education
    
    open(unit=97, file='input/nkids_input.txt', status='OLD', action='READ')   
    read(97,*) dat_nkids
    
    open(unit=96, file='input/assets_input.txt', status='OLD', action='READ')   
    read(96,*) dat_assets
    
    open(unit=95, file='input/h0prop_input.txt', status='OLD', action='READ')   
    read(95,*) dat_h0prop
    
    open(unit=94, file='input/money_input.txt', status='OLD', action='READ')   
    read(94,*) dat_moninv
    
    open(unit=93, file='input/time_input.txt', status='OLD', action='READ')   
    read(93,*) dat_tinv
    
    close(99)
    close(98)
    close(97)
    close(96)
    close(95)
    close(94)
    close(93)
    
    demo%mc_input(1) = dat_marstatus(1,1) ! single
    demo%mc_input(2) =1.0_dp -dat_marstatus(1,1) ! dat_marstatus(1,2) ! married
    
    ! education single, married
    do sc=1,ns !-1 !-1
        ! single
        demo%sc_input(1,sc) =dat_education(1,sc)
        !if (sc==ns-1) demo%sc_input(1,ns-1) = 1.0_dp - sum(demo%sc_input(1,1:ns-2))
        ! married
        demo%sc_input(2,sc) =dat_education(2,sc)
        !if (sc==ns-1) demo%sc_input(2,ns-1) = 1.0_dp - sum(demo%sc_input(2,1:ns-2))
    enddo
    
    do sc=1,ns !-1
        ! single
        demo%nkids_input(1,sc) = dat_nkids(sc,1)
        do qc=1,5
            demo%ass_input_dat(1,sc,qc) = dat_assets(qc,sc)
        enddo
                
        ! married
        demo%nkids_input(2,sc) = dat_nkids(sc,2)
        
    enddo
    
    do gc=1,2
        do sc=1,ns
            demo%h0prop(gc,sc) = dat_h0prop(sc,gc)
        enddo
    enddo
    
    !print*, "lo", demo%h0prop(:,1)
    !print*, "me", demo%h0prop(:,2)
    !print*, "hi", demo%h0prop(:,3)
    !
    !pause
    
    
    do sc=1,ns !-1
        av_prod = 0.0_dp
        do hc=1,np
            av_prod = av_prod + ab_grad(sc) * (grid%hk_grid(hc) )
        enddo
        av_prod = av_prod / np
        
        norm_param(sc) = 1.0_dp / av_prod
        
    enddo
        
    !demo%numchild_gsj(:,:,:,:) = 0.0_dp
    !do tc=1,nt
    !    do jc = jf,jt
    !        demo%numchild_gsj(2,:,jc,tc) =1.0_dp !demo%nkids_input(:,:)     
    !    enddo
    !enddo
    
    time_sng = 0.0_dp
    time_mrg = 0.0_dp 
    mon_sng = 0.0_dp
    mon_mrg = 0.0_dp  
    
    time_j_sng  = 0.0_dp
    time_j_mrg  = 0.0_dp
    mon_j_sng  = 0.0_dp
    mon_j_mrg  = 0.0_dp
    
    do sc=1,ns !-1
        
        ! single
        time_gsj_temp(1,sc,:) =dat_tinv(:,min(sc,4))
        mon_gsj_temp(1,sc,:) =dat_moninv(:,min(sc,4))
        do jc=1,4
            if (jc==1)then
                time_gsj(1,sc,jc) =sum(time_gsj_temp(1,sc,jc+1:jc+4) ) /delta_j
                mon_gsj(1,sc,jc) =sum(mon_gsj_temp(1,sc,jc+1:jc+4) ) /delta_j
            else
                jc_temp = jc*delta_j -2
                time_gsj(1,sc,jc) =sum(time_gsj_temp(1,sc,jc_temp:jc_temp+3) ) /delta_j
                mon_gsj(1,sc,jc) =sum(mon_gsj_temp(1,sc,jc_temp:jc_temp+3) ) /delta_j
            endif
        enddo
        time_gs(1,sc) =sum(time_gsj(1,sc,:) ) /4 ! 8
        mon_gs(1,sc) =sum(mon_gsj(1,sc,:) ) /4 ! 8
        
        time_sng = time_sng +  time_gs(1,sc) *ns**(-1.0_dp)
        mon_sng = mon_sng +  mon_gs(1,sc)*ns**(-1.0_dp)
        
        do jc=1,4 !8
            time_j_sng(jc) = time_j_sng(jc) +  time_gsj(1,sc,jc)*ns**(-1.0_dp)
            mon_j_sng(jc) = mon_j_sng(jc) +  mon_gsj(1,sc,jc)*ns**(-1.0_dp)
        enddo
        
        
        ! married 
        time_gsj_temp(2,sc,:) =dat_tinv(:,sc-1+ns-1)
        mon_gsj_temp(2,sc,:) =dat_moninv(:,sc-1+ns-1)
        do jc=1,4
            if (jc==1)then
                time_gsj(2,sc,jc) =sum(time_gsj_temp(2,sc,jc:jc+3) ) /delta_j
                mon_gsj(2,sc,jc) =sum(mon_gsj_temp(2,sc,jc:jc+3) ) /delta_j
            else
                jc_temp = jc*delta_j -2
                time_gsj(2,sc,jc) =sum(time_gsj_temp(2,sc,jc_temp:jc_temp+3) ) /delta_j
                mon_gsj(2,sc,jc) =sum(mon_gsj_temp(2,sc,jc_temp:jc_temp+3) ) /delta_j
            endif
        enddo
        time_gs(2,sc) =sum(time_gsj(2,sc,:) ) /4 ! 8
        mon_gs(2,sc) =sum(mon_gsj(2,sc,:) ) /4 ! 8
        
        time_mrg = time_mrg +  time_gs(2,sc) *ns**(-1.0_dp)
        mon_mrg = mon_mrg +  mon_gs(2,sc) *ns**(-1.0_dp)
        
        do jc=1,4 !8
            time_j_mrg(jc) = time_j_mrg(jc) +  time_gsj(2,sc,jc) *ns**(-1.0_dp)
            mon_j_mrg(jc) =mon_j_mrg(jc) +  mon_gsj(2,sc,jc) *ns**(-1.0_dp)
        enddo
                
    enddo
    
    tinv_str = time_sng * demo%mc_input(1) + time_mrg * demo%mc_input(2) 
    tinv_str = tinv_str / 60
    
    moninv_str = mon_sng * demo%mc_input(1) + mon_mrg * demo%mc_input(2) 
    moninv_str = moninv_str / earnref_data  
    demo%mc_input = 0.5_dp
    mon_j = 0.0_dp
    time_j = 0.0_dp
    do jc=1,4 !8 
        time_j(jc) = time_j_sng(jc) * demo%mc_input(1) + time_j_mrg(jc) * demo%mc_input(2) ! sum(time_gsj(:,:,jc)) /6  
        time_j(jc) = time_j(jc) / 60
    
        mon_j(jc) = mon_j_sng(jc) * demo%mc_input(1) + mon_j_mrg(jc) * demo%mc_input(2) ! sum(mon_gsj(:,:,jc)) /6 !
        mon_j(jc) = mon_j(jc) / earnref_data
    enddo
    
    
    t1_str = time_j(1)
    
    do jc=1,3 !7
        tinv_slope(jc) =( time_j(jc+1)-time_j(jc)) / (jc+1 - jc)
        moninv_slope(jc) =( mon_j(jc+1)-mon_j(jc)) / (jc+1 - jc)
    enddo
    tinv_slope_str =time_j(1)/time_j(4) ! sum(tinv_slope ) / 5 !time_j(1)/time_j(7) ! sum(tinv_slope ) / 7
    moninv_slope_str =mon_j(1)/mon_j(4)  !sum(moninv_slope ) / 5 ! mon_j(1)/mon_j(7) 
    
    call sub_reg_dat(time_j,demo%betta0_dat(1),demo%betta1_dat(1),demo%betta2_dat(1))
    
    mon_j_lin = makegrid(moninv_str,moninv_str,4,1.0_dp)
    call sub_reg_dat(mon_j_lin,demo%metta0_dat(1),demo%metta1_dat(1),demo%metta2_dat(1))
    !call sub_reg_dat_lin(time_j(1:6),demo%betta0_dat(1),demo%betta1_dat(1),demo%betta2_dat(1))
    !demo%metta1_dat(1) = 1.0_dp
    !demo%metta2_dat(1) = 1.0_dp

    
    !print*,"all", mon_j
    !print*,"mrg",  mon_gsj_temp(2,2,:)
    !print*,"sng",  mon_j_sng
    !!print*, "slope", moninv_slope, moninv_slope_str 
    !!pause
    
    !print*,tinv_slope_str 
    !pause
    demo%mc_input = 0.5_dp
    do sc=1,ns !-1
        time_s(sc) = time_gs(1,sc) * demo%mc_input(1) +time_gs(2,sc) * demo%mc_input(2) 
        mon_s(sc) = mon_gs(1,sc) * demo%mc_input(1) +mon_gs(2,sc) * demo%mc_input(2) 
        time_s(sc) = time_s(sc) / 60
        mon_s(sc) = mon_s(sc) / earnref_data
    enddo
    
    tgrad_str =( time_s(3) / time_s(1) + time_s(3) / time_s(1) ) * 0.5_dp
    mgrad_str = mon_s(3) / mon_s(1)
    
    !print*, tgrad_str , mgrad_str, tinv_str, moninv_str
    !
    !print*, "sng time",time_gsj_temp(1,1,:)
    !print*, "mrg time", time_gsj(2,1,:)
    
    !pause
    
    ! save average profiles
    open(unit=83,file='output/invprof_data.txt')
       
    do jc = 1,4
        write(83,'(2f50.16)') mon_j(jc), time_j(jc)  
        
    enddo
        
    close (83) 
    
    
    ! save averages by skills
    open(unit=85,file='output/inv_s_data.txt')
     
    do sc=1,ns !-1
        write(85,'(2f50.16)') mon_s(sc), time_s(sc)  
    enddo
    
    close (85)
    
    !pause
    
    
    end subroutine distr_input
    ! ----------------------------------------------------------------------------------------------------------------
    
    ! -------------------------------------------------------------------------------
    subroutine sub_reg_dat(vec,alpha0,alpha1,alpha2)


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
    
    !print*, y_reg,x_reg(:,2)
    !pause

    !print*,"y", y_reg(1:20)
    !print*,"x", x_reg(1:20,2)

    call d_rlse(y_reg,x_reg(:,1:3),b_reg(1:3),intcep=0)
    
    alpha0 = b_reg(1)
    alpha1 = b_reg(2)
    alpha2 = b_reg(3)

    end subroutine sub_reg_dat
    ! -------------------------------------------------------------------------------
    
    
    ! -------------------------------------------------------------------------------
    subroutine sub_reg_dat_lin(vec,alpha0,alpha1,alpha2)


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
    
    !print*, y_reg,x_reg(:,2)
    !pause

    !print*,"y", y_reg(1:20)
    !print*,"x", x_reg(1:20,2)

    call d_rlse(y_reg,x_reg(:,1:2),b_reg(1:2),intcep=0)
    
    alpha0 = b_reg(1)
    alpha1 = b_reg(2)
    alpha2 = 1.0_dp !b_reg(3)

    end subroutine sub_reg_dat_lin
    ! -------------------------------------------------------------------------------
    
    
    ! ----------------------------------------------------------------------------------------------------------------
    subroutine sub_abgrad(agg)
    
    implicit none
    type(t_agg),intent(inout):: agg 
    integer:: sc,hq
    real(dp):: dat_abgrad(2,4)
    
    open(unit=99, file='input/abgrad.txt', status='OLD', action='READ')   
    read(99,*) dat_abgrad
    
    do sc=1,ns
        do hq=1,2
            agg%abgrad_s(hq,sc) = dat_abgrad(hq,sc)
        enddo
    enddo
        
    
    end subroutine sub_abgrad
    ! ----------------------------------------------------------------------------------------------------------------
    
   
    ! ----------------------------------------------------------------------------------------------------------------
    subroutine sub_hkgrids(grid)
    
    use funcs_mod,only: makegrid
    use inst_mod,only: f_hh_tp1
    
    implicit none
    type(t_grid),intent(inout):: grid
    real(dp):: mean_tinv, mean_moninv 
    real(dp):: mon_min,mon_max,ti_min, ti_max
    
    mon_min = 0.0_dp
    mon_max = 0.40_dp
    
    ti_min = 0.0_dp * ft_lab
    ti_max = 2.5_dp * ft_lab
    
    mean_tinv =0.25_dp*ft_lab
    mean_moninv = 0.07_dp
    
    grid%hk_grid(1:np-1)=makegrid(min_h,max_h/3.0_dp,np-1,3.0_dp)
    grid%hk_grid(np) = max_h
    !            f_grid_sav(nx)=max_sav0
    !        else
    !            f_grid_sav(1:nx)=makegrid(min_sav0,max_sav0,nx,3.0_dp)
    !        endif
    
    ! acquired ability / human capital grid
    !grid%hk_grid(1:np-5) = makegrid(min_h,max_h,np,3.0_dp)
    !grid%hk_grid(np-5:np) = makegrid(max_h,max_h*3.0_dp,np-np+6,1.0_dp)

    !grid%hk_grid(np-4) = grid%hk_grid(np-5) * 1.2_dp
    !grid%hk_grid(np-3) = grid%hk_grid(np-5) * 1.5_dp
    !grid%hk_grid(np-2) = grid%hk_grid(np-5) * 1.9_dp
    !grid%hk_grid(np-1) = grid%hk_grid(np-5) * 2.2_dp
    !grid%hk_grid(np) = grid%hk_grid(np-5) * 2.5_dp


    ! innate ability / human capital
    grid%h0_grid(:) = makegrid(min_h0,max_h0,ni,1.0_dp)
    
    ! time investments: suppose 2-20 hours, 6b bins: 2,5,8,11,14,17 => 0.05, 0.125, 0.2, 0.275, 0.35, 0.45
    grid%tinv_grid = makegrid(ti_min,ti_max,ntinv,1.0_dp)
    !grid%tinv_grid = (/0.1_dp, 0.15_dp, 0.2_dp, 0.25_dp, 0.3_dp, 0.32_dp, 0.34_dp, 0.37_dp, 0.39_dp, 0.41_dp /)

    grid%minv_grid_dat = makegrid(mon_min,mon_max,nminv,1.0_dp)
    
    ab_grad = (/0.374_dp, 0.374_dp, 0.510_dp, 0.643_dp  /)
    
    
    end subroutine
    ! ----------------------------------------------------------------------------------------------------------------
    
    
    ! ----------------------------------------------------------------------------------------------------------------
    subroutine sub_retage(demo,jr,jr0)
    ! computes time vector of retirement ages
    use esplot
    implicit none
    
    type(t_demo),intent(in)::demo
    integer,intent(inout),allocatable::jr(:)
    integer,intent(in)::jr0
    logical,parameter::opt_check=.false.
    
    integer::tc
    real(dp)::le0,le,dist_le
    
    allocate(jr(nt))
    
    jr(:)=jr0
    if (opt_incr_ra) then
        do tc=51,111,4
            print*, tc
            pause
            jr(tc)=jr(tc-1)+5
        enddo
        
        !! compute remaining life expectancy at year tr:
        !le0=func_le(demo%sr(1,1,jr0:nj,tr))
        !do tc=tr+1,nt
        !    le=func_le(demo%sr(1,1,jr0:nj,tc))
        !    dist_le=le-le0
        !    jr(tc)=jr0+floor(dist_le)
        !end do
        !
        !if (opt_check) then
        !    call plot_y(dble(jr),'jr','retirement age')
        !    call execplot()
        !endif
    endif
        
    end subroutine sub_retage
    ! ----------------------------------------------------------------------------------------------------------------
    
    
    ! ----------------------------------------------------------------------------------------------------------------
    function func_le(sr)
    ! computes remaining life expectancy
    implicit none
    
    real(dp)::func_le
    real(dp),intent(in),dimension(:)::sr
    
    integer::nc,nn
    real(dp)::prob  ! conditional survival probabilitiy
    
    nn=size(sr)
    func_le=1.0_dp
    prob=1.0_dp
    do nc=1,nn-1
        prob=prob*sr(nc)
        func_le=func_le+prob
    end do
    
    
    end function func_le
    ! ----------------------------------------------------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------
    subroutine sub_techrate(demo)

    implicit none

    type(t_demo),intent(inout):: demo
    integer::tc

    ! TBC read in here time-varying rate of technological progress
    do tc=1,nt
	   demo%lamt(tc) =techgr !0.0406_dp !lamb
    end do

    end subroutine sub_techrate
    ! ---------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------
    subroutine sub_poprate(demo)

    implicit none

    type(t_demo),intent(inout):: demo
    integer::tc

    do tc=1,nt
	   demo%popgr(tc) =popgr
       demo%popgrt(tc) =popgr
    end do

    end subroutine sub_poprate
    ! ---------------------------------------------------------------------
    
    
    ! ---------------------------------------------------------------------
    subroutine sub_tech(A0,g,At)

    implicit none

    real(dp),intent(in)::A0,g(:)
    real(dp),dimension(:),intent(inout)::At
    integer::tc

    ! the model is solved in detrended terms => constant tech level
    At(1)=A0
    do tc=2,nt
	    At(tc)=At(tc-1)
        
    end do

    end subroutine sub_tech
    ! ---------------------------------------------------------------------
    
    
end module calib_mod

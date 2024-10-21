program dynprgmain
!-------------------------------------------------------------------------------------------------
! This is the main program for corona education project which ogranizes the calls of the various subroutines by modules
!-------------------------------------------------------------------------------------------------

use calib_mod    
use olg_mod
!use saving_mod
!use sumstats_mod

implicit none

type(t_agg)::agg        ! aggregates
type(t_demo)::demo      ! demographic input
type(t_grid)::grid      ! grids
type(t_lc)::lc          ! life-cycle objects
type(t_pol)::pol        ! policy and value functions
type(t_pol_st)::pol_st 
type(t_stat)::stat    ! summary statistice



!INCLUDE 'link_fnl_static.h' ! for use of IMSL under windows   

! calibration: input of parameters, allocation of objects, reading of external files
call sub_calib(agg,demo,grid,lc,pol,pol_st,stat)

! solution of OLG model
call sub_olg(stat,agg,demo,grid,lc,pol)

print*, "done"

! compute summary statistics
!if (opt_sumstats) call sub_sumstats(grid,pol,stat)


print*,''
print*, 'I am finally done'
print*,''
!pause

end program dynprgmain 



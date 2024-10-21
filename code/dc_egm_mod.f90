module dc_egm_mod
! This module contains subroutines related to implementation of the upper envelope step in DC-EGM    
    
    use nrtype
    use nr
    use nrutil
    use funcs_mod
    use types_mod
    use nrutils,only: indmax
  
    implicit none
    logical:: point_added ![ indicator for the option to add point exactly to the kink]
    
    contains
 
! ------------------------------------------------- 
subroutine sub_secondary_env(val,cons,inv,griddx,gridcom,intersectionsx,intersectionsy,num_sections,top)
! This subroutine performes upper envelope computation for choice-specific value function
! Input: Raw (potentially non-monotonic) cash-on-hand grid; raw value and policy functions
! Output: Common (monotonic) cash-on-hand grid; refined value and policy functions; intersection points;
! number of sections; index of uppermost sections at each grid point

implicit none

real(dp),intent(inout):: val(nx),cons(nx),inv(nx)
type(t_pol_st)::pol
real(dp),intent(in):: griddx(:)
real(dp),intent(out):: gridcom(size(griddx,1))
integer,intent(out):: top(size(griddx,1))
real(dp),allocatable,intent(out):: intersectionsx(:),intersectionsy(:)
integer,intent(out)::num_sections
real(dp),allocatable:: segmenttochopx(:),segmenttochopy(:)
real(dp),allocatable:: secx1(:),secx2(:),secy1(:),secy2(:),secx(:,:),secy(:,:)
real(dp),allocatable:: secx_sorted(:),secy_sorted(:)
integer:: xc,i,num_el,num_el_full,ind_nan_sec,ind_nan(size(griddx,1)),i_first
integer:: ii(size(griddx,1)),i_chop
real(dp):: vals(2)
integer:: inds(2)

pol%v=val
pol%cons=cons
pol%inv_vp_x=inv

! size of choice-specific coh grid
num_el=size(griddx,1)
num_el_full=num_el ! save it here

! identify loop-back regions (with zeros)
ii=1.0
do xc=1,num_el_full-1
    if (griddx(xc+1)<griddx(xc))then
        ii(xc)=0.0
    endif
enddo

! if non-monotonicities detected => go on to refinement
if (any(ii==0.0))then
  
    ! get segments of value function
    allocate(secx(num_el_full,num_el_full))
    allocate(secy(num_el_full,num_el_full))
    call sub_getsegments(pol%v)
    
    ! cross-check that the number of sections is correct
    i=1
    ind_nan_sec=0
    do while (ind_nan_sec==0 .or. i<=num_el_full)
        if ( isnan(secx(1,i)) )then 
            ind_nan_sec=i
            exit
        endif   
        i=i+1
    enddo
    if (num_sections/=ind_nan_sec-1)then
        print*, "something went wrong with chopping in sections", num_sections, ind_nan_sec
        pause
    endif
    
    ! sort sections (since half of them are in the opposite direction)
    call sub_sort_seg()
 
    ! take upper envelope of the value function
    call sub_upper_env(secx(:,1:num_sections),secy(:,1:num_sections),griddx,pol%v,gridcom,intersectionsx,intersectionsy,top)
    
    ! refine consumption policy (can add here any other policies that should be refined; alternatively, other policies can be recomputed in the main hh module)
    call sub_getsegments(pol%cons) ! chop into segments
    call sub_sort_seg() ! sort them
    call sub_upper_env_fixed(secx(:,1:num_sections),secy(:,1:num_sections),gridcom,pol%cons,intersectionsx,top) ! take upper envelope
    
    call sub_getsegments(pol%inv_vp_x) ! chop into segments
    call sub_sort_seg() ! sort them
    call sub_upper_env_fixed(secx(:,1:num_sections),secy(:,1:num_sections),gridcom,pol%inv_vp_x,intersectionsx,top) ! take upper envelope
    
else ! no nonmonotonicities, just copy the input for common grid; value and policy functions remain unchanged
   
    gridcom=griddx
    num_sections=1
  
endif

val = pol%v
cons = pol%cons
inv = pol%inv_vp_x

contains

    ! ---------------------------------------------
    subroutine sub_getsegments(ytochop)
    ! cut ytochop in segments (growing and falling, respectively), according to detected non-monotonicities in coh-grid

    implicit none
    real(dp),intent(in)::ytochop(:)

    i=1
    num_sections=1 ! initialize number of sections 
    num_el=size(gridcom,1)
    num_el_full=num_el
    allocate(segmenttochopx(num_el_full)) ! x (coh grid) current segment (to chop)
    allocate(segmenttochopy(num_el_full)) ! y (value/policy function) current segment (to chop)
    segmenttochopx=griddx ! start the full size (for x)
    segmenttochopy=ytochop ! start the full size (for y)
    
    ! initialize sections as NaN
    secx=sqrt(-1.0_dp)
    secy=sqrt(-1.0_dp)

    i_first=ii(1)
  
    do while (i<=num_el_full)
                 
        if (ii(i)/=i_first)then 
        
            ! chop it here, including endpoints
            do xc=1,num_el
                if ( segmenttochopx(xc)==griddx(i) ) i_chop=xc
            enddo
            
            call sub_chop(segmenttochopx,segmenttochopy,i_chop,secx1,secy1,secx2,secy2)
        
            ! initialize sections as NaN
            secx(:,num_sections:num_el_full)=sqrt(-1.0_dp)
            secy(:,num_sections:num_el_full)=sqrt(-1.0_dp)
        
            ! assign values
            secx(1:size(secx1,1),num_sections)=secx1
            secx(1:size(secx2,1),num_sections+1)=secx2
            secy(1:size(secy1,1),num_sections)=secy1
            secy(1:size(secy2,1),num_sections+1)=secy2
       
            ! continue
            num_sections=num_sections+1
            num_el=num_el-i_chop+1 ! include endpoints
        
            ! reallocate segments to chop
            deallocate(segmenttochopx)
            deallocate(segmenttochopy)
            allocate(segmenttochopx(num_el))
            allocate(segmenttochopy(num_el))
            segmenttochopx=secx2
            segmenttochopy=secy2
        
            ! reset the first point
            i_first=ii(i)
        
            i=i+1
        else
        i=i+1
        endif
    enddo

    deallocate(segmenttochopx)
    deallocate(segmenttochopy)

    end subroutine sub_getsegments
    ! ------------------------------------------


    ! ------------------------------------------
    subroutine sub_sort_seg()
    ! sort sections

    implicit none

    ind_nan=0
    
    do i=1,num_sections
        
        xc=1
        do while (ind_nan(i)==0 .or. xc<=num_el)
            if ( isnan(secx(xc,i)) )then
                ind_nan(i)=xc
                exit
            endif
            xc=xc+1
        enddo
        
        ind_nan(i)=ind_nan(i)-1
        
        ! allocate objects
        allocate(secx_sorted(ind_nan(i)))
        allocate(secy_sorted(ind_nan(i)))
        
        ! sort 
        call sub_sort_xy(secx(1:ind_nan(i),i),secy(1:ind_nan(i),i),secx_sorted,secy_sorted)
        secx(1:ind_nan(i),i)=secx_sorted
        secy(1:ind_nan(i),i)=secy_sorted
        
        ! deallocate objects
        deallocate(secx_sorted)
        deallocate(secy_sorted)
    enddo

    end subroutine sub_sort_seg
    ! ------------------------------------------
      
end subroutine sub_secondary_env
! ------------------------------------------------- 


! ------------------------------------------------- 
subroutine sub_upper_env(secx,secy,griddx,res,gridcom,intersectionsx,intersectionsy,top)
! This subroutine computes upper envelope of a function for determined segments of non-monotonic grid
! Input: segments x, segments y, raw (cash-on-hand) grid
! Output: common (regular) cash-on-hand grid; refined value function; index of the segment containing max of value function
use funcs_mod
use toolbox , only: fzero, fminsearch

implicit none
real(dp),intent(in):: secx(:,:),secy(:,:),griddx(:)
real(dp),intent(inout):: res(size(griddx,1)),gridcom(size(griddx,1))
integer,intent(out):: top(size(griddx,1))
real(dp),allocatable,intent(out):: intersectionsx(:),intersectionsy(:)
real(dp)::maxintr(size(griddx,1))
real(dp):: intersectionsx_temp(size(griddx,1)),intersectionsy_temp(size(griddx,1))
real(dp),allocatable:: intp(:,:)
real(dp)::x1,x2,y1(2),y2(2),x3,intr2(size(secx,2)),maxintr2, min_xcom, max_xcom
integer:: i,j,num_sec,num_el,k0,k1,ln1,ln2,ln3
real(dp):: vals(2),dist_y1,dist_y2
integer:: inds(2),extr1(2),extr2(2)
real(dp),parameter::epsi=1.0e-08
real(dp),parameter::tolf=1.0e-12
logical:: cont_search, extr_ind, dist_ind
integer:: ind_nan(size(secx,2)),num_inters,ind_nani

! span a common grid 
min_xcom = minval(griddx)
max_xcom = maxval(griddx)


gridcom = makegrid(min_xcom,max_xcom,size(griddx,1),1.0_dp)

! number of segments
num_sec=size(secx,2)
! number of elements in common grid
num_el=size(griddx,1)

do j=1,num_sec
    i=1
    ind_nan(j)=0
    do while (ind_nan(j)==0 .and. i<=size(secx,1))
        if (isnan(secx(i,j)))then
            ind_nan(j)=i
            exit
        endif    
        i=i+1
    enddo
    if (ind_nan(j)==0)then
        ind_nan(j)=size(secx(:,j),1)
    else
        ind_nan(j)=ind_nan(j)-1
    endif
    
enddo

allocate(intp(num_el,num_sec))

! interpolate all lines on all points recording the extrapolation cases
do j=1,num_sec    
    do i=1, num_el
        intp(i,j)=func_intp_mod(secx(1:ind_nan(j),j),secy(1:ind_nan(j),j),gridcom(i),vals,inds)  
    enddo
enddo

! find lines on the top
do i=1,num_el
    maxintr(i)=maxval(intp(i,:))
    top(i)=maxloc(intp(i,:),1)
 
    res(i)=maxintr(i) ! output
    
enddo

point_added=.false.
if (opt_add_point)then
    ! find intersection point(s)
    num_inters=0
    k0=top(1) ! index of top line
    ! loop through all gridpoints
    do j=2,num_el
        k1=top(j) ! index of next top line
        if (k1/=k0)then ! switched to another line
            ! check for intersection of 2 lines (b/w points x(j) and x(j-1))
            ln1=k0
            ln2=k1
            x1=gridcom(j-1)
            x2=gridcom(j)
            extr1=0
            extr2=0
            ! interpolate the two points on each segment
            ! seg1:
            y1(1)=func_intp_ind(secx(1:ind_nan(ln1),ln1),secy(1:ind_nan(ln1),ln1),x1,vals,inds,extr1(1))
            y1(2)=func_intp_ind(secx(1:ind_nan(ln1),ln1),secy(1:ind_nan(ln1),ln1),x2,vals,inds,extr1(2))
            ! seg2:
            y2(1)=func_intp_ind(secx(1:ind_nan(ln2),ln2),secy(1:ind_nan(ln2),ln2),x2,vals,inds,extr2(1))
            y2(2)=func_intp_ind(secx(1:ind_nan(ln2),ln2),secy(1:ind_nan(ln2),ln2),x2,vals,inds,extr2(2))
            
            dist_y1=abs(y1(1)-y2(1))
            dist_y2=abs(y1(2)-y2(2))
            extr_ind=.false.
            dist_ind=.false.
            
            if (x2<x1)then
                print*, "whatt?"
                pause
            endif
            
       
            if ( (extr1(1)==0 .or. extr1(2)==0) .and. (extr2(1)==0 .or. extr2(2)==0)  ) extr_ind=.true.
           ! if ( extr1(1)==0 .and. extr2(2)==0) extr_ind=.true.
            if (dist_y1>sqrt(epsi) .and. dist_y2>sqrt(epsi)) dist_ind=.true.

            if (  extr_ind  .and. dist_ind   )then
                ! find intersection point
                cont_search=.true.
               
                do while (cont_search==.true.)
                    
                    !call fminsearch(x3, fret, x1, x2, fun_intersect)
                 
                    x3=zbrent(fun_intersect,x1,x2,tolf)
                                                     
                    ! check if there are lines above the found intersection
                    do i=1,num_sec
                        intr2(i)=func_intp_mod(secx(1:ind_nan(i),i),secy(1:ind_nan(i),i),x3,vals,inds)
                    enddo
            
                    maxintr2=maxval(intr2)
                    ln3=maxloc(intr2,1)
            
                    if ( ln3==ln1 .or. ln3==ln2)then ! no other functions above
                        cont_search=.false.
                        ! add intersection point: to keep number of gridpoints unchanged, I substitute j point with x3 (simply move point j)
                    
                        ! move point j
                        gridcom(j)=x3
                        
                        ! compute corresponding value
                        res(j)=func_intp(secx(1:ind_nan(ln2),ln2),secy(1:ind_nan(ln2),ln2),x3,vals,inds,.false.,.false.)
                        
                        num_inters=num_inters+1
                        intersectionsx_temp(num_inters)=x3
                        intersectionsy_temp(num_inters)=res(j)
                            
                        point_added=.true.
                        
                    else ! line ln3 above the found intersection point => it's not upper envelope, search again
                        ln2=ln3 ! new candidate
                        x2=x3 !new border
                        cont_search=.true.
                    endif
                enddo
                
            else
                point_added=.false.
            endif
        
        
        endif  
   
        k0=k1 ! next step
    
    enddo

    ind_nani=0
    i=1
    do while (  (ind_nani==0) .and. (i<=size(griddx,1)) )
        if ( isnan(intersectionsx_temp(i)) ) then
            ind_nani=i
            exit
        endif
    
        i=i+1        
    enddo
    
    ind_nani=ind_nani-1
    
    allocate(intersectionsx(ind_nani))
    allocate(intersectionsy(ind_nani))
    intersectionsx=intersectionsx_temp(1:ind_nani)
    intersectionsy=intersectionsy_temp(1:ind_nani)
endif    

contains

    ! ------------------------------------------
 
    function fun_intersect(x)
    
    implicit none
    real(dp),intent(in):: x
    real(dp):: fun_intersect
    real(dp)::inters1,inters2
    
    inters1=func_intp_mod(secx(1:ind_nan(ln1),ln1),secy(1:ind_nan(ln1),ln1),x,vals,inds)
    inters2=func_intp_mod(secx(1:ind_nan(ln2),ln2),secy(1:ind_nan(ln2),ln2),x,vals,inds)
    
    fun_intersect=inters2-inters1
    
    end function fun_intersect
    ! ------------------------------------------

end subroutine sub_upper_env
! ------------------------------------------------- 


! ------------------------------------------------- 
subroutine sub_upper_env_fixed(secx,secy,gridcom,res,intersectionsx,top)

use funcs_mod

implicit none
real(dp),intent(in):: secx(:,:),secy(:,:)
real(dp),intent(in):: gridcom(:)
real(dp),allocatable,intent(in):: intersectionsx(:)
integer,intent(in):: top(:)
real(dp),intent(inout):: res(size(gridcom,1))
real(dp),allocatable:: intpf(:,:)
integer:: i,ii,j,num_sec,num_el
real(dp):: vals(2)
integer:: inds(2)
real(dp),parameter::epsi=1.0e-08
real(dp),parameter::tolf=1.0e-12
integer:: ind_nan(size(secx,2)),num_inters

! number of segments
num_sec=size(secx,2)

! number of elements in common grid
num_el=size(gridcom,1)

do j=1,num_sec
    i=1
    ind_nan(j)=0
    do while (ind_nan(j)==0 .and. i<=size(secx,1))
        if (isnan(secx(i,j)))then
            ind_nan(j)=i
            exit
        endif
        i=i+1
    enddo
    if (ind_nan(j)==0)then
        ind_nan(j)=size(secx(:,j),1)
    else
        ind_nan(j)=ind_nan(j)-1
    endif
    
enddo

allocate(intpf(num_el,num_sec))

! pick points from the top
do i=1,num_el
    j=top(i)
    res(i)=func_intp_mod(secx(1:ind_nan(j),j),secy(1:ind_nan(j),j),gridcom(i),vals,inds) ! output
enddo

if (point_added)then
    ! put/move intersection point
    num_inters=size(intersectionsx,1)
    do i=1,num_inters
       do ii=1,num_el
           if (gridcom(ii)==intersectionsx(i))then
               j=top(ii)
               res(ii)=func_intp(secx(1:ind_nan(j),j),secy(1:ind_nan(j),j),gridcom(i),vals,inds,.false.,.false.)
           endif
       enddo
    enddo
endif

end subroutine sub_upper_env_fixed
! -------------------------------------------------  


! -------------------------------------------------    
subroutine sub_sort_xy(objx,objy,objx1,objy1)

implicit none

real(dp),intent(in):: objx(:),objy(:)
real(dp),intent(inout):: objx1(size(objx,1)),objy1(size(objy,1))
integer:: i,num_el,min_ind,min_indy,xc
real(dp),allocatable:: tosort(:),tosort_new(:)
real(dp):: minx, dist_i,x_temp(2),y_temp(2)
integer:: inds(2)
real(dp):: vals(2)
real(dp),parameter::epsi=1.0e-08

i=1
num_el=size(objx,1)
allocate(tosort(num_el))
allocate(tosort_new(num_el))
tosort=objx

do while (i<=size(objx,1))
    minx=minval(tosort)
    min_ind=minloc(tosort,1)
    do xc=1,size(objx,1)
        if (objx(xc)==minx) min_indy=xc
    enddo
   
    objx1(i)=minx
    objy1(i)=objy(min_indy)
    
    ! delete smallest point
    num_el=num_el-1
    deallocate(tosort_new)
    allocate(tosort_new(num_el))
    
    tosort_new(1:min_ind-1)=tosort(1:min_ind-1)
    tosort_new(min_ind:size(tosort_new,1))=tosort(min_ind+1:size(tosort,1))
    
    deallocate(tosort)
    allocate(tosort(num_el))
    tosort=tosort_new
    i=i+1
enddo

! check if all elements in the sorted object are unique ACHTUNG
do i=1,size(objx1,1)-2
    dist_i=objx1(i+1)-objx1(i)
    if (dist_i<sqrt(epsi))then
        objx1(i+1)=(objx1(i)+objx1(i+2))/2.0_dp
        x_temp=(/ objx1(i), objx1(i+2) /)
        y_temp=(/ objy1(i), objy1(i+2) /)
        objy1(i+1)=func_intp(x_temp,y_temp,objx1(i+1),vals,inds,.false.,.false.)
    endif
enddo

deallocate(tosort)
deallocate(tosort_new)

end subroutine sub_sort_xy
! --------------------------------------------------    


! -------------------------------------------------- 
subroutine sub_chop(objx,objy,j,objx1,objy1,objx2,objy2)

implicit none
real(dp),intent(in)::objx(:),objy(:)
integer,intent(in):: j
real(dp),allocatable,intent(out):: objx1(:),objy1(:),objx2(:),objy2(:)

allocate(objx1(1:j))
allocate(objy1(1:j))
allocate(objx2(j:size(objx,1)))
allocate(objy2(j:size(objy,1)))

objx1=objx(1:j)
objx2=objx(j:size(objx,1))
objy1=objy(1:j)
objy2=objy(j:size(objy,1))


end subroutine sub_chop
! -------------------------------------------------- 


! -------------------------------------------------- 
subroutine sub_logsumprob(v,logsum,prob,nd_in,sigma)
! This is an elemental subroutine for computing logsum and choice-specific prob 
! of choice-specific value functions
implicit none
integer,intent(in):: nd_in
real(dp),intent(in):: v(nd_in),sigma
real(dp),intent(out):: logsum,prob(nd_in)
real(dp),parameter::epsi=1.0e-08
real(dp):: maxv
integer:: maxv_i

if (sigma>epsi)then ! solution w/ taste shocks
    ! take max over number of choices
    maxv=maxval(v(:)) 
    maxv_i=indmax(v,maxv) ! index the max
    ! numerically robust logsum
    logsum=maxv+sigma*log( sum(exp( (v-maxv)/sigma) ) )
    ! numerically robust prob
    prob=exp( (v-logsum)/sigma )
    
else ! solution w/o taste shocks => exact solution
    logsum=maxval(v)
    maxv_i=maxloc(v(:),1) ! index the max
    prob=0.0_dp
    prob(maxv_i)=1.0_dp
endif

end subroutine sub_logsumprob
! -------------------------------------------------- 


    
end module dc_egm_mod
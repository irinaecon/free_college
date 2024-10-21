module sumstats_mod
    
    ! this module computes summary statistics
    use nrtype
    use params_mod
    use types_mod
    use alexutils
    
	implicit none
    
    contains

! -----------------------------------------------------------------
subroutine sub_sumstats(grid,pol,stat)
! organizes computation of summary statistics
implicit none

type(t_grid),intent(inout)::grid
type(t_pol),intent(in)::pol
type(t_stat),intent(inout)::stat

real(dp),allocatable,dimension(:)::distrvec,yvec
real(dp),allocatable,dimension(:)::lorx,lory
real(dp),parameter::epsi=1.0e-08_dp
real(dp),parameter::min_inc=0.05
real(dp)::cvy0,vary0,mean_y,med_y
integer::tc,tcc,nh
integer::ntg

print*, ' '
print*, '----------------------------------------'
print*, 'Computation of Summary Statistics'
print*, '----------------------------------------'
print*, ' '

! allocate objects of Lorenz curve and Gini
ntg=(t2g-t1g)/tg_delt+1
allocate(stat%gini_ass(ntg))
allocate(stat%varlog_ass(ntg))
allocate(stat%gini_cons(ntg))
allocate(stat%varlog_cons(ntg))

tcc=1
do tc=1,1 !t1g,t2g,tg_delt       ! compute Gini, etc. only every tg_delt years for years from t1g to t2g
    ! ********************************
    ! asset gini
    
    ! sort and compute nh
    call sub_vectorize(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,:,tc),grid%ass(:,:,:,:,:,:,:,:,:,:,:,:,:,tc),distrvec,yvec,nh,.false.)

    ! allocate objects here bec. nh is only known after call to sub_vectorize
    if (tcc==1) then
        allocate(lorx(nh))
        allocate(lory(nh))
    endif
    
    ! compute gini
    call sub_gini(distrvec,yvec,lorx(:),lory(:),stat%gini_ass(tcc),.false.)
    
    ! correct small and negative entries (rounding error):
    yvec=max(epsi,yvec)
    
    ! compute variance of logs, coefficient of variation, median 
    call sub_varcv(distrvec,log(yvec),stat%varlog_ass(tcc),cvy0,med_y)

    
    ! ********************************
    ! consumption gini
    
    ! sort and compute nh, now for consumption
    call sub_vectorize(grid%Phi(:,:,:,:,:,:,:,:,:,:,:,:,:,tc),pol%cons(:,:,:,:,:,:,:,:,:,:,:,:,:,tc),distrvec,yvec,nh,.false.)

    ! compute gini
    call sub_gini(distrvec,yvec,lorx(:),lory(:),stat%gini_cons(tcc),.false.)
    
    ! correct small entries (rounding error):
    yvec=max(epsi,yvec)
    
    ! compute variance of logs, coefficient of variation, median 
    call sub_varcv(distrvec,log(yvec),stat%varlog_cons(tcc),cvy0,med_y)

    
    tcc=tcc+1
end do

end subroutine sub_sumstats
! -----------------------------------------------------------------

! -----------------------------------------------------------------
subroutine sub_vectorize(distr,y,distrvec,yvec,nh,opt_srt)

use alexutils, only: linmake
implicit none

real(dp),dimension(:,:,:,:,:,:,:,:,:,:,:,:,:),intent(in)::distr,y
real(dp),allocatable,dimension(:),intent(out)::distrvec,yvec
integer,intent(out)::nh
logical,intent(in):: opt_srt

integer,allocatable,dimension(:)::perm
integer::hc

! build objects
nh=size(y)
allocate(distrvec(nh))
allocate(yvec(nh))
allocate(perm(nh))

! build vectors
yvec=reshape(y,(/nh/))
distrvec=reshape(distr,(/nh/))

if (opt_srt)then
    ! sort matrices according to y-dimension
    perm=linmake(nh)
    call dsvrgp(nh,yvec,yvec,perm)
    call dpermu(nh,distrvec,perm,1,distrvec)
endif

end subroutine sub_vectorize
! -----------------------------------------------------------------

! ----------------------------------------------------------------
function fun_gini(distr,y)
use omp_lib
implicit none
real(dp),intent(in):: distr(:),y(:)
real(dp):: fun_gini
integer:: n,i,j
real(dp):: mu,s,d

n=size(y)

mu=sum(y(:)*distr(:))

! check for negative entries (tbc put assert statement here)
do i=1,n
    if (y(i)<0.0_dp)then
        print*, 'Income/consumption value can not be negativ. Please verify your data'
    endif
enddo

! compute gini index
s=0
!$OMP PARALLEL
!$OMP DO collapse(2), private(i,j), reduction(+:s)
do i=1,n
    do j=1,n
        if (distr(i)==0.0_dp .or. distr(j)==0.0_dp) cycle
        s=s+distr(i)*distr(j)*abs(y(i)-y(j))
    enddo
enddo
!$OMP END DO
!$OMP END PARALLEL
d=2.0_dp*mu*(n**2.0_dp)
fun_gini=s/d

end function fun_gini
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
recursive subroutine quicksort(a, first, last)
  implicit none
  real(dp),intent(inout)::  a(:)
  real(dp):: x, t
  integer,intent(in):: first, last
  integer i, j

  x = a( (first+last) / 2 )
  i = first
  j = last
  do
     do while (a(i) < x)
        i=i+1
     end do
     do while (x < a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  if (first < i-1) call quicksort(a, first, i-1)
  if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
subroutine sub_gini(distr,y,lorx,lory,gini,opt_srt)

! computes the gini coefficient

implicit none

real(dp),dimension(:),intent(in)::distr,y
real(dp),intent(out),dimension(:),optional::lorx,lory
real(dp),intent(out)::gini
logical,intent(in):: opt_srt
integer:: nhz

real(dp)::mass,toty
integer::nh,hc,hcc

nh=size(distr,1)

toty=sum(y(:)*distr(:))

if (opt_srt)then
    ! compute the lorenz curves (only for sorted data)
    do hc=1,nh
        mass=sum(y(1:hc)*distr(1:hc))
        lory(hc)=mass/toty
        lorx(hc)=sum(distr(1:hc))
    end do
endif

gini=0.0_dp
nhz = count(distr>0.0_dp)
!$OMP PARALLEL
!$OMP DO collapse(2), private(hc,hcc), reduction(+:gini)
do hc=1,nh
    do hcc=1,nh
        gini=gini+distr(hc)*distr(hcc)*abs(y(hc)-y(hcc))
    end do
end do
!$OMP END DO
!$OMP END PARALLEL
gini=gini/(2.0_dp*toty*nhz**2.0_dp)

end subroutine sub_gini
! ---------------------------------------------------------------------


! ---------------------------------------------------------------------
subroutine sub_theil(distr,y,theil)

implicit none
real(dp),dimension(:),intent(in)::distr,y
real(dp),intent(out)::theil

real(dp)::temp,toty
integer::nh,hc,hcc

nh=size(distr,1)

toty=sum(y(:)*distr(:))

temp = 0.0_dp
do hc = 1,nh
    temp = temp + y(hc)/toty * log(y(hc)/toty)
enddo

theil = temp/nh

end subroutine sub_theil
! ---------------------------------------------------------------------


! ---------------------------------------------------------------------
subroutine sub_varcv(distr,y,vary,cvy,medy)

! computes variance, coefficient of variation and median of y

implicit none

real(dp),dimension(:),intent(in)::distr,y
real(dp),intent(out)::vary,cvy,medy

real(dp)::meany,meany2,sumdistr,afac
real(dp),allocatable::adistr(:)
integer::nh,hc

nh=size(distr,1)
allocate(adistr(nh))

! make sure that distr sums to one:
afac=sum(distr(:))
adistr=distr/afac

meany=sum(y(:)*adistr(:))
meany2=sum(y(:)**2.0*adistr(:))

vary=meany2-meany**2
cvy=sqrt(vary)/meany

! determine median:
hc=0
sumdistr=0.0_dp
do while ( sumdistr<=0.5 )
    ! only look at positive entries in distribution:
    hc=hc+1
    sumdistr=sumdistr+adistr(hc)
end do
medy=y(hc)

end subroutine sub_varcv
! ---------------------------------------------------------------------






end module sumstats_mod





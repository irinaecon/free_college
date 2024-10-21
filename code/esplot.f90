!##############################################################################
! MODULE ESPlot 
! 
! The module ESPlot is a utility to easily plot graphs with ESPlot.
!
! In order to correctly use this module, you must have installed ESPlot from:
! http://www.neng.usu.edu/mae/faculty/stevef/prg/ESPlot/index.html
! and have adapted the variable esplotfile to your ESPlot.exe path.
!
! copyright: Fabian Kindermann
!            University of Wuerzburg
!            kindermann.fabian@uni-wuerzburg.de
!##############################################################################

module ESPlot

implicit none

save


!##############################################################################
! Declaration of variables                                                  
!##############################################################################

! is true if a data column has previously been written and the plot was not
!     executed
logical, private :: tfiovar=.false.

! gives the path where the ESPlot.exe program is
character (LEN=*), parameter, private :: &
    esplotfile='"C:\Program Files (x86)\ESPlot v1.3c\ESPlot.exe"'
            

!##############################################################################
! Interface declarations
!##############################################################################


!##############################################################################
! INTERFACE plot
! 
! Interface for plot with either y or x-y.
!##############################################################################
interface plot

    ! define methods used
    module procedure plot_xy, plot_y
        
end interface            


!##############################################################################
! Subroutines and functions                                               
!##############################################################################

contains


!##############################################################################
! SUBROUTINE plot_xy 
! 
! Plots a x-y-data column to the output file.
!##############################################################################
subroutine plot_xy(x, y, dataname, plotname)

    implicit none

    
    !##### INPUT/OUTPUT VARIABLES #############################################

    ! x and y values
    real*8, intent(in) :: x(:), y(:)
    
    ! the name of the data-column in plot's legend
    character (len = *), optional, intent(in) :: dataname
    
    ! the name of the plot
    character (len = *), optional, intent(in) :: plotname
    
    
    !##### OTHER VARIABLES ####################################################
    
    integer :: n, i1    
    
    
    !##### ROUTINE CODE #######################################################
    
    ! open output file unit and set tfiovar to true
    if(.not.tfiovar) then
        open(213659,file='outplotdata1354.dat')
        tfiovar=.true.
    endif
    
    ! set array sizes
    n = size(x,1)
    
    if(size(x,1) /= size(y, 1))then
        write(*,*)'Error in ESPlot: Arrays do not have same size!'
        stop
    endif
    
    ! check wether plotname is present
    if(present(plotname))then
        write(213659,'(a,a)')'$ ',plotname
    else
        write(213659,'(a)')'$ '
    endif
    
    ! check wether dataname is present
    if(present(dataname))then
        write(213659,'(a,a)')'! ',dataname
    endif
    
    ! write data to file
    do i1 = 1,n
        write(213659,'(f30.10,2x,f30.10)')x(i1),y(i1)
    enddo
    
end subroutine plot_xy


!##############################################################################
! SUBROUTINE plot_y
! 
! Plots a y-data column to the output file.
!##############################################################################
subroutine plot_y(y, dataname, plotname)

    implicit none

    
    !##### INPUT/OUTPUT VARIABLES #############################################

    ! x and y values
    real*8, intent(in) :: y(:)
    
    ! the name of the data-column in plot's legend
    character (len = *), optional, intent(in) :: dataname
    
    ! the name of the plot
    character (len = *), optional, intent(in) :: plotname
    
    
    !##### OTHER VARIABLES ####################################################
    
    integer :: n, i1
    real*8, allocatable :: x(:)
    
    
    !##### ROUTINE CODE #######################################################
    
    ! open output file unit and set tfiovar to true
    if(.not.tfiovar) then
        open(213659,file='outplotdata1354.dat')
        tfiovar=.true.
    endif
    
    ! set array sizes
    n = size(y, 1)    
    
    ! Set up x array
    allocate(x(n))
    
    ! Fill x array with integer values
    do i1 = 1,n
        x(i1) = dble(i1)
    enddo
    
    ! check wether plotname is present
    if(present(plotname))then
        write(213659,'(a,a)')'$ ',plotname
    else
        write(213659,'(a)')'$ '
    endif
    
    ! check wether dataname is present
    if(present(dataname))then
        write(213659,'(a,a)')'! ',dataname
    endif
    
    ! write data to file
    do i1 = 1,n
        write(213659,'(f30.10,2x,f30.10)')x(i1),y(i1)
    enddo
    
end subroutine plot_y


!##############################################################################
! SUBROUTINE execplot
! 
! Executes ESPlot and plots the data columns.
!##############################################################################
subroutine execplot()

    implicit none


    !##### ROUTINE CODE #######################################################
    
    ! close output file unit
    close(213659)
    tfiovar=.false.

    ! execute ESPlot
    call system('"'//ESplotfile//'" outplotdata1354.dat')

    ! delete plot file
    call system('del outplotdata1354.dat')

end subroutine   

end module ESPlot
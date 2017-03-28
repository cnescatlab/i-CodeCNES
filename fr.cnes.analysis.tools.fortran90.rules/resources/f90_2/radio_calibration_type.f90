!!* type_Radio_Calibration -- Public
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: april 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-04-26 09:23:18 $
!!
!> type_Detection -- Module
!!
!! * Purpose
!!
!!     Define parameters for radiometric calibration simulation
!!
!! * Description
!!
!!     type for declaration and allocation of radiometric calibration simulation
!!
!! * Sub-routines and functions
!!
!!     * type_Radio_Calibration   : type for declaration and allocation of 
!!                                  radiometric calibration simulation
!!     * alloc_Radio_Calibration  : type_Radio_Calibration allocation
!!     * dalloc_Radio_Calibration : type_Radio_Calibration deallocation
!!
!! * References
!!
!!     SPS ATBD


module radio_calibration_type
  use precision_type
  use error_type
  use constantes_type
!
  implicit none
!
!
  public :: type_Radio_Calibration,  &
            alloc_Radio_Calibration, &
            dalloc_Radio_Calibration

  type :: type_Radio_Calibration
     character(len=500)                            :: filename !< radiometric calibration characterisation file name 
     integer(kind=LONG)                            :: NsWn0 !< backbody emissivity samples number
     real(kind=DOUBLE) ,dimension(:) ,allocatable  :: Wn0 !< wavenumbers - Wn0(1:NsWn0) 
     real(kind=DOUBLE) ,dimension(:) ,allocatable  :: BB_Emis !< backbody emissivity - BB_Emis(1:NsWn0)
     integer(kind=LONG)                            :: NbBBRrs !< number of reflected radiative surface by the blackbody
     real(kind=DOUBLE),dimension(:) ,allocatable   :: TRrs !< reflected radiative surface temperature - TRrs(1:NbBBRrs) (K)
     real(kind=DOUBLE),dimension(:) ,allocatable   :: RRrs !< reflected radiative surface ratio - RRrs(1:NbBBRrs) (K)
  end type type_Radio_Calibration
!
contains
!
!--------------------------------------------------------------------
  subroutine alloc_Radio_Calibration( radio_cal )
    implicit none
    type(type_Radio_Calibration), intent(inout) :: radio_cal
    integer(kind=LONG)                          :: ErrCode
    integer(kind=LONG)                          :: ioalloc
    !
    ioalloc = 0
    allocate( radio_cal%Wn0(radio_cal%NsWn0), stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1
    allocate( radio_cal%BB_Emis(radio_cal%NsWn0), stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1
    allocate( radio_cal%TRrs(radio_cal%NbBBRrs), stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1
    allocate( radio_cal%RRrs(radio_cal%NbBBRrs), stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1
    !
    if( ioalloc > 0 ) then
       write(*,*) 'allocation radio_cal Error'
       write(*,*) 'radio_cal: fatal error', ioalloc
       call exit(1)
    end if
    return
  end subroutine alloc_Radio_Calibration
  !
!--------------------------------------------------------------------
  subroutine dalloc_Radio_Calibration( radio_cal )
    implicit none
    type(type_Radio_Calibration) , intent(inout) :: radio_cal
    !
    deallocate( radio_cal%Wn0 )
    deallocate( radio_cal%BB_Emis )
    deallocate( radio_cal%TRrs )
    deallocate( radio_cal%RRrs )
    return
  end subroutine dalloc_Radio_Calibration
!
!
end module radio_calibration_type

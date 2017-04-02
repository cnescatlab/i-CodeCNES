!!* type_Detection -- Public
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: august 2010
!!*           Version: $Revision: 1.4 $
!!* Last modification: $Date: 2011-11-02 14:57:53 $
!!
!> type_Detection -- Module
!!
!! * Purpose
!!
!!     Define parameters for detection simulation
!!
!! * Description
!!
!!     type for declaration and allocation of detection simulation
!!
!! * Sub-routines and functions
!!
!!     * type_Detection   : type for declaration and allocation of detection simulation
!!     * alloc_Detection  : type_Detection allocation
!!     * dalloc_Detection : type_Detection deallocation
!!
!! * References
!!



module detection_type
   use precision_type
   use error_type
   use constantes_type
!
   implicit none
!
!
   public :: type_Detection,  &
             alloc_Detection, &
             dalloc_Detection

   type :: type_Detection
     character(len=500)                            :: filename !< detection characterisation file name 
     integer(kind=LONG)                            :: NsWn0 !< frequencies samples number
     real(kind=DOUBLE) ,dimension(:) ,allocatable  :: Wn0 !< wavenumbers - Wn0(1:NsWn0) 
     real(kind=DOUBLE) ,dimension(:) ,allocatable  :: Detector_Response !< Detector response - Detector_Response(1:NsWn0)
     real(kind=DOUBLE) ,dimension(:) ,allocatable  :: Transfert_Mod !< Transfert function - Transfert_Fct(1:NsWn0)  
     real(kind=DOUBLE) ,dimension(:) ,allocatable  :: Transfert_Dir !< Transfert function - Transfert_Fct(1:NsWn0)  
     real(kind=DOUBLE)                             :: V_Min !< Output Min (Volts)
     real(kind=DOUBLE)                             :: V_Max !< Output Max (Volts)
     real(kind=DOUBLE)                             :: Dark_Current !< Dark_Current (Amp)
     real(kind=DOUBLE)                             :: Rcr !< Resistance of negative feedback (Ohm)
     real(kind=DOUBLE)                             :: Vdc !< Continuous offset voltage (Volts)  
     real(kind=DOUBLE)                             :: Noise !< Detection Noise Amp/(Hz)**0.5
     real(kind=DOUBLE)                             :: Photon_Noise !< Detection Noise Amp/(Hz)**0.5
     real(kind=DOUBLE)                             :: H_Sat !< Altitude Satellite(m)
     real(kind=DOUBLE)                             :: Pupill_Diam !< Pupill diameter (m)
     real(kind=DOUBLE)                             :: Pixel_Diam !< Pixel diameter (m)
      integer(kind=LONG)                           :: SigS !< Smoothing length
   end type type_Detection
!
   contains
!
!
   subroutine alloc_Detection( Detection )
   implicit none
     type(type_Detection)    , intent(inout)      :: Detection
     integer(kind=LONG)                           :: ErrCode
     integer(kind=LONG)                           :: ioalloc
!
     ioalloc = 0
     allocate( Detection%Wn0(Detection%NsWn0),               stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Detection%Detector_Response(Detection%NsWn0), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Detection%Transfert_Mod(Detection%NsWn0),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Detection%Transfert_Dir(Detection%NsWn0),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
!
     if( ioalloc > 0 ) then
        write(*,*) 'allocation Detection Error'
        write(*,*) 'Detection: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Detection
!
!
   subroutine dalloc_Detection( Detection )
   implicit none
     type(type_Detection)    , intent(inout)         :: Detection
!
     deallocate( Detection%Wn0 )
     deallocate( Detection%Detector_Response )
     deallocate( Detection%Transfert_Mod )
     deallocate( Detection%Transfert_Dir )
     return
   end subroutine dalloc_Detection
!
!
end module detection_type

!!* spike_detection_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: march 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-04-12 08:32:32 $
!!
!> spike_detection_type -- Module
!!
!! * Purpose
!!
!!   Module for type Spike_Detection parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the Spike Detection parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Spike_Detection   : type for declaration and allocation
!!     * alloc_Spike_Detection  : type_Spike_Detection allocation
!!     * dalloc_Spike_Detection : type_Spike_Detection deallocation
!!
!! * References
!!     SPS ATBD
!!


module spike_detection_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Spike_Detection,  &
             alloc_Spike_Detection, &
             dalloc_Spike_Detection
!
   type :: type_Spike_Detection
     character(len=500)                               :: filename     !< non-linearity parametres file name
     integer(kind=LONG)                               :: NTaps_CF     !< number of taps in Central Fringe
     integer(kind=LONG)                               :: NTaps_CFO    !< number of taps in Outside Central Fringe
     real(kind=DOUBLE)  ,dimension(:), allocatable    :: Taps_CF      !< taps weights in the Central Fringe - Taps_CF(NTaps_CF)
     real(kind=DOUBLE)  ,dimension(:), allocatable    :: Taps_CFO     !< taps weights Outside Central Fringe - Taps_CFO(NTaps_CFO)
     real(kind=DOUBLE)                                :: Opd_Width_CF !< width in OPD of the Central Fringe
     integer(kind=LONG)                               :: Ns_Width_CF  !< number of sample in the Central Fringe
     integer(kind=LONG)                               :: Ns_First_CF  !< first sample of the Central Fringe
     integer(kind=LONG)                               :: Ns_Last_CF   !< last sample of the Central Fringe
     real(kind=DOUBLE)                                :: Cutoff_CF    !< cutoff of the filtered interferogram in the Central Fringe
     real(kind=DOUBLE)                                :: Cutoff_CFO   !< cutoff of the filtered interferogram Outside Central Fringe
     integer(kind=LONG)                               :: Count_CF     !< number of spike in the Central Fringe
     integer(kind=LONG)                               :: Count_CFO    !< number of spike Outside Central Fringe
     integer(kind=LONG)                               :: Flag_Spike   !< spike flag 0= interferogram affected by spikes
   end type type_Spike_Detection
!
   contains
!
!
   subroutine alloc_Spike_Detection( Spike_Detection )
   implicit none
     type(type_Spike_Detection), intent(inout)           :: Spike_Detection
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
!
     ioalloc = 0
     allocate( Spike_Detection%Taps_CF(-Spike_Detection%NTaps_CF/2: &
                                       +Spike_Detection%NTaps_CF/2),&
                                                       stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spike_Detection%Taps_CFO(-Spike_Detection%NTaps_CFO/2: &
                                        +Spike_Detection%NTaps_CFO/2),&
                                                         stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if (ioalloc > 0) then
        write(0,*) 'allocation Error',ioalloc
        write(0,*) 'Spike_Detection: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Spike_Detection
!
!
   subroutine dalloc_Spike_Detection( Spike_Detection )
   implicit none
     type(type_Spike_Detection), intent(inout)            :: Spike_Detection
!
     deallocate( Spike_Detection%Taps_CF )
     deallocate( Spike_Detection%Taps_CFO )
!
     return
   end subroutine dalloc_Spike_Detection
!
!
end module spike_detection_type

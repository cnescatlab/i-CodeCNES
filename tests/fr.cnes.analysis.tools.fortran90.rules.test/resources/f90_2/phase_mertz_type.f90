!!* phase_mertz_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: may 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-06-22 10:06:27 $
!!
!> phase_mertz_type -- Module
!!
!! * Purpose
!!
!!   Module for type Phase_mertz parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the Phase_mertz parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Phase_Mertz   : type for declaration and allocation of Psf
!!     * alloc_Phase_Mertz  : type_Phase_Mertz allocation
!!     * dalloc_Phase_Mertz : type_Phase_Mertz deallocation
!!
!! * References
!!


module phase_mertz_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Phase_Mertz,  &
             alloc_Phase_Mertz, &
             dalloc_Phase_Mertz
!
   type :: type_Phase_Mertz
     character(len=500)                               :: filename !< phase_Mertz parameters characterisation file name
     character(len=10)                                :: Mode_Traitement !< Mode de traitement (MESUREIASI or SIMULATION)
     character(len=10)                                :: Filter !< filter type
     integer(kind=LONG)                               :: Deg_Polynom !< Phase fitting polynom
     real(kind=DOUBLE)                                :: Ratio_bilatere !< Ratio Bilatere
     integer(kind=LONG)                               :: Nband !< Spectral bands number
     integer(kind=LONG)                               :: NsFft !< Fourier transform samples number
     integer(kind=LONG)                               :: N_Sample !< usefull samples number
     integer(kind=LONG)                               :: Ns_First !< first usefull sample
     integer(kind=LONG)                               :: Ns_Last !< last usefull sample
     real(kind=DOUBLE)                                :: Wn_First !< first usefull wavenumber (m-1)
     real(kind=DOUBLE)                                :: Wn_Last !< last usefull wavenumber (m-1)
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling (m-1)
     real(kind=DOUBLE)                                :: dOpd !< OPD sampling (m)
     integer(kind=LONG)                               :: SigI !< samples number of the smoothing function on the interferogram
     real(kind=DOUBLE) ,dimension(:)  ,allocatable    :: Wn !< usefull wavenumber (m-1) - Wn(1:Ns_Fft+1)
     real(kind=DOUBLE) ,dimension(:)  ,allocatable    :: Sp_Mod !< usefull spectrum - Sp(1:Ns_Fft+1)
     real(kind=DOUBLE) ,dimension(:)  ,allocatable    :: Sp_Arg !< usefull spectrum - Sp(1:Ns_Fft+1)
   end type type_Phase_Mertz
!
   contains
!
!
   subroutine alloc_Phase_Mertz( Phase_Mertz )
   implicit none
     type(type_Phase_Mertz), intent(inout)               :: Phase_Mertz
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
!
     ioalloc = 0
     allocate( Phase_Mertz%Wn(Phase_Mertz%NsFft+1),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Phase_Mertz%Sp_Mod(Phase_Mertz%NsFft+1),         stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Phase_Mertz%Sp_Arg(Phase_Mertz%NsFft+1),         stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if (ioalloc > 0) then
        write(0,*) 'allocation Error',ioalloc
        write(0,*) 'Phase_Mertz: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Phase_Mertz
!
!
   subroutine dalloc_Phase_Mertz( Phase_Mertz )
   implicit none
     type(type_Phase_Mertz), intent(inout)                   :: Phase_Mertz
!
     deallocate( Phase_Mertz%Wn )
     deallocate( Phase_Mertz%Sp_Mod )
     deallocate( Phase_Mertz%Sp_Arg )
!
     return
   end subroutine dalloc_Phase_Mertz
!
!
end module phase_mertz_type

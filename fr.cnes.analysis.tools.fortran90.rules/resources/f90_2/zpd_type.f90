!!* zpd_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.7 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> zpd_type -- Module
!!
!! * Purpose
!!
!!   Module for type zpd parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the zpd parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Zpd   : type for declaration and allocation of Zpd
!!     * alloc_Zpd  : type_Zpd allocation
!!     * dalloc_Zpd : type_Zpd deallocation
!!
!! * References
!!

module zpd_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Zpd,  &
             alloc_Zpd, &
             dalloc_Zpd
!
   type :: type_Zpd
     character(len=500)                               :: filename !< Zpd characterisation file name
     integer(kind=LONG)                               :: WidthCF !< half width for the central fringe research
     integer(kind=LONG)                               :: Ns_Fft !< Fourier transform samples number
     integer(kind=LONG)                               :: N_Sample !< samples number
     integer(kind=LONG)                               :: Ns_First !< first sample number
     integer(kind=LONG)                               :: Ns_Last !< last sample number
     real(kind=DOUBLE)                                :: Wn_First !< first wavenumber (m-1)
     real(kind=DOUBLE)                                :: Wn_Last !< last wavenumber (m-1)
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling
     real(kind=DOUBLE)                                :: Round_Offset !< algorithmic parameter for the nzpd determination 
     integer(kind=LONG)                               :: NZpd !< Sample Number at Zero Path Difference  
     integer(kind=LONG)                               :: NZpd_Connes !< Sample Number at Zero Path Difference computed with Connes' method
     integer(kind=LONG)                               :: NZpd_Barycentre !< Sample Number at Zero Path Difference computed with the interferogram barycentre method
     integer(kind=LONG)                               :: NstepSrdFT !< sample number used as first guess for zpd determination
     integer(kind=LONG)                               :: OffsetCF !< offset used as first guess for central fringe determination
     real(kind=DOUBLE)                                :: Qual_Index_Cutoff !< quality index cutoff
     real(kind=DOUBLE)                                :: Qual_Index_Connes !< quality index with Connes' method
     real(kind=DOUBLE)                                :: Qual_Index !< quality index
     integer(kind=LONG)                               :: Flag_Qual !< quality flag
     real(kind=DOUBLE)                                :: Offset_Barycentre !< offset (in fraction) obtained for central fringe determination with barycentre's method 
     real(kind=DOUBLE)                                :: Offset_Connes !< offset (in fraction) obtained for central fringe determination with Connes' method
     complex(kind=DOUBLE),dimension(:)  ,allocatable  :: Sp_Reduced_Connes !< extracted complex reduced spectrum with Connes' method - Sp_Reduced_Connes(1:N_Sample)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Mod_Connes !< modulus of extracted complex reduced spectrum with Connes' method - Mod_Connes(1:N_Sample) 
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Arg_Connes !< argument of extracted complex reduced spectrum with Connes' method - Arg_Connes(1:N_Sample)
     complex(kind=DOUBLE),dimension(:)  ,allocatable  :: Sp_Reduced !< extracted complex reduced spectrum - Sp_Reduced(1:N_Sample)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Mod !< modulus of extracted complex reduced spectrum - Mod(1:N_Sample)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Arg !< argument of extracted complex reduced spectrum - Arg(1:N_Sample)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Phase_Model !< model phase - Phase_Model(1:N_Sample)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Wn !< wavenumber (m-1) - Wn(1:N_Sample)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Dist !< distance between the complex spectra samples and the calibration line - Dist(1:2*NstepSrdFT+1)
   end type type_Zpd
!
   contains
!
!
   subroutine alloc_Zpd( Zpd )
   implicit none
     type(type_Zpd), intent(inout)                    :: Zpd
     integer(kind=LONG)                               :: ErrCode
     integer(kind=LONG)                               :: ios
     ios = 0
     allocate( Zpd%Sp_Reduced_Connes(Zpd%N_Sample),      stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Mod_Connes(Zpd%N_Sample),             stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Arg_Connes(Zpd%N_Sample),             stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Sp_Reduced(Zpd%N_Sample),             stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Phase_Model(Zpd%N_Sample),            stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Mod(Zpd%N_Sample),                    stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Arg(Zpd%N_Sample),                    stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Wn(Zpd%N_Sample),                     stat=ErrCode )
     ios = ios + ErrCode
     allocate( Zpd%Dist(2*Zpd%NstepSrdFT+1),             stat=ErrCode )
     ios = ios + ErrCode
     if (ios > 0) then
        write(0,*) 'allocation Error',ios
        write(0,*) 'Zpd: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
!
     return
   end subroutine alloc_Zpd
!
!
   subroutine dalloc_Zpd( Zpd )
   implicit none
     type(type_Zpd), intent(inout)                       :: Zpd
!
     deallocate( Zpd%Sp_Reduced_Connes )
     deallocate( Zpd%Sp_Reduced )
     deallocate( Zpd%Phase_Model )
     deallocate( Zpd%Mod )
     deallocate( Zpd%Arg )
     deallocate( Zpd%Wn )
     deallocate( Zpd%Dist )
!
     return
   end subroutine dalloc_Zpd
!
!
end module zpd_type

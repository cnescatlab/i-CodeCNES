!!* spectral_l1b_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: mars 2010
!!*           Version: $Revision: 1.4 $
!!* Last modification: $Date: 2011-11-02 14:57:54 $
!!
!> spectral_l1b_type -- Module
!!
!! * Purpose
!!
!!   Module for type spectral l1b parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the spectral l1b parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Spectral_L1b   : type for declaration and allocation of spectral l1b parameters
!!     * alloc_Spectral_L1b  : type_Spectral_L1b allocation
!!     * dalloc_Spectral_L1b : type_Spectral_L1b deallocation
!!
!! * References
!!


module spectral_l1b_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Spectral_L1b,  &
             alloc_Spectral_L1b, &
             dalloc_Spectral_L1b  
!
   type :: type_Spectral_L1b
     character(len=500)                                  :: filename !< spectral l1b parameters characterisation file name
     integer(kind=LONG)                                  :: NsFft !< Fft Size
     integer(kind=LONG)                                  :: OSFactor_zeropad !< oversampling factor when zero padding method is used
     integer(kind=LONG)                                  :: OSFactor_dephas !< oversampling factor when dephasing method is used
     integer(kind=LONG)                                  :: OSFactor_spline !< oversampling factor when spline interpolation method is used
     integer(kind=LONG)                                  :: SigS !< Edges smoothing
     integer(kind=LONG)                                  :: Nband !< spectral bands number
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: WnS !< First wavenumber for each spectral band (m-1) - WnS(1:Nband)
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: WnE !< Last wavenumber for each spectral band (m-1) - WnE(1:Nband)
     real(kind=DOUBLE)                                   :: dWn !< spectral sampling (m-1)
   end type type_Spectral_L1b
!
   contains
!
!
   subroutine alloc_Spectral_L1b( Spectral_L1b )
   implicit none
     type(type_Spectral_L1b), intent(inout)        :: Spectral_L1b
     integer(kind=LONG)                            :: ErrCode
     integer(kind=LONG)                            :: ioalloc
!
     ioalloc = 0
     allocate( Spectral_L1b%WnS(Spectral_L1b%Nband),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1b%WnE(Spectral_L1b%Nband),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc > 0 ) then
        write(0,*) 'allocation Spectral_L1b Error',ioalloc
        write(0,*) 'Spectral_l1b_type: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectral_L1b
!
!
   subroutine dalloc_Spectral_L1b( Spectral_L1b )
   implicit none
     type(type_Spectral_L1b), intent(inout)         :: Spectral_L1b
     deallocate( Spectral_L1b%WnS )
     deallocate( Spectral_L1b%WnE )
     return
   end subroutine dalloc_Spectral_L1b
!
!
end module spectral_l1b_type

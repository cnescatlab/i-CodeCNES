!!* spectral_l1b_mb_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: mars 2010
!!*           Version: $Revision: 1.4 $
!!* Last modification: $Date: 2011-11-02 14:57:54 $
!!
!> spectral_l1b_mb_type -- Module
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
!!     * type_Spectral_l1b_mb   : type for declaration and allocation of spectral l1b parameters
!!     * alloc_Spectral_l1b_mb  : type_Spectral_l1b_mb allocation
!!     * dalloc_Spectral_l1b_mb : type_Spectral_l1b_mb deallocation
!!
!! * References
!!


module spectral_l1b_mb_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Spectral_L1b_mb,  &
             alloc_Spectral_L1b_mb, &
             dalloc_Spectral_L1b_mb  
!
   type :: type_Spectral_L1b_mb
     character(len=500)                                  :: filename !< spectral l1b parameters characterisation file name
     integer(kind=LONG)                                  :: Nband !< spectral bands number
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: NsFft !< Fft Size
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: OSFactor_zeropad !< oversampling factor when zero padding method is used
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: OSFactor_dephas !< oversampling factor when dephasing method is used
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: OSFactor_spline !< oversampling factor when spline interpolation method is used
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: SigS !< Edges smoothing
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: WnS !< First wavenumber for each spectral band (m-1) - WnS(1:Nband)
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: WnE !< Last wavenumber for each spectral band (m-1) - WnE(1:Nband)
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: dWn !< spectral sampling (m-1)
   end type type_Spectral_L1b_mb
!
   contains
!
!
   subroutine alloc_Spectral_L1b_mb( Spectral_l1b )
   implicit none
     type(type_Spectral_L1b_mb), intent(inout)     :: Spectral_l1b
     integer(kind=LONG)                            :: ErrCode
     integer(kind=LONG)                            :: ioalloc
!
     ioalloc = 0
     allocate( Spectral_l1b%NsFft(Spectral_l1b%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%OSFactor_zeropad(Spectral_l1b%Nband),   &
                                                             stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%OSFactor_dephas(Spectral_l1b%Nband),    &
                                                             stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%OSFactor_spline(Spectral_l1b%Nband),    &
                                                             stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%SigS(Spectral_l1b%Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%WnS(Spectral_l1b%Nband),   stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%WnE(Spectral_l1b%Nband),   stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_l1b%dWn(Spectral_l1b%Nband),   stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc > 0 ) then
        write(0,*) 'allocation Spectral_L1b_mb Error',ioalloc
        write(0,*) 'Spectral_L1b_mb_type: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectral_L1b_mb
!
!
   subroutine dalloc_Spectral_L1b_mb( Spectral_l1b )
   implicit none
     type(type_Spectral_L1b_mb), intent(inout)         :: Spectral_l1b
     deallocate( Spectral_l1b%NsFft )
     deallocate( Spectral_l1b%OSFactor_zeropad )
     deallocate( Spectral_l1b%OSFactor_dephas )
     deallocate( Spectral_l1b%OSFactor_spline )
     deallocate( Spectral_l1b%SigS )
     deallocate( Spectral_l1b%WnS )
     deallocate( Spectral_l1b%WnE )
     deallocate( Spectral_l1b%dWn )
     return
   end subroutine dalloc_Spectral_L1b_mb
!
!
end module spectral_l1b_mb_type

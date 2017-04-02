!!* spectral_l1c_mb_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: april 2010
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> spectral_l1c_mb_type -- Module
!!
!! * Purpose
!!
!!   Module for type spectral l1c parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the spectral l1c parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Spectral_l1c_mb   : type for declaration and allocation of spectral l1c parameters
!!     * alloc_Spectral_l1c_mb  : type_Spectral_l1c_mb allocation
!!     * dalloc_Spectral_l1c_mb : type_Spectral_l1c_mb deallocation
!!
!! * References
!!


module spectral_l1c_mb_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Spectral_L1c_mb,  &
             alloc_Spectral_L1c_mb, &
             dalloc_Spectral_L1c_mb  
!
   type :: type_Spectral_L1c_mb
     character(len=500)                                  :: filename !< spectral l1c parameters characterisation file name
     integer(kind=LONG)                                  :: Nband !< spectral bands number
     character(len=6)     ,dimension(:)    ,allocatable  :: Mode !< apodisation mode 
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: NsFft !< Fourier transform samples number
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: SigS !< samples number of the smoothing function on the spectrum
     integer(kind=LONG)   ,dimension(:)    ,allocatable  :: SigI !< samples number of the smoothing function on the interferogram
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: WnS !< First wavenumber for each spectral band (m-1)
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: WnE !< Last wavenumber for each spectral band (m-1)
     real(kind=DOUBLE)    ,dimension(:)    ,allocatable  :: dWn !< spectral sampling (m-1)
   end type type_Spectral_L1c_mb
!
   contains
!
!
   subroutine alloc_Spectral_L1c_mb( Spectral_L1c )
   implicit none
     type(type_Spectral_L1c_mb), intent(inout)     :: Spectral_L1c
     integer(kind=LONG)                            :: ErrCode
     integer(kind=LONG)                            :: ioalloc
!
     ioalloc = 0
     allocate( Spectral_L1c%Mode(Spectral_L1c%Nband),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1c%NsFft(Spectral_L1c%Nband),    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1c%SigS(Spectral_L1c%Nband),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1c%SigI(Spectral_L1c%Nband),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1c%WnS(Spectral_L1c%Nband),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1c%WnE(Spectral_L1c%Nband),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spectral_L1c%dWn(Spectral_L1c%Nband),      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc > 0 ) then
        write(0,*) 'allocation Spectral_L1c Error',ioalloc
        write(0,*) 'Spectral_L1c_mb_type: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectral_L1c_mb
!
!
   subroutine dalloc_Spectral_L1c_mb( Spectral_L1c )
   implicit none
     type(type_Spectral_L1c_mb), intent(inout)         :: Spectral_L1c
     deallocate( Spectral_L1c%Mode )
     deallocate( Spectral_L1c%NsFft )
     deallocate( Spectral_L1c%SigS )
     deallocate( Spectral_L1c%SigI )
     deallocate( Spectral_L1c%WnS )
     deallocate( Spectral_L1c%WnE )
     deallocate( Spectral_L1c%dWn )
     return
   end subroutine dalloc_Spectral_L1c_mb
!
!
end module spectral_l1c_mb_type

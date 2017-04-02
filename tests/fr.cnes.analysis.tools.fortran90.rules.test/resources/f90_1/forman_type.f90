!!* forman_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.6 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> forman_type -- Module
!!
!! * Purpose
!!
!!   Module for type Forman parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the Forman parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Forman   : type for declaration and allocation of Psf
!!     * alloc_Forman  : type_Forman allocation
!!     * dalloc_Forman : type_Forman deallocation
!!
!! * References
!!


module forman_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Forman,  &
             alloc_Forman, &
             dalloc_Forman
!
   type :: type_Forman
     character(len=500)                               :: filename !< forman parameters characterisation file name
     character(len=2)                                 :: Type !< forman kernel type
     character(len=10)                                :: Mode_Traitement !< Mode de traitement (MESUREIASI ou SIMULATION)
     real(kind=DOUBLE)                                :: Ratio_bilatere !< Ratio Bilatere
     integer(kind=LONG)                               :: Ns_Fft !< Fourier transform samples number
     integer(kind=LONG)                               :: N_Usefull_Sample !< usefull samples number
     integer(kind=LONG)                               :: Ns_Usefull_First !< first usefull sample
     integer(kind=LONG)                               :: Ns_Usefull_Last !< last usefull sample
     real(kind=DOUBLE)                                :: Wn_Usefull_First !< first usefull wavenumber (m-1)
     real(kind=DOUBLE)                                :: Wn_Usefull_Last !< last usefull wavenumber (m-1)
     real(kind=DOUBLE)                                :: Wn_Ref !< Reference wavenumber (m-1)
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling (m-1)
     real(kind=DOUBLE)                                :: dOpd !< OPD sampling (m)
     integer(kind=LONG)                               :: SigK !< samples number of the smoothing function on the forman Kernel function 
     integer(kind=LONG)                               :: SigS !< samples number of the smoothing function on the spectrum
     integer(kind=LONG)                               :: SigI !< samples number of the smoothing function on the interferogram
     integer(kind=LONG)                               :: NZpd !< Interferogram pivot sample
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Wn_Usefull !< usefull wavenumber (m-1) - Wn_Usefull(1:Ns_Fft+1)
     complex(kind=DOUBLE),dimension(:)  ,allocatable  :: Sp_Usefull !< usefull spectrum - Sp_Usefull(1:Ns_Fft+1)
     complex(kind=DOUBLE),dimension(:)  ,allocatable  :: Sp_Reduced !< reduced spectrum - Sp_Reduced(1:Ns_Fft+1)
     real(kind=DOUBLE)   ,dimension(:)  ,allocatable  :: Kernel_R !< Forman kernel - Kernel(1:Ns_Fft+1)
     complex(kind=DOUBLE),dimension(:)  ,allocatable  :: Kernel_C !< Forman kernel - Kernel(1:Ns_Fft+1)
   end type type_Forman
!
   contains
!
!
   subroutine alloc_Forman( Forman )
   implicit none
     type(type_Forman), intent(inout)                    :: Forman
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
!
     ioalloc = 0
     allocate( Forman%Wn_Usefull(Forman%Ns_Fft+1),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Forman%Sp_Usefull(Forman%Ns_Fft+1),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Forman%Sp_Reduced(Forman%Ns_Fft+1),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( Forman%Type == 'R' ) then
       allocate( Forman%Kernel_R(Forman%Ns_Fft+1),    stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     else if( Forman%Type == 'C' ) then
       allocate( Forman%Kernel_C(Forman%Ns_Fft+1),    stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     else
       write(0,*) 'Forman Type Error must be R or C'
       write(0,*) 'Kernel not allocated'
       write(0,*) 'Forman: fatal error'
       call exit(1)
     end if
     if (ioalloc > 0) then
        write(0,*) 'allocation Error',ioalloc
        write(0,*) 'Forman: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Forman
!
!
   subroutine dalloc_Forman( Forman )
   implicit none
     type(type_Forman), intent(inout)                       :: Forman
!
     deallocate( Forman%Wn_Usefull )
     deallocate( Forman%Sp_Usefull )
     deallocate( Forman%Sp_Reduced )
     if( Forman%Type == 'R' ) then
       deallocate( Forman%Kernel_R )
     else if( Forman%Type == 'C' ) then
       deallocate( Forman%Kernel_C )
     end if
!
     return
   end subroutine dalloc_Forman
!
!
end module forman_type

!!* general_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.2 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> general_type -- Module
!!
!! * Purpose
!!
!!   Module for general type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the general type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Cnv_Fct    : type for declaration and allocation of convolution function parameters
!!     * alloc_Cnv_Fct   : type_Cnv_Fct allocation
!!     * dalloc_Cnv_Fct  : type_Cnv_Fct deallocation
!!     * Cnv_Fct_Basis   : computes convolution function spectral base in wavenumber
!!
!! * References
!!


module general_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Cnv_Fct    &
            ,alloc_Cnv_Fct   &
            ,dalloc_Cnv_Fct  &
            ,Cnv_Fct_Basis
!
   type :: type_Cnv_Fct
     character(len=500)                               :: filename !< convolution function parameters characterisation file name
     integer(kind=LONG)                               :: N_Sample !< spectral samples number
     integer(kind=LONG)                               :: Ns_Opd !< OPD samples number
     real(kind=DOUBLE)                                :: FullSDWn !< Full spectral domain wavenumber (m-1)  
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling (m-1)
     real(kind=DOUBLE)                                :: FWHM !< Full Width at Half Maximum 
     real(kind=DOUBLE)                                :: WnShift !< spectral shift (m-1)
     real(kind=DOUBLE)                                :: dOpd !< OPD sampling (m)
     real(kind=DOUBLE)                                :: OpdMax !< OPD maximum (m)
     real(kind=DOUBLE)                                :: Opd_effective !< OPD effective (m)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: Wn !< spectral base (m-1) - Wn(1:N_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: Fct !< convolution function - Fct(1:N_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: Opd ! OPD base (m) - Opd(1:Ns_Opd)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: Fct_TF !< convolution function Fourier transform - Fct_TF(1:Ns_Opd)
   end type type_Cnv_Fct
!
!
   contains
!
!
   subroutine alloc_Cnv_Fct( Cnv_Fct )
   implicit none
     type(type_Cnv_Fct)    , intent(inout)      :: Cnv_Fct
     integer(kind=LONG)                         :: ErrCode
     integer(kind=LONG)                         :: ios
!
     ios = 0
     allocate( Cnv_Fct%Wn(Cnv_Fct%N_Sample),    stat=ErrCode )
     if( ErrCode /= 0 ) ios = ios + 1
     allocate( Cnv_Fct%Fct(Cnv_Fct%N_Sample),   stat=ErrCode )
     if( ErrCode /= 0 ) ios = ios + 1
     allocate( Cnv_Fct%Opd(Cnv_Fct%Ns_Opd),     stat=ErrCode )
     if( ErrCode /= 0 ) ios = ios + 1
     allocate( Cnv_Fct%Fct_TF(Cnv_Fct%Ns_Opd),  stat=ErrCode )
     if( ErrCode /= 0 ) ios = ios + 1
!
     if (ios > 0) then
        write(0,*) 'allocation Cnv_Fct Error',ios
        write(0,*) 'Cnv_Fct: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Cnv_Fct
!
!
   subroutine dalloc_Cnv_Fct( Cnv_Fct )
   implicit none
     type(type_Cnv_Fct)    , intent(inout)         :: Cnv_Fct
!
     deallocate( Cnv_Fct%Wn )
     deallocate( Cnv_Fct%Fct )
     deallocate( Cnv_Fct%Opd )
     deallocate( Cnv_Fct%Fct_TF )
!
     return
   end subroutine dalloc_Cnv_Fct
!
!
   subroutine Cnv_Fct_Basis( Cnv_Fct )
   implicit none
     type(type_Cnv_Fct)    , intent(inout)            :: Cnv_Fct
     integer(kind=LONG)                               :: N_Sample0
     integer(kind=LONG)                               :: Ns
!
     N_Sample0 = int( Cnv_Fct%N_Sample/2 ) + 1
     Cnv_Fct%Wn(1:Cnv_Fct%N_Sample) = &
              (/ (dble(Ns-N_Sample0),Ns=1,Cnv_Fct%N_Sample) /) * Cnv_Fct%dWn
     N_Sample0 = int( Cnv_Fct%Ns_Opd/2 ) + 1
     Cnv_Fct%Opd(1:Cnv_Fct%Ns_Opd) = &
              (/ (dble(Ns-N_Sample0),Ns=1,Cnv_Fct%Ns_Opd) /) * Cnv_Fct%dOpd
!
     return
   end subroutine Cnv_Fct_Basis
!
!
end module general_type

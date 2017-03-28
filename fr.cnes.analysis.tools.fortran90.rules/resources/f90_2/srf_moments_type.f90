!!* srf_moments_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: may 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-06-22 10:06:27 $
!!
!> srf_monents_type -- Module
!!
!! * Purpose
!!
!!   Module for type srf moments declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the srf_moment type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Srf_moments  : type for declaration and allocation of Srf_moments
!!     * alloc_Srf_moments : type_Srf_moments allocation
!!     * dalloc_Srf_moments: type_Srf_moments deallocation
!
!
module srf_moments_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Srf_Moments      &
            ,alloc_Srf_Moments     &
            ,dalloc_Srf_Moments
!
   type :: type_Srf_Moments
     character(len=500)                                :: filename !< file name
     integer(kind=LONG)                                :: N_Moments
     integer(kind=LONG)                                :: NsWn0
     real(kind=DOUBLE)     ,dimension(:)  ,allocatable :: Wn0
     complex(kind=DOUBLE)  ,dimension(:,:),allocatable :: Moments
     complex(kind=DOUBLE)  ,dimension(:,:),allocatable :: Derive0
   end type type_Srf_Moments
!
   contains
!
!
   subroutine alloc_Srf_Moments( Srf_Moments )
   implicit none
     type(type_Srf_Moments)    , intent(inout)         :: Srf_Moments
     integer(kind=LONG)                                :: ErrCode
     allocate( Srf_Moments%Wn0(Srf_Moments%NsWn0), stat=ErrCode )
     allocate( Srf_Moments%Moments(Srf_Moments%NsWn0,0:Srf_Moments%N_Moments),&
                                                                stat=ErrCode )
     allocate( Srf_Moments%Derive0(Srf_Moments%NsWn0,0:Srf_Moments%N_Moments),&
                                                                stat=ErrCode )
     return
   end subroutine alloc_Srf_Moments
!
!
   subroutine dalloc_Srf_Moments( Srf_Moments )
   implicit none
     type(type_Srf_Moments)    , intent(inout)         :: Srf_Moments
     deallocate( Srf_Moments%Wn0 )
     deallocate( Srf_Moments%Moments )
     deallocate( Srf_Moments%Derive0 )
     return
   end subroutine dalloc_Srf_Moments
!
!
end module srf_moments_type

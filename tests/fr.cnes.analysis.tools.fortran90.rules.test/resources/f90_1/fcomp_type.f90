!!* fcomp_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER-CS
!!*              Date: January 2010
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-03-07 15:54:59 $
!!
!> fcomp_type -- Module
!!
!! * Purpose
!!
!!   Module for type field compensation declaration.
!!
!! * Description
!!
!!     This module defines the field compensation type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Fcomp   : type for declaration and allocation of field compensation
!!     * alloc_Fcomp  : type_Fcomp allocation
!!     * dalloc_Fcomp : type_Fcomp deallocation
!!
!! * References
!!

module fcomp_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Fcomp,  &
             alloc_Fcomp, &
             dalloc_Fcomp
!
   type :: type_Fcomp
     character(len=500)                               :: filename !< field compensation characterisation file name 
     real(kind=DOUBLE)                                :: DX !< Constant mis-synchronisation of field compensation with ZPD (in m) in metric units (OPD/2)ff
     real(kind=DOUBLE)                                :: DA !< Field compensation amplitude fraction (with respect to nominal)
     real(kind=DOUBLE)                                :: DA_Jitter !< Field compensation amplitude fraction jitter
     integer(kind=LONG)                               :: PN_Cen_Mes !< Pixel at the center of measurement
     integer(kind=LONG)                               :: PN_Cen_Rpd !< Pixel at the center of the RPD laser beam 
     integer(kind=LONG)                               :: PN_Cen !< Central pixel for which the field compensation is not required
     integer(kind=LONG)                               :: NsOpd !< OPD samples number
     real(kind=DOUBLE)  ,dimension(:) ,allocatable    :: DA_Jitter_Vect !< Vector of field compensation amplitude fraction jitter (value for each OPD samples number) - DA_Jitter_Vect(1:NsOpd)
     character(len=4)                                 :: Material !< wedge material for refractive index computation
     real(kind=DOUBLE)  ,dimension(:,:) ,allocatable  :: Theta ! field angle
   end type type_Fcomp
!
   contains
!
!
   subroutine alloc_Fcomp( Fcomp )
   implicit none
     type(type_Fcomp)  , intent(inout)                  :: Fcomp
     integer(kind=LONG)                                 :: ErrCode
!
!    allocation
     allocate( Fcomp%DA_Jitter_Vect(Fcomp%NsOpd), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'Fcomp allocation Error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Fcomp
!
!
   subroutine dalloc_Fcomp( Fcomp )
   implicit none
     type(type_Fcomp)  , intent(inout)                  :: Fcomp
!
!    deallocation
     deallocate( Fcomp%DA_Jitter_Vect )
!
     return
   end subroutine dalloc_Fcomp
!
!
end module fcomp_type

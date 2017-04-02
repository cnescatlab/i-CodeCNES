!!* refrac_index_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> refrac_index_type -- Module
!!
!! * Purpose
!!
!!      Module for refractive index declaration and allocation.
!!
!! * Description
!!      
!!      This module defines the refractive index of materials type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_RefracIndex : type for declaration and allocation of refractive index of materials
!!
!! * References
!!

module refrac_index_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_RefracIndex,  &
             alloc_RefracIndex, &
             dalloc_RefracIndex


   type :: type_RefracIndex
     character(len=500)                                :: filename !< material refractive index characterisation file name- if matrix index 1 = ZnSe, Zinc Selenide refractive index characterisation - if matrix index 2 = KBr, Potassium Bromide refractive index characterisation
     real(kind=DOUBLE)    ,dimension(2)                :: C !< constant Sellmeier coefficient for material refractive index computation 
     integer(kind=SIMPLE) ,dimension(2)                :: NCoef !< number of Sellmeier coefficient for material refractive index computation
     real(kind=DOUBLE)    ,dimension(5,2)              :: A !< first Sellmeier coefficient for material refractive index computation
     real(kind=DOUBLE)    ,dimension(5,2)              :: B !< second Sellmeier coefficient for material refractive index computation
     real(kind=DOUBLE)                                 :: N !< material refractive index a wave number
     integer(kind=SIMPLE)                              :: NbCol !< column number 
     integer(kind=SIMPLE)                              :: NbLin !< line number 
     real(kind=DOUBLE)    ,dimension(:,:), allocatable :: Nper !< material refractive index on the field for a wave number, it includes defects
     real(kind=DOUBLE)    ,dimension(:,:), allocatable :: FracN !< refractive index defect, fraction of N
     real(kind=DOUBLE)                                 :: MeanRefracIndex !< Mean refractive index over the field
  end type type_RefracIndex
!
!
   contains
!
!
   subroutine alloc_RefracIndex( RefracIndex )
     implicit none
     type(type_RefracIndex) ,intent(inout)             :: RefracIndex
     integer(kind=LONG)                                :: ErrCode
     allocate( RefracIndex%Nper(RefracIndex%NbCol,RefracIndex%NbLin), &
                                                         stat=ErrCode )
     if ( ErrCode /= 0 ) then
        write(0,*) 'alloc_RefracIndex Error'
        call exit(1)
     end if
     allocate( RefracIndex%FracN(RefracIndex%NbCol,RefracIndex%NbLin),&
                                                         stat=ErrCode )
     if ( ErrCode /= 0 ) then
        write(0,*) 'alloc_RefracIndex Error'
        call exit(1)
     end if
     return
   end subroutine alloc_RefracIndex
!
!
   subroutine dalloc_RefracIndex( RefracIndex )
     implicit none
     type(type_RefracIndex) ,intent(inout)             :: RefracIndex
     deallocate( RefracIndex%Nper )
     deallocate( RefracIndex%FracN )
     return
   end subroutine dalloc_RefracIndex
!
!
end module refrac_index_type

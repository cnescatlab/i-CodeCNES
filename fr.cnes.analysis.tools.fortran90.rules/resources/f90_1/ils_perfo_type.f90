!!* ils_perfo_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> ils_perfo_type -- Module
!!
!! * Purpose
!!
!!   Module for type spectral performance parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the spectral performance parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Ils_Perfo   : type for declaration and allocation of spectral performance parameters
!!     * alloc_Ils_Perfo  : type_Ils_Perfo allocation
!!     * dalloc_Ils_Perfo : type_Ils_Perfo deallocation
!!
!! * References
!!


module ils_perfo_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Ils_Perfo    &
            ,alloc_Ils_Perfo   &
            ,dalloc_Ils_Perfo  
!
   type :: type_Ils_Perfo
     character(len=500)                               :: filename !< spectral performance parameters characterisation file name
     real(kind=DOUBLE)                                :: WnIls !< ILS wavenumber (m-1)
     integer(kind=LONG)                               :: Ip !< flag for barycenter computation (linear/quadratic)  
     integer(kind=LONG)                               :: Iteration !< barycenter iteration number  
     integer(kind=LONG)                               :: OSFactor !< oversampling factor
     integer(kind=LONG)                               :: Ns_Ils !< ILS samples number
     real(kind=DOUBLE)                                :: SDWn !< Spectral domain wavenumber (m-1)  
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling (m-1)
     real(kind=DOUBLE)                                :: Shift !< spectral shift (m-1)
     real(kind=DOUBLE)                                :: ErShift !< shift error
     real(kind=DOUBLE)                                :: FWhm !< Full Width at Half Maximum 
     real(kind=DOUBLE)                                :: ErFWhm !< Resolution error 
     real(kind=DOUBLE)                                :: Shape_R !< Shape Index real part
     real(kind=DOUBLE)                                :: Shape_I !< Shape Index imaginary part
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Wn !< spectral base (m-1) - Wn(1:Ns_Ils)
     complex(kind=DOUBLE) ,dimension(:) ,allocatable  :: Ils ! ILS function - Ils(1:Ns_Ils)
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_R !< Shape Error Index real part - ShapeDiff_R(1:Ns_Ils)
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_I !< Shape Error Index imaginary part - ShapeDiff_I(1:Ns_Ils)
   end type type_Ils_Perfo
!
   contains
!
!
   subroutine alloc_Ils_Perfo( Ils_Perfo )
   implicit none
     type(type_Ils_Perfo), intent(inout)           :: Ils_Perfo
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Ils_Perfo%Wn(Ils_Perfo%Ns_Ils),         stat=ErrCode )
     allocate( Ils_Perfo%Ils(Ils_Perfo%Ns_Ils),        stat=ErrCode )
     allocate( Ils_Perfo%ShapeDiff_R(Ils_Perfo%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo%ShapeDiff_I(Ils_Perfo%Ns_Ils),stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Ils_Perfo Error'
        write(0,*) 'Ils_Perfo: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Ils_Perfo
!
!
   subroutine dalloc_Ils_Perfo( Ils_Perfo )
   implicit none
     type(type_Ils_Perfo), intent(inout)         :: Ils_Perfo
!
     deallocate( Ils_Perfo%Wn )
     deallocate( Ils_Perfo%Ils )
     deallocate( Ils_Perfo%ShapeDiff_R )
     deallocate( Ils_Perfo%ShapeDiff_I )
     return
   end subroutine dalloc_Ils_Perfo
!
!
end module ils_perfo_type

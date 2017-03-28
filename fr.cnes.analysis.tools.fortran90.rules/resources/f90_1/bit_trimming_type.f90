!!* bit_trimming_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: may 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-06-22 10:06:27 $
!!
!> bit_trimming_type -- Module
!!
!! * Purpose
!!
!!   Module for type bit_trimming parameters declaration and allocation.
!!
!! * Sub-routines and functions
!!
!!     * type_Bit_trimming : type for declaration and allocation of bit_trimming parameters 
!!
!! * References
!!

module bit_trimming_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Bit_Trimming,  &
             alloc_Bit_Trimming, &
             dalloc_Bit_Trimming
!
   type :: type_Bit_Trimming
     character(len=500)  :: filename !< bit_trimming characterisation file name
     integer(kind=LONG)                           :: Nb_Bits !< quantification
     integer(kind=LONG)                           :: N_Sample !< Sample number of the template
     real(kind=DOUBLE) ,dimension(:),allocatable  :: V_Min !< minimum value of the signal (Volts)
     real(kind=DOUBLE) ,dimension(:),allocatable  :: V_Max !< maximum value of the signal(Volts)
   end type type_Bit_Trimming
!
!
   contains
!
!  
   subroutine alloc_Bit_Trimming( Bit_Trimming )
   implicit none
     type(type_Bit_Trimming) ,intent(inout)       :: Bit_Trimming
     integer(kind=LONG)                           :: ErrCode
!
     allocate( Bit_Trimming%V_Min( Bit_Trimming%N_Sample) , stat=ErrCode )
     allocate( Bit_Trimming%V_Max( Bit_Trimming%N_Sample) , stat=ErrCode )
!
     return
   end subroutine alloc_Bit_Trimming
!
!  
   subroutine dalloc_Bit_Trimming( Bit_Trimming )
   implicit none
     type(type_Bit_Trimming) ,intent(inout)       :: Bit_Trimming
!
     deallocate( Bit_Trimming%V_Min )
     deallocate( Bit_Trimming%V_Max )
!
     return
   end subroutine dalloc_Bit_Trimming
!
!
end module bit_trimming_type

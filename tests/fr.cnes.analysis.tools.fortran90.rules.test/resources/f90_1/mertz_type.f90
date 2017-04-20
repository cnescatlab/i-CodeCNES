!!* mertz_type.f90 --
!!* 
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*               Date: january 2011
!!*            Version: $Revision: 1.1 $
!!*  Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> mertz_type -- Module
!!
!! * Purpose
!!
!!   Module for mertz system declaration.
!!
!! * Description
!!
!!     type for declaration and allocation of detection simulation
!!
!! * Sub-routines and functions
!!
!!     * type_Mertz : type for declaration and allocation of mertz parameters
!!
!! * References
!!

module mertz_type
  use precision_type
!
  implicit none
!
!
  public :: type_Mertz,  &
            alloc_Mertz, &
            dalloc_Mertz
!
  type :: type_Mertz
     integer(kind=SIMPLE)                         :: NsCcm !< sample number
     real(kind=DOUBLE) ,dimension(:),allocatable  :: Dthickness !< thickness defect in MERTZ configuration.
  end type type_Mertz
!
!
   contains
!
!
   subroutine alloc_Mertz( Mertz )
     implicit none
     type(type_Mertz) ,intent(inout)                    :: Mertz
     integer(kind=LONG)                                 :: ErrCode
     allocate( Mertz%Dthickness(Mertz%NsCcm), stat=ErrCode )
     if ( ErrCode /= 0 ) then
        write(0,*) 'alloc_Mertz Error'
        call exit(1)
     end if
     return
   end subroutine alloc_Mertz
!
!
   subroutine dalloc_Mertz( Mertz )
     implicit none
     type(type_Mertz) ,intent(inout)                    :: Mertz
     deallocate( Mertz%Dthickness )
     return
   end subroutine dalloc_Mertz
!
!
end module mertz_type

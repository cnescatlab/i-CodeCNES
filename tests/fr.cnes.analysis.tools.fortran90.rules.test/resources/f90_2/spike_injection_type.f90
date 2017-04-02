!!* spike_injection_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: march 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-04-12 08:32:32 $
!!
!> spike_injection_type -- Module
!!
!! * Purpose
!!
!!   Module for type Spike_Injection parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the Spike Injection parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Spike_Injection   : type for declaration and allocation
!!     * alloc_Spike_Injection  : type_Spike_Injection allocation
!!     * dalloc_Spike_Injection : type_Spike_Injection deallocation
!!
!! * References
!!


module spike_injection_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Spike_Injection,  &
             alloc_Spike_Injection, &
             dalloc_Spike_Injection
!
   type :: type_Spike_Injection
     character(len=500)                               :: filename  !< spike parametres file name  
     integer(kind=LONG)                               :: NSpike    !< number of spike to inject into interferogram
     real(kind=DOUBLE)  ,dimension(:), allocatable    :: Amplitude !< amplitude of spike - Amplitude(NSpike)
     real(kind=DOUBLE)  ,dimension(:), allocatable    :: Duration  !< duration of spike - Duration(NSpike) [s]
   end type type_Spike_Injection
!
   contains
!
!
   subroutine alloc_Spike_Injection( Spike_Injection )
   implicit none
     type(type_Spike_Injection), intent(inout)           :: Spike_Injection
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
!
     ioalloc = 0
     allocate( Spike_Injection%Amplitude(Spike_Injection%NSpike), &
                                                     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Spike_Injection%Duration(Spike_Injection%NSpike), &
                                                    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if (ioalloc > 0) then
        write(0,*) 'allocation Error',ioalloc
        write(0,*) 'Spike_Injection: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Spike_Injection
!
!
   subroutine dalloc_Spike_Injection( Spike_Injection )
   implicit none
     type(type_Spike_Injection), intent(inout)            :: Spike_Injection
!
     deallocate( Spike_Injection%Amplitude )
     deallocate( Spike_Injection%Duration )
!
     return
   end subroutine dalloc_Spike_Injection
!
!
end module spike_injection_type

!!* emissivity_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: april 2011
!!*           Version: $Revision: 1.2 $
!!* Last modification: $Date: 2011-11-02 14:57:53 $
!!
!> type_emissivity -- Module
!!
!! * Purpose
!!
!!   Module for type emissivity parameters declaration and allocation.
!!
!! * Description
!! * References
!!     SPS ATBD

module emissivity_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Emissivity ,  &
             alloc_Emissivity,  &
             dalloc_Emissivity
!
   type :: type_Emissivity
     character(len=500)   :: filename !< Emissivity characterisation file name
     integer(kind=LONG)   :: NsWn0    !< number of spectral samples
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Wn0    !< Wave numbers
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Mod_BG1 !< BG1 Emissivity
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Mod_BG2 !< BG2 Emissivity
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Dir_BG !< BG Emissivity
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Fct_CS !< CS Emissivity
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Fct_BB !< BB Emissivity
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Fct_EW !< EW Emissivity
   end type type_Emissivity
!
!
   contains
!
!
   subroutine alloc_Emissivity( Emissivity )
   implicit none
     type(type_Emissivity), intent(inout)         :: Emissivity
     integer(kind=LONG)                           :: ErrCode
     integer(kind=LONG)                           :: ioalloc
!
     ioalloc = 0
     allocate( Emissivity%Wn0(Emissivity%NsWn0)    , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Emissivity%Mod_BG1(Emissivity%NsWn0) , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Emissivity%Mod_BG2(Emissivity%NsWn0) , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Emissivity%Dir_BG(Emissivity%NsWn0) , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Emissivity%Fct_CS(Emissivity%NsWn0) , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Emissivity%Fct_BB(Emissivity%NsWn0) , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Emissivity%Fct_EW(Emissivity%NsWn0) , stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
!
     if( ioalloc > 0 ) then
        write(*,*) 'allocation Emissivity Error', ErrCode
        write(*,*) 'type_Emissivity: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Emissivity
!
!
   subroutine dalloc_Emissivity( Emissivity )
   implicit none
     type(type_Emissivity), intent(inout) :: Emissivity
!
     deallocate( Emissivity%Wn0 )
     deallocate( Emissivity%Mod_BG1 )
     deallocate( Emissivity%Mod_BG2 )
     deallocate( Emissivity%Dir_BG )
     deallocate( Emissivity%Fct_CS )
     deallocate( Emissivity%Fct_BB )
     deallocate( Emissivity%Fct_EW )
     return
   end subroutine dalloc_Emissivity
!
!
 end module emissivity_type

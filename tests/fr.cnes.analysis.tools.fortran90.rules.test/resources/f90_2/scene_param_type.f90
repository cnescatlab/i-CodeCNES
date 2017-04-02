!!* scene_param_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: july 2010
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-03-14 10:39:56 $
!!
!> scene_param_type -- Module
!!
!! * Purpose
!!
!!   Module for scene parameters type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the scene parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Scene_Param   : type for declaration and allocation of Resampling_Scene_Param
!!     * alloc_Scene_Param  : type_Scene_Param allocation
!!     * dalloc_Scene_Param : type_Scene_Param deallocation
!!
!! * References
!!



module scene_param_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Scene_Param,  &
             alloc_Scene_Param, &
             dalloc_Scene_Param  
!
   type :: type_Scene_Param
     character(len=500)                               :: filename !< scene parameters characterisation file name
     integer(kind=LONG)                               :: NbScene !< scenes number
     integer(kind=LONG),dimension(:)    ,allocatable  :: Scene_List !< high resolution spectrum index - Scene_List(1:NbScene)
     character(len=500),dimension(:)    ,allocatable  :: hr_filename !< high resolution spectrum file name - hr_filename(1:NbScene)
     integer(kind=LONG)                               :: NbSubPixel !< sub-pixels number
     integer(kind=LONG),dimension(:)    ,allocatable  :: N_Spectre_1 !< N_Spectre_1(1:NbSubPixel)
     integer(kind=LONG),dimension(:)    ,allocatable  :: N_Spectre_2 !< N_Spectre_2(1:NbSubPixel)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Frac_1 !< Fraction I1 at NZPD - Frac_1(1:NbSubPixel)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: AmpFracJit !< Jitter amplitude (fraction)' - AmpFracJit(1:NbSubPixel)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: FreqJit !< Jitter frequency (Hz) - FreqJit(1:NbSubPixel)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: PhasJit !< Jitter phase at NZPD (deg) - PhasJit(1:NbSubPixel)
   end type type_Scene_Param
!
   contains
!
!
   subroutine alloc_Scene_Param( Scene_Param )
   implicit none
     type(type_Scene_Param), intent(inout)       :: Scene_Param
     integer(kind=LONG)                          :: ErrCode
     integer(kind=LONG)                          :: ioalloc
!
     ioalloc = 0
     allocate( Scene_Param%hr_filename(Scene_Param%NbScene),    stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%Scene_List(Scene_Param%NbScene),    stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%N_Spectre_1(Scene_Param%NbSubPixel), stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%N_Spectre_2(Scene_Param%NbSubPixel), stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%Frac_1(Scene_Param%NbSubPixel),      stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%AmpFracJit(Scene_Param%NbSubPixel),  stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%FreqJit(Scene_Param%NbSubPixel),     stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
     allocate( Scene_Param%PhasJit(Scene_Param%NbSubPixel),     stat=ErrCode )
     if (ErrCode /= 0) ioalloc = ioalloc +1
!
     if (ioalloc > 0) then
        write(0,*) 'allocation Scene_Param Error',ioalloc
        write(0,*) 'Scene_Param: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Scene_Param
!
!
   subroutine dalloc_Scene_Param( Scene_Param )
   implicit none
     type(type_Scene_Param), intent(inout)         :: Scene_Param
!
     deallocate( Scene_Param%hr_filename )
     deallocate( Scene_Param%Scene_List )
     deallocate( Scene_Param%N_Spectre_1 )
     deallocate( Scene_Param%N_Spectre_2 )
     deallocate( Scene_Param%Frac_1 )
     deallocate( Scene_Param%AmpFracJit )
     deallocate( Scene_Param%FreqJit )
     deallocate( Scene_Param%PhasJit )
     return
   end subroutine dalloc_Scene_Param
!
!
end module scene_param_type

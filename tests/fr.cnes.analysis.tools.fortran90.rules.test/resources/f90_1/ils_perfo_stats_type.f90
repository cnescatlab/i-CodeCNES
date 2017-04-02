!!* ils_perfo_stats_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> ils_perfo_stats_type -- Module
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
!!     * type_Ils_Perfo_Stats   : type for declaration and allocation of spectral performance parameters
!!     * alloc_Ils_Perfo_Stats  : type_Ils_Perfo_Stats allocation
!!     * dalloc_Ils_Perfo_Stats : type_Ils_Perfo_Stats deallocation
!!
!! * References
!!


module ils_perfo_stats_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Ils_Perfo_Stats    &
            ,alloc_Ils_Perfo_Stats   &
            ,dalloc_Ils_Perfo_Stats  
!
   type :: type_Ils_Perfo_Stats
     character(len=500)                               :: filename !< spectral performance parameters characterisation file name
     real(kind=DOUBLE)                                :: WnIls !< ILS wavenumber (m-1)
     integer(kind=LONG)                               :: Ip !< flag for barycenter computation (linear/quadratic)  
     integer(kind=LONG)                               :: Iteration !< barycenter iteration number  
     integer(kind=LONG)                               :: OSFactor !< oversampling factor
     integer(kind=LONG)                               :: Ns_Ils !< ILS samples number
     real(kind=DOUBLE)                                :: SDWn !< Spectral domain wavenumber (m-1)  
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling (m-1)
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Wn !< spectral base (m-1) - Wn(1:Ns_Ils)
     real(kind=DOUBLE)                                :: Shift_Avg 
     real(kind=DOUBLE)                                :: Shift_Std
     real(kind=DOUBLE)                                :: Shift_Min
     real(kind=DOUBLE)                                :: Shift_Max
     integer(kind=LONG)                               :: Shift_NsMin
     integer(kind=LONG)                               :: Shift_NsMax
     real(kind=DOUBLE)                                :: ErShift_Avg 
     real(kind=DOUBLE)                                :: ErShift_Std
     real(kind=DOUBLE)                                :: ErShift_Min
     real(kind=DOUBLE)                                :: ErShift_Max
     integer(kind=LONG)                               :: ErShift_NsMin
     integer(kind=LONG)                               :: ErShift_NsMax
     real(kind=DOUBLE)                                :: FWhm_Avg 
     real(kind=DOUBLE)                                :: FWhm_Std
     real(kind=DOUBLE)                                :: FWhm_Min
     real(kind=DOUBLE)                                :: FWhm_Max
     integer(kind=LONG)                               :: FWhm_NsMin
     integer(kind=LONG)                               :: FWhm_NsMax
     real(kind=DOUBLE)                                :: ErFWhm_Avg 
     real(kind=DOUBLE)                                :: ErFWhm_Std
     real(kind=DOUBLE)                                :: ErFWhm_Min
     real(kind=DOUBLE)                                :: ErFWhm_Max
     integer(kind=LONG)                               :: ErFWhm_NsMin
     integer(kind=LONG)                               :: ErFWhm_NsMax
     real(kind=DOUBLE)                                :: Shape_Index_R_Avg 
     real(kind=DOUBLE)                                :: Shape_Index_R_Std
     real(kind=DOUBLE)                                :: Shape_Index_R_Min
     real(kind=DOUBLE)                                :: Shape_Index_R_Max
     integer(kind=LONG)                               :: Shape_Index_R_NsMin
     integer(kind=LONG)                               :: Shape_Index_R_NsMax
     real(kind=DOUBLE)                                :: Shape_Index_I_Avg 
     real(kind=DOUBLE)                                :: Shape_Index_I_Std
     real(kind=DOUBLE)                                :: Shape_Index_I_Min
     real(kind=DOUBLE)                                :: Shape_Index_I_Max
     integer(kind=LONG)                               :: Shape_Index_I_NsMin
     integer(kind=LONG)                               :: Shape_Index_I_NsMax
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_R_Avg
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_R_Std
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_R_Min
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_R_Max
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: Shape_R_NsMin
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: Shape_R_NsMax
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_I_Avg
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_I_Std
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_I_Min
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: Shape_I_Max
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: Shape_I_NsMin
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: Shape_I_NsMax
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_R_Avg
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_R_Std
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_R_Min
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_R_Max
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: ShapeDiff_R_NsMin
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: ShapeDiff_R_NsMax
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_I_Avg
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_I_Std
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_I_Min
     real(kind=DOUBLE)    ,dimension(:) ,allocatable  :: ShapeDiff_I_Max
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: ShapeDiff_I_NsMin
     integer(kind=LONG)   ,dimension(:) ,allocatable  :: ShapeDiff_I_NsMax

   end type type_Ils_Perfo_Stats
!
   contains
!
!
   subroutine alloc_Ils_Perfo_Stats( Ils_Perfo_Stats )
   implicit none
     type(type_Ils_Perfo_Stats), intent(inout)           :: Ils_Perfo_Stats
     integer(kind=LONG)                                  :: ErrCode
!
     allocate( Ils_Perfo_Stats%Wn(Ils_Perfo_Stats%Ns_Ils),           stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_R_Avg(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_R_Std(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_R_Min(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_R_Max(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_R_NsMin(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_R_NsMax(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_I_Avg(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_I_Std(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_I_Min(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_I_Max(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_I_NsMin(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%Shape_I_NsMax(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_R_Avg(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_R_Std(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_R_Min(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_R_Max(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_R_NsMin(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_R_NsMax(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_I_Avg(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_I_Std(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_I_Min(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_I_Max(Ils_Perfo_Stats%Ns_Ils),  stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_I_NsMin(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )
     allocate( Ils_Perfo_Stats%ShapeDiff_I_NsMax(Ils_Perfo_Stats%Ns_Ils),stat=ErrCode )

     return
   end subroutine alloc_Ils_Perfo_Stats
!
!
   subroutine dalloc_Ils_Perfo_Stats( Ils_Perfo_Stats )
   implicit none
     type(type_Ils_Perfo_Stats), intent(inout)         :: Ils_Perfo_Stats
!
     deallocate( Ils_Perfo_Stats%Wn            )
     deallocate( Ils_Perfo_Stats%Shape_R_Avg   )
     deallocate( Ils_Perfo_Stats%Shape_R_Std   )
     deallocate( Ils_Perfo_Stats%Shape_R_Min   )
     deallocate( Ils_Perfo_Stats%Shape_R_Max   )
     deallocate( Ils_Perfo_Stats%Shape_R_NsMin )
     deallocate( Ils_Perfo_Stats%Shape_R_NsMax )
     deallocate( Ils_Perfo_Stats%Shape_I_Avg   )
     deallocate( Ils_Perfo_Stats%Shape_I_Std   )
     deallocate( Ils_Perfo_Stats%Shape_I_Min   )
     deallocate( Ils_Perfo_Stats%Shape_I_Max   )
     deallocate( Ils_Perfo_Stats%Shape_I_NsMin )
     deallocate( Ils_Perfo_Stats%Shape_I_NsMax )
     deallocate( Ils_Perfo_Stats%ShapeDiff_R_Avg   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_R_Std   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_R_Min   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_R_Max   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_R_NsMin )
     deallocate( Ils_Perfo_Stats%ShapeDiff_R_NsMax )
     deallocate( Ils_Perfo_Stats%ShapeDiff_I_Avg   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_I_Std   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_I_Min   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_I_Max   )
     deallocate( Ils_Perfo_Stats%ShapeDiff_I_NsMin )
     deallocate( Ils_Perfo_Stats%ShapeDiff_I_NsMax )

     return
   end subroutine dalloc_Ils_Perfo_Stats
!
!
   subroutine readils_perfo_stats( Ils_Perfo_Stats, iostatus )
   implicit none
     type(type_Ils_Perfo_Stats), intent(inout):: Ils_Perfo_Stats
     integer(kind=LONG)       , intent(out)   :: iostatus
!
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
      iostatus = 0
      call open_file_r(trim(Ils_Perfo_Stats%filename)      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%WnIls, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%Ip, %VAL(Type),       &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Iteration, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%OSFactor, %VAL(Type), &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Ns_Ils, %VAL(Type),   &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%SDWn, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%dWn, %VAL(Type), &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Ils_Perfo_Stats( Ils_Perfo_Stats )
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%Wn, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%Shift_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shift_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shift_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shift_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%Shift_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shift_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%ErShift_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErShift_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErShift_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErShift_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%ErShift_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErShift_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%FWhm_Avg, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%FWhm_Std, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%FWhm_Min, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%FWhm_Max, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%FWhm_NsMin, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%FWhm_NsMax, %VAL(Type),  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%ErFWhm_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErFWhm_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErFWhm_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErFWhm_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%ErFWhm_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ErFWhm_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_R_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_R_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_I_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_Index_I_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%Shape_R_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_R_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_R_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_R_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%Shape_R_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_R_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%Shape_I_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_I_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_I_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_I_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%Shape_I_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%Shape_I_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Avg, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Std, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Min, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Max, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_NsMin, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_NsMax, %VAL(Type),&
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
      end if
   return
   end subroutine readils_perfo_stats
!
!
   subroutine writeils_perfo_stats( Ils_Perfo_Stats, iostatus )
   implicit none
     type(type_Ils_Perfo_Stats), intent(inout) :: Ils_Perfo_Stats
     integer(kind=LONG)        , intent(out)   :: iostatus
!
     integer(kind=DOUBLE)                      :: ifile
     integer(kind=LONG)                        :: offset
     integer(kind=LONG)                        :: Type
     integer(kind=LONG)                        :: Size
!
      iostatus = 0
      call open_file_w(trim(Ils_Perfo_Stats%filename)      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%WnIls, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%Ip, %VAL(Type),       &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Iteration, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%OSFactor, %VAL(Type), &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Ns_Ils, %VAL(Type),   &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%SDWn, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%dWn, %VAL(Type), &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%Wn, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%Shift_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shift_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shift_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shift_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%Shift_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shift_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%ErShift_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErShift_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErShift_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErShift_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%ErShift_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErShift_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%FWhm_Avg, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%FWhm_Std, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%FWhm_Min, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%FWhm_Max, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%FWhm_NsMin, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%FWhm_NsMax, %VAL(Type),  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%ErFWhm_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErFWhm_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErFWhm_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErFWhm_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%ErFWhm_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ErFWhm_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_R_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_R_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_R_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_I_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_I_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_Index_I_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%Shape_R_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_R_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_R_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_R_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%Shape_R_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_R_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%Shape_I_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_I_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_I_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_I_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%Shape_I_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%Shape_I_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_R_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Avg, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Std, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Min, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_Max, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Ils_Perfo_Stats%Ns_Ils
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_NsMin, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ils_Perfo_Stats%ShapeDiff_I_NsMax, %VAL(Type),&
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
      end if
   return
   end subroutine writeils_perfo_stats
!
!
end module ils_perfo_stats_type

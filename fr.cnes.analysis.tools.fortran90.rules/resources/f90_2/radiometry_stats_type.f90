!!* radiometry_stats_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> radiometry_stats_type -- Module
!!
!! * Purpose
!!
!!   Module for radiometry statistics type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the radiometry stats type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Radiometry_stats     : type for declaration and allocation of Radiometry_stats
!!     * alloc_Radiometry_Stats    : type_Radiometry_Stats allocation
!!     * dalloc_Radiometry_Stats   : type_Radiometry_Stats deallocation
!!     * readradiometry_Stats      : type Radiometry_Ststa reading
!!     * writeradiometry_Stats     : type Radiometry_Ststa writing
!! * References
!!     
!!      SPS ATBD

module radiometry_stats_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Radiometry_Stats   &
            ,alloc_Radiometry_Stats  &
            ,dalloc_Radiometry_Stats &
            ,readradiometry_stats    &
            ,writeradiometry_stats
!
   type :: type_Radiometry_Stats
     character(len=500)                               :: filename !< spectrum file name
     real(kind=DOUBLE)                                :: Date !< date
     character(len=2)                                 :: Type !< spectrum type
     integer(kind=LONG)                               :: N_Sample !< spectrum samples number
     integer(kind=LONG)                               :: Nb_Wn_Class !< class number
     integer(kind=LONG)                               :: Ns_First !< first sample
     integer(kind=LONG)                               :: Ns_Last !< last sample
     real(kind=DOUBLE)                                :: Wn_First !< first wavenumber
     real(kind=DOUBLE)                                :: Wn_Last !< last wavenumber
     real(kind=DOUBLE)                                :: WnMax !< maximum wavenumber
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn_Class !< class wavenumber
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Avg_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Avg_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Avg_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Avg_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Avg_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Avg_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Std_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Std_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Std_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Std_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Std_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Std_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Min_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Min_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Min_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Min_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Min_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Min_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Max_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Max_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Max_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Max_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Max_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_Max_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMin_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMin_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMin_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMin_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_NsMin_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_NsMin_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMax_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMax_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMax_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_NsMax_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_NsMax_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_NsMax_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Avg_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Avg_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Avg_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Avg_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Avg_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Avg_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Std_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Std_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Std_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Std_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Std_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Std_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Min_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Min_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Min_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Min_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Min_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Min_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Max_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Max_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Max_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Max_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Max_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_Max_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMin_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMin_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMin_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMin_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_NsMin_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_NsMin_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMax_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMax_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMax_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_NsMax_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_NsMax_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_NsMax_NsMax

     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Avg_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Avg_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Avg_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Avg_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Avg_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Avg_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Std_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Std_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Std_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Std_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Std_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Std_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Min_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Min_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Min_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Min_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Min_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Min_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Max_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Max_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Max_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Max_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Max_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_Max_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMin_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMin_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMin_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMin_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_NsMin_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_NsMin_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMax_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMax_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMax_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_NsMax_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_NsMax_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_NsMax_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Avg_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Avg_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Avg_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Avg_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Avg_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Avg_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Std_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Std_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Std_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Std_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Std_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Std_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Min_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Min_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Min_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Min_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Min_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Min_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Max_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Max_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Max_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Max_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Max_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_Max_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMin_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMin_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMin_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMin_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_NsMin_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_NsMin_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMax_Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMax_Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMax_Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_NsMax_Max
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_NsMax_NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_NsMax_NsMax
   end type type_Radiometry_Stats
!
   contains
!
!
   subroutine alloc_Radiometry_Stats( Spectrum )
     implicit none
     type(type_Radiometry_Stats), intent(inout) :: Spectrum
     integer(kind=LONG)                         :: ErrCode
!
     allocate( Spectrum%Wn(Spectrum%N_Sample),           stat=ErrCode )
     allocate( Spectrum%Wn_Class(Spectrum%Nb_Wn_Class+1),stat=ErrCode )

     if( trim(Spectrum%Type) == 'RI' .or. &
         trim(Spectrum%Type) == 'R') then
        allocate( Spectrum%Nedl_R_Avg(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_R_Std(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_R_Min(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_R_Max(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_R_NsMin(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedl_R_NsMax(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedt_R_Avg(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_R_Std(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_R_Min(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_R_Max(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_R_NsMin(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedt_R_NsMax(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        Spectrum%Nedl_R_Class_Avg_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Avg_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Avg_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Avg_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Avg_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Avg_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Std_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Std_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Std_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Std_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Std_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Std_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Min_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Min_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Min_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Min_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Min_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Min_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Max_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Max_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Max_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Max_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Max_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_Max_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_NsMin_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMin_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMin_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMin_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMin_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_NsMin_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_NsMax_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMax_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMax_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMax_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMax_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_NsMax_NsMax(1:Spectrum%Nb_Wn_Class) = 0
     end if
     if( trim(Spectrum%Type) == 'RI' .or. &
              trim(Spectrum%Type) == 'I') then
        allocate( Spectrum%Nedl_I_Avg(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_I_Std(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_I_Min(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_I_Max(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_I_NsMin(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedl_I_NsMax(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedt_I_Avg(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_I_Std(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_I_Min(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_I_Max(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_I_NsMin(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedt_I_NsMax(Spectrum%N_Sample),stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        Spectrum%Nedl_I_Class_Avg_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Avg_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Avg_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Avg_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Avg_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Avg_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Std_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Std_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Std_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Std_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Std_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Std_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Min_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Min_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Min_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Min_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Min_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Min_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Max_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Max_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Max_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Max_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Max_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_Max_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_NsMin_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMin_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMin_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMin_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMin_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_NsMin_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_NsMax_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMax_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMax_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMax_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMax_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_NsMax_NsMax(1:Spectrum%Nb_Wn_Class) = 0
     end if
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Radiometry_stats Error'
        write(0,*) 'Radiometry_stats: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Radiometry_Stats
!
!
   subroutine dalloc_Radiometry_Stats( Spectrum )
     implicit none
     type(type_Radiometry_Stats), intent(inout) :: Spectrum
!
     deallocate( Spectrum%Wn )
     deallocate( Spectrum%Wn_Class )
     if( trim(Spectrum%Type) == 'RI' .or. &
         trim(Spectrum%Type) == 'R') then
       deallocate( Spectrum%Nedl_R_Avg )
       deallocate( Spectrum%Nedl_R_Std )
       deallocate( Spectrum%Nedl_R_Min )
       deallocate( Spectrum%Nedl_R_Max )
       deallocate( Spectrum%Nedl_R_NsMin )
       deallocate( Spectrum%Nedl_R_NsMax )
       deallocate( Spectrum%Nedt_R_Avg )
       deallocate( Spectrum%Nedt_R_Std )
       deallocate( Spectrum%Nedt_R_Min )
       deallocate( Spectrum%Nedt_R_Max )
       deallocate( Spectrum%Nedt_R_NsMin )
       deallocate( Spectrum%Nedt_R_NsMax )
       deallocate( Spectrum%Nedl_R_Class_Avg_Avg )
       deallocate( Spectrum%Nedl_R_Class_Avg_Std )
       deallocate( Spectrum%Nedl_R_Class_Avg_Min )
       deallocate( Spectrum%Nedl_R_Class_Avg_Max )
       deallocate( Spectrum%Nedl_R_Class_Avg_NsMin )
       deallocate( Spectrum%Nedl_R_Class_Avg_NsMax )
       deallocate( Spectrum%Nedt_R_Class_Avg_Avg )
       deallocate( Spectrum%Nedt_R_Class_Avg_Std )
       deallocate( Spectrum%Nedt_R_Class_Avg_Min )
       deallocate( Spectrum%Nedt_R_Class_Avg_Max )
       deallocate( Spectrum%Nedt_R_Class_Avg_NsMin )
       deallocate( Spectrum%Nedt_R_Class_Avg_NsMax )
       deallocate( Spectrum%Nedl_R_Class_Std_Avg )
       deallocate( Spectrum%Nedl_R_Class_Std_Std )
       deallocate( Spectrum%Nedl_R_Class_Std_Min )
       deallocate( Spectrum%Nedl_R_Class_Std_Max )
       deallocate( Spectrum%Nedl_R_Class_Std_NsMin )
       deallocate( Spectrum%Nedl_R_Class_Std_NsMax )
       deallocate( Spectrum%Nedt_R_Class_Std_Avg )
       deallocate( Spectrum%Nedt_R_Class_Std_Std )
       deallocate( Spectrum%Nedt_R_Class_Std_Min )
       deallocate( Spectrum%Nedt_R_Class_Std_Max )
       deallocate( Spectrum%Nedt_R_Class_Std_NsMin )
       deallocate( Spectrum%Nedt_R_Class_Std_NsMax )
       deallocate( Spectrum%Nedl_R_Class_Min_Avg )
       deallocate( Spectrum%Nedl_R_Class_Min_Std )
       deallocate( Spectrum%Nedl_R_Class_Min_Min )
       deallocate( Spectrum%Nedl_R_Class_Min_Max )
       deallocate( Spectrum%Nedl_R_Class_Min_NsMin )
       deallocate( Spectrum%Nedl_R_Class_Min_NsMax )
       deallocate( Spectrum%Nedt_R_Class_Min_Avg )
       deallocate( Spectrum%Nedt_R_Class_Min_Std )
       deallocate( Spectrum%Nedt_R_Class_Min_Min )
       deallocate( Spectrum%Nedt_R_Class_Min_Max )
       deallocate( Spectrum%Nedt_R_Class_Min_NsMin )
       deallocate( Spectrum%Nedt_R_Class_Min_NsMax )
       deallocate( Spectrum%Nedl_R_Class_Max_Avg )
       deallocate( Spectrum%Nedl_R_Class_Max_Std )
       deallocate( Spectrum%Nedl_R_Class_Max_Min )
       deallocate( Spectrum%Nedl_R_Class_Max_Max )
       deallocate( Spectrum%Nedl_R_Class_Max_NsMin )
       deallocate( Spectrum%Nedl_R_Class_Max_NsMax )
       deallocate( Spectrum%Nedt_R_Class_Max_Avg )
       deallocate( Spectrum%Nedt_R_Class_Max_Std )
       deallocate( Spectrum%Nedt_R_Class_Max_Min )
       deallocate( Spectrum%Nedt_R_Class_Max_Max )
       deallocate( Spectrum%Nedt_R_Class_Max_NsMin )
       deallocate( Spectrum%Nedt_R_Class_Max_NsMax )
       deallocate( Spectrum%Nedl_R_Class_NsMin_Avg )
       deallocate( Spectrum%Nedl_R_Class_NsMin_Std )
       deallocate( Spectrum%Nedl_R_Class_NsMin_Min )
       deallocate( Spectrum%Nedl_R_Class_NsMin_Max )
       deallocate( Spectrum%Nedl_R_Class_NsMin_NsMin )
       deallocate( Spectrum%Nedl_R_Class_NsMin_NsMax )
       deallocate( Spectrum%Nedt_R_Class_NsMin_Avg )
       deallocate( Spectrum%Nedt_R_Class_NsMin_Std )
       deallocate( Spectrum%Nedt_R_Class_NsMin_Min )
       deallocate( Spectrum%Nedt_R_Class_NsMin_Max )
       deallocate( Spectrum%Nedt_R_Class_NsMin_NsMin )
       deallocate( Spectrum%Nedt_R_Class_NsMin_NsMax )
       deallocate( Spectrum%Nedl_R_Class_NsMax_Avg )
       deallocate( Spectrum%Nedl_R_Class_NsMax_Std )
       deallocate( Spectrum%Nedl_R_Class_NsMax_Min )
       deallocate( Spectrum%Nedl_R_Class_NsMax_Max )
       deallocate( Spectrum%Nedl_R_Class_NsMax_NsMin )
       deallocate( Spectrum%Nedl_R_Class_NsMax_NsMax )
       deallocate( Spectrum%Nedt_R_Class_NsMax_Avg )
       deallocate( Spectrum%Nedt_R_Class_NsMax_Std )
       deallocate( Spectrum%Nedt_R_Class_NsMax_Min )
       deallocate( Spectrum%Nedt_R_Class_NsMax_Max )
       deallocate( Spectrum%Nedt_R_Class_NsMax_NsMin )
       deallocate( Spectrum%Nedt_R_Class_NsMax_NsMax )
     end if

     if( trim(Spectrum%Type) == 'RI' .or. &
         trim(Spectrum%Type) == 'I') then
       deallocate( Spectrum%Nedl_I_Avg )
       deallocate( Spectrum%Nedl_I_Std )
       deallocate( Spectrum%Nedl_I_Min )
       deallocate( Spectrum%Nedl_I_Max )
       deallocate( Spectrum%Nedl_I_NsMin )
       deallocate( Spectrum%Nedl_I_NsMax )
       deallocate( Spectrum%Nedt_I_Avg )
       deallocate( Spectrum%Nedt_I_Std )
       deallocate( Spectrum%Nedt_I_Min )
       deallocate( Spectrum%Nedt_I_Max )
       deallocate( Spectrum%Nedt_I_NsMin )
       deallocate( Spectrum%Nedt_I_NsMax )
       deallocate( Spectrum%Nedl_I_Class_Avg_Avg )
       deallocate( Spectrum%Nedl_I_Class_Avg_Std )
       deallocate( Spectrum%Nedl_I_Class_Avg_Min )
       deallocate( Spectrum%Nedl_I_Class_Avg_Max )
       deallocate( Spectrum%Nedl_I_Class_Avg_NsMin )
       deallocate( Spectrum%Nedl_I_Class_Avg_NsMax )
       deallocate( Spectrum%Nedt_I_Class_Avg_Avg )
       deallocate( Spectrum%Nedt_I_Class_Avg_Std )
       deallocate( Spectrum%Nedt_I_Class_Avg_Min )
       deallocate( Spectrum%Nedt_I_Class_Avg_Max )
       deallocate( Spectrum%Nedt_I_Class_Avg_NsMin )
       deallocate( Spectrum%Nedt_I_Class_Avg_NsMax )
       deallocate( Spectrum%Nedl_I_Class_Std_Avg )
       deallocate( Spectrum%Nedl_I_Class_Std_Std )
       deallocate( Spectrum%Nedl_I_Class_Std_Min )
       deallocate( Spectrum%Nedl_I_Class_Std_Max )
       deallocate( Spectrum%Nedl_I_Class_Std_NsMin )
       deallocate( Spectrum%Nedl_I_Class_Std_NsMax )
       deallocate( Spectrum%Nedt_I_Class_Std_Avg )
       deallocate( Spectrum%Nedt_I_Class_Std_Std )
       deallocate( Spectrum%Nedt_I_Class_Std_Min )
       deallocate( Spectrum%Nedt_I_Class_Std_Max )
       deallocate( Spectrum%Nedt_I_Class_Std_NsMin )
       deallocate( Spectrum%Nedt_I_Class_Std_NsMax )
       deallocate( Spectrum%Nedl_I_Class_Min_Avg )
       deallocate( Spectrum%Nedl_I_Class_Min_Std )
       deallocate( Spectrum%Nedl_I_Class_Min_Min )
       deallocate( Spectrum%Nedl_I_Class_Min_Max )
       deallocate( Spectrum%Nedl_I_Class_Min_NsMin )
       deallocate( Spectrum%Nedl_I_Class_Min_NsMax )
       deallocate( Spectrum%Nedt_I_Class_Min_Avg )
       deallocate( Spectrum%Nedt_I_Class_Min_Std )
       deallocate( Spectrum%Nedt_I_Class_Min_Min )
       deallocate( Spectrum%Nedt_I_Class_Min_Max )
       deallocate( Spectrum%Nedt_I_Class_Min_NsMin )
       deallocate( Spectrum%Nedt_I_Class_Min_NsMax )
       deallocate( Spectrum%Nedl_I_Class_Max_Avg )
       deallocate( Spectrum%Nedl_I_Class_Max_Std )
       deallocate( Spectrum%Nedl_I_Class_Max_Min )
       deallocate( Spectrum%Nedl_I_Class_Max_Max )
       deallocate( Spectrum%Nedl_I_Class_Max_NsMin )
       deallocate( Spectrum%Nedl_I_Class_Max_NsMax )
       deallocate( Spectrum%Nedt_I_Class_Max_Avg )
       deallocate( Spectrum%Nedt_I_Class_Max_Std )
       deallocate( Spectrum%Nedt_I_Class_Max_Min )
       deallocate( Spectrum%Nedt_I_Class_Max_Max )
       deallocate( Spectrum%Nedt_I_Class_Max_NsMin )
       deallocate( Spectrum%Nedt_I_Class_Max_NsMax )
       deallocate( Spectrum%Nedl_I_Class_NsMin_Avg )
       deallocate( Spectrum%Nedl_I_Class_NsMin_Std )
       deallocate( Spectrum%Nedl_I_Class_NsMin_Min )
       deallocate( Spectrum%Nedl_I_Class_NsMin_Max )
       deallocate( Spectrum%Nedl_I_Class_NsMin_NsMin )
       deallocate( Spectrum%Nedl_I_Class_NsMin_NsMax )
       deallocate( Spectrum%Nedt_I_Class_NsMin_Avg )
       deallocate( Spectrum%Nedt_I_Class_NsMin_Std )
       deallocate( Spectrum%Nedt_I_Class_NsMin_Min )
       deallocate( Spectrum%Nedt_I_Class_NsMin_Max )
       deallocate( Spectrum%Nedt_I_Class_NsMin_NsMin )
       deallocate( Spectrum%Nedt_I_Class_NsMin_NsMax )
       deallocate( Spectrum%Nedl_I_Class_NsMax_Avg )
       deallocate( Spectrum%Nedl_I_Class_NsMax_Std )
       deallocate( Spectrum%Nedl_I_Class_NsMax_Min )
       deallocate( Spectrum%Nedl_I_Class_NsMax_Max )
       deallocate( Spectrum%Nedl_I_Class_NsMax_NsMin )
       deallocate( Spectrum%Nedl_I_Class_NsMax_NsMax )
       deallocate( Spectrum%Nedt_I_Class_NsMax_Avg )
       deallocate( Spectrum%Nedt_I_Class_NsMax_Std )
       deallocate( Spectrum%Nedt_I_Class_NsMax_Min )
       deallocate( Spectrum%Nedt_I_Class_NsMax_Max )
       deallocate( Spectrum%Nedt_I_Class_NsMax_NsMin )
       deallocate( Spectrum%Nedt_I_Class_NsMax_NsMax )
     end if
     return
   end subroutine dalloc_Radiometry_Stats
!
!
   subroutine readradiometry_Stats( Spectrum,iostatus )
     implicit none
     type(type_Radiometry_Stats), intent(inout) :: Spectrum
!
     integer(kind=LONG) , intent(out)   :: iostatus
     integer(kind=DOUBLE)               :: ifile
     integer(kind=LONG)                 :: offset
     integer(kind=LONG)                 :: Type
     integer(kind=LONG)                 :: Size
!
      iostatus = 0
      call open_file_r(Spectrum%filename(1:len_trim(        &
                       Spectrum%filename)) // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Date, %VAL(Type),        &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call read_field( ifile, Spectrum%Type, %VAL(Type),        &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Spectrum%N_Sample, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Nb_Wn_Class, %VAL(Type), &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_First, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_Last, %VAL(Type),     &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Wn_First, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Wn_Last, %VAL(Type),     &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%WnMax, %VAL(Type),       &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%dWn, %VAL(Type),         &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Radiometry_Stats( Spectrum )
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call read_field( ifile, Spectrum%Wn, %VAL(Type),          &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Size = 8*Spectrum%Nb_Wn_Class
         call read_field( ifile, Spectrum%Wn_Class, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Spectrum%Type) == 'RI' .or. &
             trim(Spectrum%Type) == 'R') then
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedl_R_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedl_R_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedt_R_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedt_R_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Avg_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Avg_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Avg_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Avg_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Avg_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Avg_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Std_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Std_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Std_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Std_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Std_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Std_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Min_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Min_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Min_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Min_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Min_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Min_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Max_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Max_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Max_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Max_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_Max_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_Max_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMin_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMin_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMin_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMin_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMin_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMin_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMax_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMax_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMax_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMax_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMax_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_R_Class_NsMax_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Avg_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Avg_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Avg_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Avg_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Avg_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Avg_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Std_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Std_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Std_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Std_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Std_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Std_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Min_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Min_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Min_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Min_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Min_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Min_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Max_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Max_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Max_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Max_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_Max_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_Max_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMin_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMin_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMin_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMin_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMin_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMin_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMax_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMax_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMax_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMax_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMax_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_R_Class_NsMax_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( trim(Spectrum%Type) == 'RI' .or. &
             trim(Spectrum%Type) == 'I') then
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedl_I_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedl_I_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedt_I_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call read_field( ifile, Spectrum%Nedt_I_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Avg_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Avg_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Avg_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Avg_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Avg_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Avg_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Std_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Std_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Std_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Std_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Std_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Std_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Min_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Min_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Min_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Min_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Min_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Min_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Max_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Max_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Max_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Max_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_Max_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_Max_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMin_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMin_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMin_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMin_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMin_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMin_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMax_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMax_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMax_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMax_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMax_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedl_I_Class_NsMax_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Avg_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Avg_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Avg_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Avg_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Avg_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Avg_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Std_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Std_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Std_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Std_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Std_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Std_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Min_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Min_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Min_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Min_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Min_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Min_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Max_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Max_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Max_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Max_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_Max_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_Max_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMin_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMin_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMin_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMin_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMin_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMin_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMax_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMax_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMax_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMax_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMax_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%Nedt_I_Class_NsMax_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         call close_file(Spectrum%filename(1:len_trim(&
                         Spectrum%filename)) // char(0), ifile)
      end if

   return

   end subroutine readradiometry_stats
!
!
   subroutine writeradiometry_stats( Spectrum, iostatus )
     implicit none
     type(type_Radiometry_Stats), intent(inout) :: Spectrum
!
     integer(kind=LONG) , intent(out)   :: iostatus
     integer(kind=DOUBLE)               :: ifile
     integer(kind=LONG)                 :: offset
     integer(kind=LONG)                 :: Type
     integer(kind=LONG)                 :: Size
!
      iostatus = 0
      call open_file_w(Spectrum%filename(1:len_trim(        &
                       Spectrum%filename)) // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum%Date, %VAL(Type),        &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call write_field( ifile, Spectrum%Type, %VAL(Type),        &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Spectrum%N_Sample, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Nb_Wn_Class, %VAL(Type), &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Ns_First, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Ns_Last, %VAL(Type),     &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum%Wn_First, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Wn_Last, %VAL(Type),     &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%WnMax, %VAL(Type),       &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%dWn, %VAL(Type),         &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call write_field( ifile, Spectrum%Wn, %VAL(Type),          &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Size = 8*Spectrum%Nb_Wn_Class
         call write_field( ifile, Spectrum%Wn_Class, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Spectrum%Type) == 'RI' .or. &
             trim(Spectrum%Type) == 'R') then
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedl_R_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedl_R_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedt_R_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedt_R_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Avg_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Avg_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Avg_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Avg_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Avg_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Avg_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Std_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Std_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Std_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Std_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Std_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Std_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Min_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Min_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Min_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Min_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Min_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Min_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Max_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Max_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Max_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Max_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_Max_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_Max_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMin_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMin_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMin_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMin_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMin_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMin_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMax_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMax_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMax_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMax_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMax_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_R_Class_NsMax_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Avg_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Avg_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Avg_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Avg_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Avg_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Avg_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Std_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Std_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Std_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Std_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Std_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Std_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Min_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Min_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Min_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Min_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Min_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Min_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Max_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Max_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Max_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Max_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_Max_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_Max_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMin_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMin_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMin_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMin_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMin_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMin_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMax_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMax_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMax_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMax_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMax_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_R_Class_NsMax_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( trim(Spectrum%Type) == 'RI' .or. &
             trim(Spectrum%Type) == 'I') then
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedl_I_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedl_I_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedt_I_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%N_Sample
           call write_field( ifile, Spectrum%Nedt_I_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Avg_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Avg_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Avg_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Avg_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Avg_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Avg_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Std_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Std_Std, %VAL(Type),  &
                                       %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Std_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Std_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Std_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Std_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Min_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Min_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Min_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Min_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Min_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Min_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Max_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Max_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Max_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Max_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_Max_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_Max_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMin_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMin_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMin_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMin_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMin_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMin_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMax_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMax_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMax_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMax_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMax_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedl_I_Class_NsMax_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Avg_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Avg_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Avg_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Avg_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Avg_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Avg_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Std_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Std_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Std_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Std_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Std_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Std_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Min_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Min_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Min_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Min_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Min_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Min_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Max_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Max_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Max_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Max_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_Max_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_Max_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMin_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMin_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMin_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMin_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMin_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMin_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMax_Avg, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMax_Std, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMax_Min, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMax_Max, %VAL(Type),  &
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum%Nb_Wn_Class
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMax_NsMin, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%Nedt_I_Class_NsMax_NsMax, %VAL(Type),&
                                     %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         call close_file(Spectrum%filename(1:len_trim(&
                         Spectrum%filename)) // char(0), ifile)
      end if

   return

   end subroutine writeradiometry_stats
!
!
end module radiometry_stats_type

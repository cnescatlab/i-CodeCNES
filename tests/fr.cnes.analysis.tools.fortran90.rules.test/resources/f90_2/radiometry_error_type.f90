!!* radiometry_error_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> radiometry_error_type -- Module
!!
!! * Purpose
!!
!!   Module for spectrum type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the radiometry error type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Radiometry_Error     : type for declaration and allocation of Radiometry_Error
!!     * alloc_Radiometry_Error    : type_Radiometry_Error allocation
!!     * dalloc_Radiometry_Error   : type_Radiometry_Error deallocation
!! * References
!!     
!!      SPS ATBD

module radiometry_error_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Radiometry_Error   &
            ,alloc_Radiometry_Error  &
            ,dalloc_Radiometry_Error &
            ,readradiometry_error    &
            ,writeradiometry_error
!
   type :: type_Radiometry_Error
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
     real(kind=DOUBLE)    , dimension(:), allocatable :: S_R !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: S_I !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: S0_R !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: S0_I !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: TB !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: TB0 !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I !< 
     real(kind=DOUBLE)                                :: Nedl_R_Avg
     real(kind=DOUBLE)                                :: Nedl_R_Std
     real(kind=DOUBLE)                                :: Nedl_R_Min
     real(kind=DOUBLE)                                :: Nedl_R_Max
     integer(kind=LONG)                               :: Nedl_R_NsMin
     integer(kind=LONG)                               :: Nedl_R_NsMax
     real(kind=DOUBLE)                                :: Nedt_R_Avg
     real(kind=DOUBLE)                                :: Nedt_R_Std
     real(kind=DOUBLE)                                :: Nedt_R_Min
     real(kind=DOUBLE)                                :: Nedt_R_Max
     integer(kind=LONG)                               :: Nedt_R_NsMin
     integer(kind=LONG)                               :: Nedt_R_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Avg !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Std !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Min !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_R_Class_Max !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_NsMin !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_R_Class_NsMax !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Avg !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Std !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Min !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_R_Class_Max !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_NsMin !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_R_Class_NsMax !< 
     real(kind=DOUBLE)                                :: Nedl_I_Avg
     real(kind=DOUBLE)                                :: Nedl_I_Std
     real(kind=DOUBLE)                                :: Nedl_I_Min
     real(kind=DOUBLE)                                :: Nedl_I_Max
     integer(kind=LONG)                               :: Nedl_I_NsMin
     integer(kind=LONG)                               :: Nedl_I_NsMax
     real(kind=DOUBLE)                                :: Nedt_I_Avg
     real(kind=DOUBLE)                                :: Nedt_I_Std
     real(kind=DOUBLE)                                :: Nedt_I_Min
     real(kind=DOUBLE)                                :: Nedt_I_Max
     integer(kind=LONG)                               :: Nedt_I_NsMin
     integer(kind=LONG)                               :: Nedt_I_NsMax
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Avg !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Std !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Min !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedl_I_Class_Max !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_NsMin !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedl_I_Class_NsMax !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Avg !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Std !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Min !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Nedt_I_Class_Max !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_NsMin !< 
     integer(kind=LONG)   , dimension(:), allocatable :: Nedt_I_Class_NsMax !< 
   end type type_Radiometry_Error
!
   contains
!
!
   subroutine alloc_Radiometry_Error( Spectrum )
     implicit none
     type(type_Radiometry_Error), intent(inout) :: Spectrum
     integer(kind=LONG)                 :: ErrCode
!
     allocate( Spectrum%Wn(Spectrum%N_Sample),           stat=ErrCode )
     allocate( Spectrum%Wn_Class(Spectrum%Nb_Wn_Class+1),stat=ErrCode )

     if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' .or.&
         Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R' ) then
        allocate( Spectrum%TB(Spectrum%N_Sample),      stat=ErrCode )
        allocate( Spectrum%TB0(Spectrum%N_Sample),     stat=ErrCode )
        allocate( Spectrum%S_R(Spectrum%N_Sample),     stat=ErrCode )
        allocate( Spectrum%S0_R(Spectrum%N_Sample),    stat=ErrCode )
        allocate( Spectrum%Nedl_R(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_R(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_R_Class_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_R_Class_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        Spectrum%Nedl_R_Class_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_R_Class_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_R_Class_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedt_R_Class_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_R_Class_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_R_Class_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_R_Class_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_R_Class_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedt_R_Class_NsMax(1:Spectrum%Nb_Wn_Class) = 0
     end if
     if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' .or.&
         Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I' ) then
        allocate( Spectrum%S_I(Spectrum%N_Sample),     stat=ErrCode )
        allocate( Spectrum%S0_I(Spectrum%N_Sample),    stat=ErrCode )
        allocate( Spectrum%Nedl_I(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedt_I(Spectrum%N_Sample),  stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedl_I_Class_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Avg(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Std(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Min(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_Max(Spectrum%Nb_Wn_Class),  &
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMin(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        allocate( Spectrum%Nedt_I_Class_NsMax(Spectrum%Nb_Wn_Class),&
                                                       stat=ErrCode )
        Spectrum%Nedl_I_Class_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedl_I_Class_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedl_I_Class_NsMax(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedt_I_Class_Avg(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_I_Class_Std(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_I_Class_Min(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_I_Class_Max(1:Spectrum%Nb_Wn_Class) = 0_DOUBLE
        Spectrum%Nedt_I_Class_NsMin(1:Spectrum%Nb_Wn_Class) = 0
        Spectrum%Nedt_I_Class_NsMax(1:Spectrum%Nb_Wn_Class) = 0
     end if
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Radiometry_Error Error'
        write(0,*) 'Radiometry_Error: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Radiometry_Error
!
!
   subroutine dalloc_Radiometry_Error( Spectrum )
     implicit none
     type(type_Radiometry_Error), intent(inout) :: Spectrum
!
     deallocate( Spectrum%Wn )
     deallocate( Spectrum%Wn_Class )
     if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' .or.&
         Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R' ) then
       deallocate( Spectrum%TB )
       deallocate( Spectrum%TB0 )
       deallocate( Spectrum%S_R )
       deallocate( Spectrum%S0_R )
       deallocate( Spectrum%Nedl_R )
       deallocate( Spectrum%Nedt_R )
       deallocate( Spectrum%Nedl_R_Class_Avg )
       deallocate( Spectrum%Nedl_R_Class_Std )
       deallocate( Spectrum%Nedl_R_Class_Min )
       deallocate( Spectrum%Nedl_R_Class_Max )
       deallocate( Spectrum%Nedl_R_Class_NsMin )
       deallocate( Spectrum%Nedl_R_Class_NsMax )
       deallocate( Spectrum%Nedt_R_Class_Avg )
       deallocate( Spectrum%Nedt_R_Class_Std )
       deallocate( Spectrum%Nedt_R_Class_Min )
       deallocate( Spectrum%Nedt_R_Class_Max )
       deallocate( Spectrum%Nedt_R_Class_NsMin )
       deallocate( Spectrum%Nedt_R_Class_NsMax )
     end if
     if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' .or.&
          Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I' ) then
       deallocate( Spectrum%S_I )
       deallocate( Spectrum%S0_I )
       deallocate( Spectrum%Nedl_I )
       deallocate( Spectrum%Nedt_I )
       deallocate( Spectrum%Nedl_I_Class_Avg )
       deallocate( Spectrum%Nedl_I_Class_Std )
       deallocate( Spectrum%Nedl_I_Class_Min )
       deallocate( Spectrum%Nedl_I_Class_Max )
       deallocate( Spectrum%Nedl_I_Class_NsMin )
       deallocate( Spectrum%Nedl_I_Class_NsMax )
       deallocate( Spectrum%Nedt_I_Class_Avg )
       deallocate( Spectrum%Nedt_I_Class_Std )
       deallocate( Spectrum%Nedt_I_Class_Min )
       deallocate( Spectrum%Nedt_I_Class_Max )
       deallocate( Spectrum%Nedt_I_Class_NsMin )
       deallocate( Spectrum%Nedt_I_Class_NsMax )
     end if
     return
   end subroutine dalloc_Radiometry_Error
!
!
   subroutine readradiometry_error( Spectrum_Error,iostatus )
     implicit none
     type(type_Radiometry_Error), intent(inout) :: Spectrum_Error
!
     integer(kind=LONG) , intent(out)   :: iostatus
     integer(kind=DOUBLE)               :: ifile
     integer(kind=LONG)                 :: offset
     integer(kind=LONG)                 :: Type
     integer(kind=LONG)                 :: Size
!
      iostatus = 0
      call open_file_r(Spectrum_Error%filename(1:len_trim(        &
                       Spectrum_Error%filename)) // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum_Error%Date, %VAL(Type),        &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call read_field( ifile, Spectrum_Error%Type, %VAL(Type),        &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Spectrum_Error%N_Sample, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum_Error%Nb_Wn_Class, %VAL(Type), &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum_Error%Ns_First, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum_Error%Ns_Last, %VAL(Type),     &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum_Error%Wn_First, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum_Error%Wn_Last, %VAL(Type),     &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum_Error%WnMax, %VAL(Type),       &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum_Error%dWn, %VAL(Type),         &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Radiometry_Error( Spectrum_Error )
         Type = r8Type
         Size = 8*Spectrum_Error%N_Sample
         call read_field( ifile, Spectrum_Error%Wn, %VAL(Type),          &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Size = 8*Spectrum_Error%Nb_Wn_Class
         call read_field( ifile, Spectrum_Error%Wn_Class, %VAL(Type),    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Spectrum_Error%Type) == 'RI' .or.&
             trim(Spectrum_Error%Type) == 'R' ) then
           Type = r8Type
           Size = 8*Spectrum_Error%N_Sample
           call read_field( ifile, Spectrum_Error%S_R, %VAL(Type),         &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%S0_R, %VAL(Type),        &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R, %VAL(Type),      &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R, %VAL(Type),      &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%TB, %VAL(Type),          &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%TB0, %VAL(Type),         &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call read_field( ifile, Spectrum_Error%Nedl_R_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call read_field( ifile, Spectrum_Error%Nedl_R_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call read_field( ifile, Spectrum_Error%Nedt_R_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call read_field( ifile, Spectrum_Error%Nedt_R_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedl_R_Class_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Class_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Class_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Class_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedl_R_Class_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_R_Class_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedt_R_Class_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Class_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Class_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Class_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedt_R_Class_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_R_Class_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( trim(Spectrum_Error%Type) == 'RI' .or.&
             trim(Spectrum_Error%Type) == 'I' ) then
           Type = r8Type
           Size = 8*Spectrum_Error%N_Sample
           call read_field( ifile, Spectrum_Error%S_I, %VAL(Type),         &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%S0_I, %VAL(Type),        &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I, %VAL(Type),      &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I, %VAL(Type),      &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call read_field( ifile, Spectrum_Error%Nedl_I_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call read_field( ifile, Spectrum_Error%Nedl_I_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call read_field( ifile, Spectrum_Error%Nedt_I_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call read_field( ifile, Spectrum_Error%Nedt_I_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedl_I_Class_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Class_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Class_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Class_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedl_I_Class_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedl_I_Class_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedt_I_Class_Avg, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Class_Std, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Class_Min, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Class_Max, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call read_field( ifile, Spectrum_Error%Nedt_I_Class_NsMin, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum_Error%Nedt_I_Class_NsMax, %VAL(Type),&
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         call close_file(Spectrum_Error%filename(1:len_trim(&
                         Spectrum_Error%filename)) // char(0), ifile)
      end if

   return

   end subroutine readradiometry_error
!
!
   subroutine writeradiometry_error( Spectrum_Error, iostatus )
     implicit none
     type(type_Radiometry_Error), intent(inout) :: Spectrum_Error
!
     integer(kind=LONG) , intent(out)   :: iostatus
     integer(kind=DOUBLE)               :: ifile
     integer(kind=LONG)                 :: offset
     integer(kind=LONG)                 :: Type
     integer(kind=LONG)                 :: Size
!
      iostatus = 0
      call open_file_w(Spectrum_Error%filename(1:len_trim(        &
                       Spectrum_Error%filename)) // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum_Error%Date, %VAL(Type),        &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call write_field( ifile, Spectrum_Error%Type, %VAL(Type),        &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Spectrum_Error%N_Sample, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum_Error%Nb_Wn_Class, %VAL(Type), &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum_Error%Ns_First, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum_Error%Ns_Last, %VAL(Type),     &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum_Error%Wn_First, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum_Error%Wn_Last, %VAL(Type),     &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum_Error%WnMax, %VAL(Type),       &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum_Error%dWn, %VAL(Type),         &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Spectrum_Error%N_Sample
         call write_field( ifile, Spectrum_Error%Wn, %VAL(Type),          &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Size = 8*Spectrum_Error%Nb_Wn_Class
         call write_field( ifile, Spectrum_Error%Wn_Class, %VAL(Type),    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Spectrum_Error%Type) == 'RI' .or.&
             trim(Spectrum_Error%Type) == 'R' ) then
           Type = r8Type
           Size = 8*Spectrum_Error%N_Sample
           call write_field( ifile, Spectrum_Error%S_R, %VAL(Type),         &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%S0_R, %VAL(Type),        &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R, %VAL(Type),      &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R, %VAL(Type),      &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%TB, %VAL(Type),          &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%TB0, %VAL(Type),         &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call write_field( ifile, Spectrum_Error%Nedl_R_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call write_field( ifile, Spectrum_Error%Nedl_R_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call write_field( ifile, Spectrum_Error%Nedt_R_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call write_field( ifile, Spectrum_Error%Nedt_R_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedl_R_Class_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Class_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Class_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Class_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedl_R_Class_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_R_Class_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedt_R_Class_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Class_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Class_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Class_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedt_R_Class_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_R_Class_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( trim(Spectrum_Error%Type) == 'RI' .or.&
             trim(Spectrum_Error%Type) == 'I' ) then
           Type = r8Type
           Size = 8*Spectrum_Error%N_Sample
           call write_field( ifile, Spectrum_Error%S_I, %VAL(Type),         &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%S0_I, %VAL(Type),        &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I, %VAL(Type),      &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I, %VAL(Type),      &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call write_field( ifile, Spectrum_Error%Nedl_I_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call write_field( ifile, Spectrum_Error%Nedl_I_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8
           call write_field( ifile, Spectrum_Error%Nedt_I_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4
           call write_field( ifile, Spectrum_Error%Nedt_I_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedl_I_Class_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Class_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Class_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Class_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedl_I_Class_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedl_I_Class_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = r8Type
           Size = 8*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedt_I_Class_Avg, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Class_Std, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Class_Min, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Class_Max, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           Type = i4Type
           Size = 4*Spectrum_Error%Nb_Wn_Class
           call write_field( ifile, Spectrum_Error%Nedt_I_Class_NsMin, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum_Error%Nedt_I_Class_NsMax, %VAL(Type),&
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         call close_file(Spectrum_Error%filename(1:len_trim(&
                         Spectrum_Error%filename)) // char(0), ifile)
      end if

   return

   end subroutine writeradiometry_error
!
!
end module radiometry_error_type

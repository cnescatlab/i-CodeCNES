!!* interf_perfo_stats_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> type_Interf_Perfo_Stats -- Module
!!
!! * Purpose
!!
!!   Module for type Interf_Perfo_Stats declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the Interf_Perfo_Stats type and allows its allocation and declaration. 
!
!
module interf_perfo_stats_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Interf_Perfo_Stats    &
            ,alloc_Interf_Perfo_Stats   &
            ,dalloc_Interf_Perfo_Stats  &
            ,readInterf_Perfo_Stats     &
            ,writeInterf_Perfo_Stats    
!
   type :: type_Interf_Perfo_Stats
     character(len=500)                               :: filename !< 
     integer(kind=LONG)                               :: N_Sample !< samples number 
     real(kind=DOUBLE)                                :: dTime !< Time sampling (s)
     real(kind=DOUBLE)                                :: dOpd !< Opd sampling (m)
     real(kind=DOUBLE)    , dimension(:), allocatable :: Time !<
     real(kind=DOUBLE)    , dimension(:), allocatable :: Opd !< 
     real(kind=DOUBLE)    , dimension(:), allocatable :: Avg
     real(kind=DOUBLE)    , dimension(:), allocatable :: Std
     real(kind=DOUBLE)    , dimension(:), allocatable :: Min
     real(kind=DOUBLE)    , dimension(:), allocatable :: Max
     integer(kind=LONG)   , dimension(:), allocatable :: NsMin
     integer(kind=LONG)   , dimension(:), allocatable :: NsMax
   end type type_Interf_Perfo_Stats
!
!
   contains
!
!
   subroutine alloc_Interf_Perfo_Stats( Interf_Perfo_Stats )
     implicit none
     type(type_Interf_Perfo_Stats), intent(inout) :: Interf_Perfo_Stats
     integer(kind=LONG)                      :: ErrCode
!
     allocate( Interf_Perfo_Stats%Time(Interf_Perfo_Stats%N_Sample),   stat=ErrCode )
     allocate( Interf_Perfo_Stats%Opd(Interf_Perfo_Stats%N_Sample),    stat=ErrCode )
     allocate( Interf_Perfo_Stats%Avg(Interf_Perfo_Stats%N_Sample),    stat=ErrCode )
     allocate( Interf_Perfo_Stats%Std(Interf_Perfo_Stats%N_Sample),    stat=ErrCode )
     allocate( Interf_Perfo_Stats%Min(Interf_Perfo_Stats%N_Sample),    stat=ErrCode )
     allocate( Interf_Perfo_Stats%Max(Interf_Perfo_Stats%N_Sample),    stat=ErrCode )
     allocate( Interf_Perfo_Stats%NsMin(Interf_Perfo_Stats%N_Sample),  stat=ErrCode )
     allocate( Interf_Perfo_Stats%NsMax(Interf_Perfo_Stats%N_Sample),  stat=ErrCode )
     return
   end subroutine alloc_Interf_Perfo_Stats
!
!
   subroutine dalloc_Interf_Perfo_Stats( Interf_Perfo_Stats )
   implicit none
     type(type_Interf_Perfo_Stats), intent(inout) :: Interf_Perfo_Stats
!
     deallocate( Interf_Perfo_Stats%Time )
     deallocate( Interf_Perfo_Stats%Opd )
     deallocate( Interf_Perfo_Stats%Avg )
     deallocate( Interf_Perfo_Stats%Std )
     deallocate( Interf_Perfo_Stats%Min )
     deallocate( Interf_Perfo_Stats%Max )
     deallocate( Interf_Perfo_Stats%NsMin )
     deallocate( Interf_Perfo_Stats%NsMax )
!
     return
   end subroutine dalloc_Interf_Perfo_Stats
!
!
   subroutine readInterf_Perfo_Stats( Interf_Perfo_Stats, ErrCode )
   implicit none
     type(type_Interf_Perfo_Stats), intent(inout) :: Interf_Perfo_Stats
!
     integer(kind=LONG), intent(out)         :: iostatus
     integer(kind=DOUBLE)                    :: ifile
     integer(kind=LONG)                      :: offset
     integer(kind=LONG)                      :: Type
     integer(kind=LONG)                      :: Size
!
      iostatus = 0
      call open_file_r(Interf_Perfo_Stats%filename                    &
                       (1:len_trim(Interf_Perfo_Stats%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = i4Type
         Size = 4
         call read_field( ifile, Interf_Perfo_Stats%N_Sample,         &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Interf_Perfo_Stats( Interf_Perfo_Stats )
         Type = r8Type
         Size = 8
         call read_field( ifile, Interf_Perfo_Stats%dTime,            &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%dOpd,             &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Interf_Perfo_Stats%N_Sample
         call read_field( ifile, Interf_Perfo_Stats%Time,             &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%Opd,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%Avg,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%Std,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%Min,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%Max,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Interf_Perfo_Stats%N_Sample
         call read_field( ifile, Interf_Perfo_Stats%NsMin,            &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Interf_Perfo_Stats%NsMax,            &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size

         call close_file(Interf_Perfo_Stats%filename                  &
                         (1:len_trim(Interf_Perfo_Stats%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine readInterf_Perfo_Stats
!
!
   subroutine writeInterf_Perfo_Stats( Interf_Perfo_Stats, ErrCode )
   implicit none
     type(type_Interf_Perfo_Stats), intent(inout) :: Interf_Perfo_Stats
!
     integer(kind=LONG), intent(out)         :: iostatus
     integer(kind=DOUBLE)                    :: ifile
     integer(kind=LONG)                      :: offset
     integer(kind=LONG)                      :: Type
     integer(kind=LONG)                      :: Size
!
      iostatus = 0
      call open_file_w(Interf_Perfo_Stats%filename                    &
                       (1:len_trim(Interf_Perfo_Stats%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = i4Type
         Size = 4
         call write_field( ifile, Interf_Perfo_Stats%N_Sample,         &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Interf_Perfo_Stats%dTime,            &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%dOpd,             &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Interf_Perfo_Stats%N_Sample
         call write_field( ifile, Interf_Perfo_Stats%Time,             &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%Opd,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%Avg,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%Std,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%Min,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%Max,              &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4*Interf_Perfo_Stats%N_Sample
         call write_field( ifile, Interf_Perfo_Stats%NsMin,            &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Interf_Perfo_Stats%NsMax,            &
                                  %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size

         call close_file(Interf_Perfo_Stats%filename                  &
                         (1:len_trim(Interf_Perfo_Stats%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine writeInterf_Perfo_Stats
!
!
end module interf_perfo_stats_type

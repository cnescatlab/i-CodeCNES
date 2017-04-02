!!* dc_component_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: april 2011
!!*           Version: $Revision: 1.2 $
!!* Last modification: $Date: 2011-06-22 10:06:27 $
!!
!> type_dc_component -- Module
!!
!! * Purpose
!!
!!   Module for type dc_component declaration and allocation.
!!
!! * Description
!! * References
!!     SPS ATBD

module dc_component_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Dc_Component ,  &
             alloc_Dc_Component,  &
             dalloc_Dc_Component, &
             readdc_component,    &
             writedc_component
!
   type :: type_Dc_Component
     character(len=500)   :: filename     !< Dc_Component file name
     integer(kind=LONG)   :: N_Sample     !< Samples per segment
     integer(kind=LONG)   :: N_Segment    !< number of segment
     real(kind=DOUBLE)    :: Wn           !< Band Wave number
     real(kind=DOUBLE)    :: T            !< Calibration Target Temperature
     real(kind=DOUBLE)    :: B            !< Calibration Target Planck
     real(kind=DOUBLE)    :: DC           !< Full DC
     real(kind=DOUBLE)    :: DC_Cal       !< Full DC calibrated
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Opd      !< DC Opd
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Time     !< DC Time
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Segment  !< DC Value
     real(kind=DOUBLE) ,dimension(:),allocatable   :: Segment_Cal !< DC Cal
   end type type_Dc_Component
!
!
   contains
!
!
   subroutine alloc_Dc_Component( Dc_Component )
   implicit none
     type(type_Dc_Component), intent(inout)         :: Dc_Component
     integer(kind=LONG)                             :: ErrCode
     integer(kind=LONG)                             :: ioalloc
!
     ioalloc = 0
     allocate( Dc_Component%Opd(Dc_Component%N_Segment), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Dc_Component%Time(Dc_Component%N_Segment), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Dc_Component%Segment(Dc_Component%N_Segment), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Dc_Component%Segment_Cal(Dc_Component%N_Segment), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
!
     if( ioalloc > 0 ) then
        write(*,*) 'allocation Dc_Component Error', ErrCode
        write(*,*) 'type_Dc_Component: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine alloc_Dc_Component
!
!
   subroutine dalloc_Dc_Component( Dc_Component )
   implicit none
     type(type_Dc_Component), intent(inout) :: Dc_Component
!
     deallocate( Dc_Component%Opd )
     deallocate( Dc_Component%Time )
     deallocate( Dc_Component%Segment )
     deallocate( Dc_Component%Segment_Cal )
!
     return
   end subroutine dalloc_Dc_Component
!
!
   subroutine readdc_component( Dc_Component, iostatus )
   implicit none
     type(type_Dc_Component), intent(inout)  :: Dc_Component
!
     integer(kind=LONG), intent(out)         :: iostatus
     integer(kind=DOUBLE)                    :: ifile
     integer(kind=LONG)                      :: offset
     integer(kind=LONG)                      :: Type
     integer(kind=LONG)                      :: Size
!
     iostatus = 0
     call open_file_r(Dc_Component%filename               &
                      (1:len_trim(Dc_Component%filename)) &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Dc_Component%N_Sample,               &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%N_Segment,              &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Dc_Component%Wn,                     &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%T,                      &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%B,                      &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%DC,                     &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%DC_Cal,                 &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Dc_Component( Dc_Component )
        Type = r8Type
        Size = 8*Dc_Component%N_Segment
        call read_field( ifile, Dc_Component%Opd,                    &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%Time,                   &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%Segment,                &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Dc_Component%Segment_Cal,            &
                                %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
!
        call close_file(Dc_Component%filename                  &
                        (1:len_trim(Dc_Component%filename))    &
                        // char(0), ifile)
     end if
!
   end subroutine readdc_component
!
!
   subroutine writedc_component( Dc_Component, iostatus )
     implicit none
     type(type_Dc_Component), intent(in) :: Dc_Component
!
     integer(kind=LONG), intent(out)     :: iostatus
     integer(kind=DOUBLE)                :: ifile
     integer(kind=LONG)                  :: offset
     integer(kind=LONG)                  :: Type
     integer(kind=LONG)                  :: Size
!
     iostatus = 0
     call open_file_w(Dc_Component%filename                    &
                      (1:len_trim(Dc_Component%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Dc_Component%N_Sample,               &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%N_Segment,              &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Dc_Component%Wn,                     &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%T,                      &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%B,                      &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%DC,                     &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%DC_Cal,                 &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8*Dc_Component%N_Segment
        call write_field( ifile, Dc_Component%Opd,                    &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%Time,                   &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%Segment,                &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Dc_Component%Segment_Cal,            &
                                 %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
!
        call close_file(Dc_Component%filename                  &
                        (1:len_trim(Dc_Component%filename))    &
                        // char(0), ifile)
     end if
!
     return
   end subroutine writedc_component
!
!
 end module dc_component_type

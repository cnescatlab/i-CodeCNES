!!* pupil_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: december 2010
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-03-07 15:55:00 $
!!
!> type_pupil -- Module
!!
!! * Purpose
!!
!!   Module for pupil declaration and allocation.
!!
!! * Description
!!
!!     type for declaration and allocation of pupil
!!
!! * Sub-routines and functions
!!
!!     * type_Pupil   : type for declaration and allocation of pupil
!!     * alloc_Pupil  : type_Pupil allocation
!!     * dalloc_Pupil : type_Pupil deallocation
!!     * readpupil    : type_Pupil reading routine
!!     * writepupil   : type_Pupil writing routine
!!
!! * References
!!

module pupil_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Pupil   &
            ,alloc_Pupil  &
            ,dalloc_Pupil &
            ,readpupil    &
            ,writepupil   
!
   type :: type_Pupil
     character(len=500)                               :: filename !< pupil characterisation file name 
     integer(kind=LONG)                               :: NbCol !< columns number (Y direction -=>+)
     integer(kind=LONG)                               :: NbLin !< lines number (Z direction +=>-)
     integer(kind=LONG)                               :: NsOpd !< Opd samples number
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Y !< Pupil samples along Y (mm)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Z !< Pupil samples along Z (mm)
     real(kind=DOUBLE) ,dimension(:,:)   ,allocatable :: Wgt !< Weight function - Wgt(1:NbCol,1:NbLin) 
     real(kind=DOUBLE) ,dimension(:,:,:) ,allocatable :: WFE !< Wave Front Error (Y,Z,X)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: AlphaY !< pupil defect (Y)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: AlphaZ !< pupil defect (Z)
  end type type_Pupil
!
   contains
!
!
   subroutine alloc_Pupil( Pupil )
   implicit none
     type(type_Pupil)    , intent(inout)    :: Pupil
     integer(kind=LONG)                     :: ErrCode
!
     allocate( Pupil%Y(Pupil%NbCol),               stat=ErrCode )
     allocate( Pupil%Z(Pupil%NbLin),               stat=ErrCode )
     allocate( Pupil%Wgt(Pupil%NbCol,Pupil%NbLin), stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Pupil Error'
        write(0,*) 'Pupil: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Pupil
!
!
   subroutine dalloc_Pupil( Pupil )
   implicit none
     type(type_Pupil), intent(inout)      :: Pupil
!
     deallocate( Pupil%Y )
     deallocate( Pupil%Z )
     deallocate( Pupil%Wgt )
!
   end subroutine dalloc_Pupil
!
!
   subroutine readpupil( Pupil,iostatus )
   implicit none
     type(type_Pupil)    , intent(inout) :: Pupil
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Pupil%filename                    &
                      (1:len_trim(Pupil%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Pupil%NbCol,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Pupil%NbLin,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Pupil%NsOpd,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
       call alloc_Pupil( Pupil )
        Type = r8Type
        Size = 8*Pupil%NbCol
        call read_field( ifile, Pupil%Y,                        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Pupil%NbLin
        call read_field( ifile, Pupil%Z,                        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Pupil%NbCol*Pupil%NbLin
        call read_field( ifile, Pupil%Wgt,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Pupil%NbCol*Pupil%NbLin*Pupil%NsOpd
        call read_field( ifile, Pupil%WFE,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
       call close_file(Pupil%filename              &
                        (1:len_trim(Pupil%filename))&
                        // char(0), ifile)
     end if
     return
   end subroutine readpupil
!
!
  subroutine writepupil( Pupil,iostatus )
  implicit none
     type(type_Pupil)    , intent(in)  :: Pupil
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
     iostatus = 0
     call open_file_w(Pupil%filename                    &
                      (1:len_trim(Pupil%filename))      &
                       // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Pupil%NbCol,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Pupil%NbLin,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        call write_field( ifile, Pupil%NsOpd,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8
        Size = 8*Pupil%NbCol
        call write_field( ifile, Pupil%Y,                        &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Pupil%NbLin
        call write_field( ifile, Pupil%Z,                        &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Pupil%NbCol*Pupil%NbLin
        call write_field( ifile, Pupil%Wgt,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Pupil%NbCol*Pupil%NbLin*Pupil%NsOpd
        call write_field( ifile, Pupil%WFE,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
       call close_file(Pupil%filename              &
                        (1:len_trim(Pupil%filename))&
                        // char(0), ifile)
     end if
     return
   end subroutine writepupil
!
!
 end module pupil_type

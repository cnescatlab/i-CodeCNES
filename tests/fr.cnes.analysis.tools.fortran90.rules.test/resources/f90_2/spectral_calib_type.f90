! spectral_calib_type.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: mars 2010
!           Version: $Revision: 1.5 $
! Last modification: $Date: 2011-03-07 15:55:00 $
!
! type_shift in the Fourier domain-- Module
!
! * Purpose
!   Module for type spectral shift FT parameters declaration and allocation.
!
module spectral_calib_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Spectral_Calib,     &
             type_Spectral_Calib_SW,  &
             type_Spectral_Calib_PN,  &
             type_Spectral_Calib_SB,  &
             alloc_Spectral_Calib_SW, &
             dalloc_Spectral_Calib_SW,&
             alloc_Spectral_Calib_PN, &
             dalloc_Spectral_Calib_PN,&
             alloc_Spectral_Calib_SB, &
             dalloc_Spectral_Calib_SB,&
             readspectral_calib,      &
             writespectral_calib
!
   type :: type_Spectral_Calib
     character(len=500)                                  :: filename
     integer(kind=LONG)                                  :: Last_Iteration
     integer(kind=LONG)                                  :: FlagShiftQual
     integer(kind=LONG)                                  :: FlagConver
     real(kind=DOUBLE)                                   :: WnShiftQual
     real(kind=DOUBLE)                                   :: Wn
     real(kind=DOUBLE)                                   :: WnShift
     real(kind=DOUBLE)                                   :: WnShift_Expected
     real(kind=DOUBLE)                                   :: CoefCal
     real(kind=DOUBLE)                                   :: CoefCal0
   end type type_Spectral_Calib
!
!
   type :: type_Spectral_Calib_SW
     integer(kind=LONG)                                   :: NbSW
     type(type_Spectral_Calib),dimension(:),allocatable   :: Spectral_Calib
     !dimension(NbSW)
   end type type_Spectral_Calib_SW
!
!
   type :: type_Spectral_Calib_PN
     integer(kind=LONG)                                    :: NbPixel
     type(type_Spectral_Calib_SW),dimension(:),allocatable :: Spectral_Calib_SW
     !dimension(NbPixel)
   end type type_Spectral_Calib_PN
!
!
   type :: type_Spectral_Calib_SB
     integer(kind=LONG)                                    :: Nband
     type(type_Spectral_Calib_PN),dimension(:),allocatable :: Spectral_Calib_PN
     ! dimension(nband)
   end type type_Spectral_Calib_SB
!if (allocated(Spectral_Calib_SW_PN(iband)) deallocate(Spectral_Calib_SW_PN(iband))
!
   contains
!
!
   subroutine alloc_Spectral_Calib_SW( Spectral_Calib_SW )
   implicit none
     type(type_Spectral_Calib_SW), intent(inout)   :: Spectral_Calib_SW
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Spectral_Calib_SW%Spectral_Calib(Spectral_Calib_SW%NbSW), &
                                                            stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectral_Calib_SW Error',ErrCode
        write(*,*) 'type_Spectral_Calib_SW: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectral_Calib_SW
!
!
   subroutine alloc_Spectral_Calib_PN( Spectral_Calib_PN )
   implicit none
     type(type_Spectral_Calib_PN), intent(inout)   :: Spectral_Calib_PN
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Spectral_Calib_PN%Spectral_Calib_SW(Spectral_Calib_PN%NbPixel),&
                                                                 stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectral_Calib_PN Error',ErrCode
        write(*,*) 'type_Spectral_Calib_PN: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectral_Calib_PN
!
!
   subroutine alloc_Spectral_Calib_SB( Spectral_Calib_SB )
   implicit none
     type(type_Spectral_Calib_SB), intent(inout)   :: Spectral_Calib_SB
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Spectral_Calib_SB%Spectral_Calib_PN(Spectral_Calib_SB%Nband),&
                                                               stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Spectral_Calib_SB Error',ErrCode
        write(*,*) 'type_Spectral_Calib_SB: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Spectral_Calib_SB
!
!
   subroutine dalloc_Spectral_Calib_SW( Spectral_Calib_SW )
   implicit none
     type(type_Spectral_Calib_SW), intent(inout) :: Spectral_Calib_SW
!
     deallocate(Spectral_Calib_SW%Spectral_Calib)
     return
   end subroutine dalloc_Spectral_Calib_SW
!
!
   subroutine dalloc_Spectral_Calib_PN( Spectral_Calib_PN )
   implicit none
     type(type_Spectral_Calib_PN), intent(inout) :: Spectral_Calib_PN
!
     deallocate(Spectral_Calib_PN%Spectral_Calib_SW)
     return
   end subroutine dalloc_Spectral_Calib_PN
!
!
   subroutine dalloc_Spectral_Calib_SB( Spectral_Calib_SB )
   implicit none
     type(type_Spectral_Calib_SB), intent(inout) :: Spectral_Calib_SB
!
     deallocate(Spectral_Calib_SB%Spectral_Calib_PN)
     return
   end subroutine dalloc_Spectral_Calib_SB
!
!
   subroutine readspectral_calib( Spectral_Calib,iostatus )
   implicit none
     type(type_Spectral_Calib), intent(inout) :: Spectral_Calib
!
     integer(kind=LONG)         , intent(out) :: iostatus
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
     iostatus = 0
     call open_file_r(Spectral_Calib%filename              &
                      (1:len_trim(Spectral_Calib%filename))&
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Spectral_Calib%Last_Iteration,&
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%FlagShiftQual, &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%FlagConver,    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Spectral_Calib%WnShiftQual,   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%Wn,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%WnShift,       &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%WnShift_Expected,&
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%CoefCal,       &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Spectral_Calib%CoefCal0,      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Spectral_Calib%filename              &
                        (1:len_trim(Spectral_Calib%filename))&
                        // char(0), ifile)
     end if
     return
   end subroutine readspectral_calib
!
!
   subroutine writespectral_calib( Spectral_Calib,iostatus )
   implicit none
     type(type_Spectral_Calib)  , intent(in)  :: Spectral_Calib
!
     integer(kind=LONG)         , intent(out) :: iostatus
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
     iostatus = 0
     call open_file_w(Spectral_Calib%filename              &
                      (1:len_trim(Spectral_Calib%filename))&
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Spectral_Calib%Last_Iteration,&
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%FlagShiftQual, &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%FlagConver,    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Spectral_Calib%WnShiftQual,   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%Wn,            &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%WnShift,       &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%WnShift_Expected,&
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%CoefCal,       &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Spectral_Calib%CoefCal0,      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Spectral_Calib%filename              &
                        (1:len_trim(Spectral_Calib%filename))&
                        // char(0), ifile)
     end if
     return
   end subroutine writespectral_calib
!
!
end module spectral_calib_type

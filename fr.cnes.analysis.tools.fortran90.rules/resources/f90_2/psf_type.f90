!!* psf_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.5 $
!!* Last modification: $Date: 2011-11-02 14:57:53 $
!!
!> psf_type -- Module
!!
!! * Purpose
!!
!!   Module for type psf (Point Spread Function) declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the psf type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Psf    : type for declaration and allocation of Psf
!!     * alloc_Psf   : type_Psf allocation
!!     * dalloc_Psf  : type_Psf deallocation
!!     * readpsf     : Psf reading routine
!!     * writepsf    : Psf writing routine
!!
!! * References
!!


module psf_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Psf   &
            ,alloc_Psf  &
            ,dalloc_Psf &
            ,readpsf    &
            ,writepsf   
!
   type :: type_Psf
     character(len=500)                               :: filename !< Psf file name
     integer(kind=LONG)                               :: NsWn !< wavenumbers number
     integer(kind=LONG)                               :: NbCol !< columns number
     integer(kind=LONG)                               :: NbLin !< lines number
     integer(kind=LONG)                               :: NSamp !< dimensions of the undersampled PSF
     real(kind=DOUBLE)                                :: H_Sat !< Satellite altitude 
     real(kind=DOUBLE)                                :: MagnificationY !< magnification along Y axis 
     real(kind=DOUBLE)                                :: MagnificationZ !< magnification along Z axis 
     real(kind=DOUBLE)                                :: PixelPosY !< pixel position along Y axis 
     real(kind=DOUBLE)                                :: PixelPosZ !< pixel position along Z axis 
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: BaryCentreY !< PSF barycentre along Y-axis - BaryCentreY(1:NsWn)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: BaryCentreZ !< PSF barycentre along Z-axis - BaryCentreZ(1:NsWn) 
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: FieldMeanAngle !< mean field angle of the PSF - FieldMeanAngle(1:NsWn)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Wn !< wavenumber - Wn(1:NsWn)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Y !< PSF position along Y-axis - Y(1:NbCol) 
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: Z !< PSF position along Y-axis - Z(1:NbLin)
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: AY !< PSF position along Y-axis - Y(1:NbCol) 
     real(kind=DOUBLE) ,dimension(:)     ,allocatable :: AZ !< PSF angular position along Y-axis - Z(1:NbLin)
     real(kind=DOUBLE) ,dimension(:,:,:) ,allocatable :: Wgt ! normalised weight of the PSF - Wgt(1:NbCol,1:NbLin,1:NsWn)
     real(kind=DOUBLE) ,dimension(:,:,:) ,allocatable :: AWgt ! normalised weight of the PSF - Wgt(1:NbCol,1:NbLin,1:NsWn)
   end type type_Psf
!
   contains
!
!
   subroutine alloc_Psf( Psf )
   implicit none
     type(type_Psf)    , intent(inout)      :: Psf
     integer(kind=LONG)                     :: ErrCode
!
     allocate( Psf%BaryCentreY(Psf%NsWn),             stat=ErrCode )
     allocate( Psf%BaryCentreZ(Psf%NsWn),             stat=ErrCode )
     allocate( Psf%FieldMeanAngle(Psf%NsWn),          stat=ErrCode )
     allocate( Psf%Wn(Psf%NsWn),                      stat=ErrCode )
     allocate( Psf%Y(Psf%NbCol),                      stat=ErrCode )
     allocate( Psf%Z(Psf%NbLin),                      stat=ErrCode )
     allocate( Psf%AY(Psf%NbCol),                     stat=ErrCode )
     allocate( Psf%AZ(Psf%NbLin),                     stat=ErrCode )
     allocate( Psf%Wgt(Psf%NbCol,Psf%NbLin,Psf%NsWn), stat=ErrCode )
     allocate( Psf%AWgt(Psf%NbCol,Psf%NbLin,Psf%NsWn),stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Psf Error'
        write(0,*) 'Psf: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Psf
!
!
   subroutine dalloc_Psf( Psf )
   implicit none
     type(type_Psf), intent(inout)      :: Psf
!
     deallocate( Psf%BaryCentreY )
     deallocate( Psf%BaryCentreZ )
     deallocate( Psf%FieldMeanAngle )
     deallocate( Psf%Y )
     deallocate( Psf%Z )
     deallocate( Psf%AY )
     deallocate( Psf%AZ )
     deallocate( Psf%Wn )
     deallocate( Psf%Wgt )
     deallocate( Psf%AWgt )
!
   end subroutine dalloc_Psf
!
!
   subroutine readpsf( Psf,iostatus )
   implicit none
     type(type_Psf)    , intent(inout) :: Psf
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Psf%filename                    &
                      (1:len_trim(Psf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Psf%NsWn,                     &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%NbCol,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%NbLin,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%NSamp,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Psf( Psf )
        Type = r8Type
        Size = 8
        call read_field( ifile, Psf%H_Sat,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%MagnificationY,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%MagnificationZ,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%PixelPosY,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%PixelPosZ,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NsWn
        call read_field( ifile, Psf%BaryCentreY,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%BaryCentreZ,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%FieldMeanAngle,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbCol
        call read_field( ifile, Psf%Y,                        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbLin
        call read_field( ifile, Psf%Z,                        &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbCol
        call read_field( ifile, Psf%AY,                       &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbLin
        call read_field( ifile, Psf%AZ,                       &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NsWn
        call read_field( ifile, Psf%Wn,                       &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbCol*Psf%NbLin*Psf%NsWn
        call read_field( ifile, Psf%Wgt,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Psf%AWgt,                     &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Psf%filename              &
                        (1:len_trim(Psf%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine readpsf
!
!
  subroutine writepsf( Psf,iostatus )
  implicit none
     type(type_Psf)    , intent(in)  :: Psf
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
     iostatus = 0
     call open_file_w(Psf%filename                    &
                      (1:len_trim(Psf%filename))      &
                       // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Psf%NsWn,                     &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%NbCol,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%NbLin,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%NSamp,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Psf%H_Sat,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%MagnificationY,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%MagnificationZ,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%PixelPosY,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%PixelPosZ,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NsWn
        call write_field( ifile, Psf%BaryCentreY,              &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%BaryCentreZ,              &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%FieldMeanAngle,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbCol
        call write_field( ifile, Psf%Y,                        &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbLin
        call write_field( ifile, Psf%Z,                        &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbCol
        call write_field( ifile, Psf%AY,                       &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbLin
        call write_field( ifile, Psf%AZ,                       &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NsWn
        call write_field( ifile, Psf%Wn,                       &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Psf%NbCol*Psf%NbLin*Psf%NsWn
        call write_field( ifile, Psf%Wgt,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Psf%AWgt,                     &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Psf%filename              &
                        (1:len_trim(Psf%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine writepsf
!
!
end module psf_type

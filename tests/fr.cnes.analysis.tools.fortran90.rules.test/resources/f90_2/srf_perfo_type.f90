!!* srf_perfo_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: november 2013
!!
!> srf_perfo_type -- Module
!!
!! * Purpose
!!
!!   Module for type srf_perfo declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the srf_perfo type
!!
!! * Sub-routines and functions
!!
!!     * type_Srf_Perfo    : type for declaration and allocation
!!     * alloc_Srf_Perfo   : type_Srf_Perfo allocation
!!     * dalloc_Srf_Perfo  : type_Srf_Perfo deallocation
!!     * readsrf_perfo     : Srf reading routine
!!     * writesrf_erfo    : Srf writing routine
!!
!! * References
!!

module srf_perfo_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Srf_Perfo  &
            ,alloc_Srf_Perfo &
            ,dalloc_Srf_Perfo&
            ,readsrf_perfo   &
            ,writesrf_perfo   
!
   type :: type_Srf_Perfo
     character(len=500)                               :: filename !< Srf file name
     integer(kind=LONG)                               :: NsPixel !< number of sub-pixel
     integer(kind=LONG)                               :: NsWn0 !< central wavenumber number sample id
     integer(kind=LONG)                               :: NsSDWn1a !< central wavenumber number sample id for spectral domain level 1a
     integer(kind=LONG)                               :: NsSDWn1b !< central wavenumber number sample id for spectral domain level 1b
     integer(kind=LONG)                               :: NsSDWn1c !< central wavenumber number sample id for spectral domain level 1c
     real(kind=DOUBLE)                                :: SDWnMax1a !< maximum wavenumber for spectral domain level 1a (m-1) 
     real(kind=DOUBLE)                                :: SDWnMax1b !< maximum wavenumber for spectral domain level 1b (m-1) 
     real(kind=DOUBLE)                                :: SDWnMax1c !< maximum wavenumber for spectral domain level 1c (m-1) 
     real(kind=DOUBLE)                                :: dWn1a !< level 1a wavenumber sampling (m-1) 
     real(kind=DOUBLE)                                :: dWn1b !< level 1b wavenumber sampling (m-1) 
     real(kind=DOUBLE)                                :: dWn1c !< level 1c wavenumber sampling (m-1) 
     real(kind=DOUBLE)                                :: Opd_effective !< effective OPD (m)
     integer(kind=LONG)                               :: SigI !< samples number of the smoothing function
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Wn0 !< wavenumber base (m-1) - Wn0(1:NsWn0)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: SDWn1a !< level 1a wavenumber base (m-1) - SDWn1a(1:NsSDWn1a)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: SDWn1b !< level 1b wavenumber base (m-1) - SDWn1b(1:NsSDWn1b) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: SDWn1c !< level 1c wavenumber base (m-1) - SDWn1c(1:NsSDWn1c)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ErShift1a !<
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ErShift1b !<
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ErShift1c !< 
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ErFWhm1a  !<
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ErFWhm1b  !<
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ErFWhm1c  !< 
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ShapeId1a !<
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ShapeId1b !<
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: ShapeId1c !< 
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: ErShape1a !<
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: ErShape1b !< 
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: ErShape1c !< 
   end type type_Srf_Perfo
!
!
   contains
!
!
   subroutine alloc_Srf_Perfo( Srf_Perfo )
   implicit none
     type(type_Srf_Perfo), intent(inout)      :: Srf_Perfo
     integer(kind=LONG)                       :: ErrCode
     integer(kind=LONG)                       :: ioalloc
!
     ioalloc = 0
     allocate( Srf_Perfo%Wn0(Srf_Perfo%NsWn0),          stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%SDWn1a(Srf_Perfo%NsSDWn1a),    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%SDWn1b(Srf_Perfo%NsSDWn1b),    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%SDWn1c(Srf_Perfo%NsSDWn1c),    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErShift1a(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErShift1b(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErShift1c(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErFWhm1a(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                          stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErFWhm1b(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                          stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErFWhm1c(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                          stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ShapeId1a(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ShapeId1b(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ShapeId1c(Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErShape1a(Srf_Perfo%NsSDWn1a, &
                                   Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErShape1b(Srf_Perfo%NsSDWn1b, &
                                   Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Srf_Perfo%ErShape1c(Srf_Perfo%NsSDWn1c, &
                                   Srf_Perfo%NsWn0,Srf_Perfo%NsPixel+1),&
                                                           stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
!
     if (ioalloc > 0) then
        write(0,*) 'allocation Srf_Perfo Error'
        write(0,*) 'Srf_Perfo: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Srf_Perfo
!
!
   subroutine dalloc_Srf_Perfo( Srf_Perfo )
   implicit none
     type(type_Srf_Perfo)    , intent(inout)      :: Srf_Perfo
     deallocate( Srf_Perfo%Wn0 )
     deallocate( Srf_Perfo%SDWn1a )
     deallocate( Srf_Perfo%SDWn1b )
     deallocate( Srf_Perfo%SDWn1c )
     deallocate( Srf_Perfo%ErShift1a )
     deallocate( Srf_Perfo%ErShift1b )
     deallocate( Srf_Perfo%ErShift1c )
     deallocate( Srf_Perfo%ErFWhm1a )
     deallocate( Srf_Perfo%ErFWhm1b )
     deallocate( Srf_Perfo%ErFWhm1c )
     deallocate( Srf_Perfo%ShapeId1a )
     deallocate( Srf_Perfo%ShapeId1b )
     deallocate( Srf_Perfo%ShapeId1c )
     deallocate( Srf_Perfo%ErShape1a )
     deallocate( Srf_Perfo%ErShape1b )
     deallocate( Srf_Perfo%ErShape1c )
     return
   end subroutine dalloc_Srf_Perfo
!
!
   subroutine readsrf_perfo( Srf_Perfo,iostatus )
   implicit none
     type(type_Srf_Perfo)    , intent(inout) :: Srf_Perfo
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Srf_Perfo%filename                    &
                      (1:len_trim(Srf_Perfo%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Srf_Perfo%NsPixel,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%NsWn0,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%NsSDWn1a,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%NsSDWn1b,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%NsSDWn1c,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Srf_Perfo%SDWnMax1a,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%SDWnMax1b,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%SDWnMax1c,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%dWn1a,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%dWn1b,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%dWn1c,              &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%Opd_effective,      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4
        call read_field( ifile, Srf_Perfo%SigI,               &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Srf_Perfo( Srf_Perfo )
        Type = r8Type
        Size = 8*Srf_Perfo%NsWn0
        call read_field( ifile, Srf_Perfo%Wn0,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1a
        call read_field( ifile, Srf_Perfo%SDWn1a,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1b
        call read_field( ifile, Srf_Perfo%SDWn1b,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1c
        call read_field( ifile, Srf_Perfo%SDWn1c,             &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call read_field( ifile, Srf_Perfo%ErShift1a,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ErShift1b,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ErShift1c,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ErFWhm1a,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ErFWhm1b,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ErFWhm1c,           &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ShapeId1a,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ShapeId1b,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf_Perfo%ShapeId1c,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1a*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call read_field( ifile, Srf_Perfo%ErShape1a,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1b*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call read_field( ifile, Srf_Perfo%ErShape1b,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1c*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call read_field( ifile, Srf_Perfo%ErShape1c,          &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Srf_Perfo%filename              &
                        (1:len_trim(Srf_Perfo%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine readsrf_perfo
!
!
   subroutine writesrf_perfo( Srf_Perfo,iostatus )
   implicit none
     type(type_Srf_Perfo)    , intent(in) :: Srf_Perfo
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_w(Srf_Perfo%filename                    &
                      (1:len_trim(Srf_Perfo%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Srf_Perfo%NsPixel,            &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%NsWn0,              &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%NsSDWn1a,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%NsSDWn1b,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%NsSDWn1c,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Srf_Perfo%SDWnMax1a,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%SDWnMax1b,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%SDWnMax1c,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%dWn1a,              &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%dWn1b,              &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%dWn1c,              &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%Opd_effective,      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4
        call write_field( ifile, Srf_Perfo%SigI,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
!
        Type = r8Type
        Size = 8*Srf_Perfo%NsWn0
        call write_field( ifile, Srf_Perfo%Wn0,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1a
        call write_field( ifile, Srf_Perfo%SDWn1a,             &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1b
        call write_field( ifile, Srf_Perfo%SDWn1b,             &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1c
        call write_field( ifile, Srf_Perfo%SDWn1c,             &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call write_field( ifile, Srf_Perfo%ErShift1a,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ErShift1b,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ErShift1c,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ErFWhm1a,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ErFWhm1b,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ErFWhm1c,           &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ShapeId1a,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ShapeId1b,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf_Perfo%ShapeId1c,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1a*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call write_field( ifile, Srf_Perfo%ErShape1a,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1b*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call write_field( ifile, Srf_Perfo%ErShape1b,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf_Perfo%NsSDWn1c*Srf_Perfo%NsWn0*(Srf_Perfo%NsPixel+1)
        call write_field( ifile, Srf_Perfo%ErShape1c,          &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Srf_Perfo%filename              &
                        (1:len_trim(Srf_Perfo%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine writesrf_perfo
!
!
end module srf_perfo_type

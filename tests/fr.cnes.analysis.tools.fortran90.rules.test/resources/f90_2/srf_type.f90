!!* srf_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.10 $
!!* Last modification: $Date: 2011-06-22 10:06:28 $
!!
!> srf_type -- Module
!!
!! * Purpose
!!
!!   Module for type srf (Spectral Response Function) declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the srf type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Srf    : type for declaration and allocation of Srf
!!     * alloc_Srf   : type_Srf allocation
!!     * dalloc_Srf  : type_Srf deallocation
!!     * Srf_Base_1a : computes Srf 1a spectral base in wavenumber
!!     * Srf_Base_1b : computes Srf 1b spectral base in wavenumber
!!     * Srf_Base_1c : computes Srf 1c spectral base in wavenumber
!!     * Srf_Header_Transfer : header transfer between to Srf types
!!     * readsrf     : Srf reading routine
!!     * writesrf    : Srf writing routine
!!     * writesrf_netcdf : type_Srf writing routine (netCDF format)
!!     * write_srf_data  : type_Srf writing routine (netCDF format)
!!
!! * References
!!

module srf_type
   use precision_type
   use error_type
   use isrf_index_type
   use constantes_type
   use modinouttools
   use modionetcdf
   use netcdf
!
   implicit none
!
!
   public :: type_Srf        &
            ,type_Srf_ST     &
            ,type_Srf_SB     &
            ,type_Srf_PN     &
            ,alloc_Srf       &
            ,alloc_Srf_ST    &
            ,alloc_Srf_SB    &
            ,alloc_Srf_PN    &
            ,dalloc_Srf      &
            ,dalloc_Srf_ST   &
            ,dalloc_Srf_SB   &
            ,dalloc_Srf_PN   &
            ,Srf_Base_1a     &
            ,Srf_Base_1b     &
            ,Srf_Base_1c     &
            ,Srf_Header_Transfer&
            ,readsrf         &
            ,writesrf        &
            ,readsrf_netcdf  &
            ,writesrf_netcdf
!
   type :: type_Srf
     type(type_isrf_index)                            :: TopLevelSrf ! < srf top level indexes
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
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: L1a !< Spectral response function level 1a - L1a(1:NsSDWn1a,1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: L1b !< Spectral response function level 1b - L1b(1:NsSDWn1b,1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:,:),allocatable  :: L1c !< Spectral response function level 1c - L1c(1:NsSDWn1c,1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: L1cTF !< Spectral response function level 1c in Fourier Space - L1cTF(1:NsSDWn1c)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: WnShift1a !< wavenumber shift for level 1a - WnShift1a(1:NsWn0,1:NsPixel+1) 
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: WnShift1b !< wavenumber shift for level 1b - WnShift1b(1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: WnShift1c !< wavenumber shift for level 1c - WnShift1c(1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: Fcs1a !< spectral calibration function for level 1a - Fcs1a(1:NsWn0,1:NsPixel+1) 
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: Fcs1b !< spectral calibration function for level 1b - Fcs1b(1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: Fcs1c !< spectral calibration function for level 1c - Fcs1c(1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: FWhm1a !< Full Width at Half Maximum of the Gauss function for level 1a - FWhm1a(1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: FWhm1b !< Full Width at Half Maximum of the Gauss function for level 1b - FWhm1b(1:NsWn0,1:NsPixel+1)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: FWhm1c !< Full Width at Half Maximum of the Gauss function for level 1c - FWhm1c(1:NsWn0,1:NsPixel+1)
   end type type_Srf
!
!
   type :: type_Srf_ST
     integer(kind=LONG)                                   :: NbState
     type(type_Srf),dimension(:),allocatable              :: Srf
     !dimension(NbState)
   end type type_Srf_ST
!
!
   type :: type_Srf_PN
     integer(kind=LONG)                                   :: NbPixel
     type(type_Srf_ST),dimension(:),allocatable           :: Srf_ST
     !dimension(NbPixel)
   end type type_Srf_PN
!
!
   type :: type_Srf_SB
     integer(kind=LONG)                                   :: Nband
     type(type_Srf_PN),dimension(:),allocatable           :: Srf_PN
     !dimension(Nband)
   end type type_Srf_SB
!
!
   contains
!
!
   subroutine alloc_Srf_ST( Srf_ST )
   implicit none
     type(type_Srf_ST), intent(inout)              :: Srf_ST
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Srf_ST%Srf(Srf_ST%NbState),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Srf_ST Error',ErrCode
        write(*,*) 'type_Srf_ST: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Srf_ST
!
!
   subroutine alloc_Srf_PN( Srf_PN )
   implicit none
     type(type_Srf_PN), intent(inout)              :: Srf_PN
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Srf_PN%Srf_ST(Srf_PN%NbPixel),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Srf_PN Error',ErrCode
        write(*,*) 'type_Srf_PN: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Srf_PN
!
!
   subroutine alloc_Srf_SB( Srf_SB )
   implicit none
     type(type_Srf_SB), intent(inout)              :: Srf_SB
     integer(kind=LONG)                            :: ErrCode
!
     allocate( Srf_SB%Srf_PN(Srf_SB%Nband),  stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Srf_SB Error',ErrCode
        write(*,*) 'type_Srf_SB: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Srf_SB
!
!
   subroutine alloc_Srf( Srf )
   implicit none
     type(type_Srf)    , intent(inout)      :: Srf
     integer(kind=LONG)                     :: ErrCode, i
!
     allocate( Srf%TopLevelSrf%SubFov(Srf%NsPixel),        stat=ErrCode )
     ! Srf%TopLevelSrf%SubFov is the list of sub-pixel index (1:NsPixel)
     do i=1,Srf%NsPixel
        Srf%TopLevelSrf%SubFov(i) = i
     end do
     allocate( Srf%Wn0(Srf%NsWn0),                            stat=ErrCode )
     allocate( Srf%SDWn1a(Srf%NsSDWn1a),                      stat=ErrCode )
     allocate( Srf%SDWn1b(Srf%NsSDWn1b),                      stat=ErrCode )
     allocate( Srf%SDWn1c(Srf%NsSDWn1c),                      stat=ErrCode )
     allocate( Srf%L1a(Srf%NsSDWn1a,Srf%NsWn0,Srf%NsPixel+1), stat=ErrCode )
     allocate( Srf%L1b(Srf%NsSDWn1b,Srf%NsWn0,Srf%NsPixel+1), stat=ErrCode )
     allocate( Srf%L1c(Srf%NsSDWn1c,Srf%NsWn0,Srf%NsPixel+1), stat=ErrCode )
     allocate( Srf%L1cTF(Srf%NsSDWn1c),                       stat=ErrCode )
     allocate( Srf%WnShift1a(Srf%NsWn0,Srf%NsPixel+1),        stat=ErrCode )
     allocate( Srf%WnShift1b(Srf%NsWn0,Srf%NsPixel+1),        stat=ErrCode )
     allocate( Srf%WnShift1c(Srf%NsWn0,Srf%NsPixel+1),        stat=ErrCode )
     allocate( Srf%Fcs1a(Srf%NsWn0,Srf%NsPixel+1),            stat=ErrCode )
     allocate( Srf%Fcs1b(Srf%NsWn0,Srf%NsPixel+1),            stat=ErrCode )
     allocate( Srf%Fcs1c(Srf%NsWn0,Srf%NsPixel+1),            stat=ErrCode )
     allocate( Srf%FWhm1a(Srf%NsWn0,Srf%NsPixel+1),           stat=ErrCode )
     allocate( Srf%FWhm1b(Srf%NsWn0,Srf%NsPixel+1),           stat=ErrCode )
     allocate( Srf%FWhm1c(Srf%NsWn0,Srf%NsPixel+1),           stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Srf Error'
        write(0,*) 'Srf: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Srf
!
!
   subroutine dalloc_Srf_ST( Srf_ST )
   implicit none
     type(type_Srf_ST), intent(inout)              :: Srf_ST
!
     deallocate( Srf_ST%Srf )
     return
   end subroutine dalloc_Srf_ST
!
!
   subroutine dalloc_Srf_PN( Srf_PN )
   implicit none
     type(type_Srf_PN), intent(inout)              :: Srf_PN
!
     deallocate( Srf_PN%Srf_ST )
     return
   end subroutine dalloc_Srf_PN
!
!
   subroutine dalloc_Srf_SB( Srf_SB )
   implicit none
     type(type_Srf_SB), intent(inout)              :: Srf_SB
!
     deallocate( Srf_SB%Srf_PN )
     return
   end subroutine dalloc_Srf_SB
!
!
   subroutine dalloc_Srf( Srf )
   implicit none
     type(type_Srf)    , intent(inout)         :: Srf
!
     deallocate( Srf%TopLevelSrf%SubFov )
     deallocate( Srf%Wn0 )
     deallocate( Srf%SDWn1a )
     deallocate( Srf%SDWn1b )
     deallocate( Srf%SDWn1c )
     deallocate( Srf%L1a )
     deallocate( Srf%L1b )
     deallocate( Srf%L1c )
     deallocate( Srf%L1cTF )
     deallocate( Srf%WnShift1a )
     deallocate( Srf%WnShift1b )
     deallocate( Srf%WnShift1c )
     deallocate( Srf%Fcs1a )
     deallocate( Srf%Fcs1b )
     deallocate( Srf%Fcs1c )
     deallocate( Srf%FWhm1a )
     deallocate( Srf%FWhm1b )
     deallocate( Srf%FWhm1c )
     return
   end subroutine dalloc_Srf
!
!
   subroutine Srf_Base_1a( Srf )
   implicit none
     type(type_Srf)    , intent(inout)                :: Srf
     integer(kind=LONG)                               :: NsSDWn1a0
     integer(kind=LONG)                               :: Ns
!
     NsSDWn1a0 = int( Srf%NsSDWn1a/2 ) + 1
     Srf%SDWn1a(1:Srf%NsSDWn1a) = &
              (/ (dble(Ns-NsSDWn1a0),Ns=1,Srf%NsSDWn1a) /) * Srf%dWn1a
!
     return
   end subroutine Srf_Base_1a
!
!
   subroutine Srf_Base_1b( Srf )
   implicit none
     type(type_Srf)    , intent(inout)                :: Srf
     integer(kind=LONG)                               :: NsSDWn1b0
     integer(kind=LONG)                               :: Ns
!
     NsSDWn1b0 = int( Srf%NsSDWn1b/2 ) + 1
     Srf%SDWn1b(1:Srf%NsSDWn1b) = &
              (/ (dble(Ns-NsSDWn1b0),Ns=1,Srf%NsSDWn1b) /) * Srf%dWn1b
!
     return
   end subroutine Srf_Base_1b
!
!
   subroutine Srf_Base_1c( Srf )
   implicit none
     type(type_Srf)    , intent(inout)                :: Srf
     integer(kind=LONG)                               :: NsSDWn1c0
     integer(kind=LONG)                               :: Ns
!
     NsSDWn1c0 = int( Srf%NsSDWn1c/2 ) + 1
     Srf%SDWn1c(1:Srf%NsSDWn1c) = &
              (/ (dble(Ns-NsSDWn1c0),Ns=1,Srf%NsSDWn1c) /) * Srf%dWn1c
!
     return
   end subroutine Srf_Base_1c
!
!
   subroutine Srf_Header_Transfer( Srf_In, Srf_Out )
   implicit none
     type(type_Srf)    , intent(in)                   :: Srf_In
     type(type_Srf)    , intent(inout)                :: Srf_Out
!
     Srf_Out%NsPixel    = Srf_In%NsPixel
     Srf_Out%NsWn0      = Srf_In%NsWn0
     Srf_Out%NsSDWn1a   = Srf_In%NsSDWn1a
     Srf_Out%NsSDWn1b   = Srf_In%NsSDWn1b
     Srf_Out%NsSDWn1c   = Srf_In%NsSDWn1c
     call alloc_Srf( Srf_Out )
     Srf_Out = Srf_In
     Srf_Out%L1a        = 0.d+00
     Srf_Out%L1b        = 0.d+00
     Srf_Out%L1c        = 0.d+00
     Srf_Out%L1cTF      = 0.d+00
     Srf_Out%WnShift1a  = 0.d+00
     Srf_Out%WnShift1b  = 0.d+00
     Srf_Out%WnShift1c  = 0.d+00
     Srf_Out%Fcs1a      = 0.d+00
     Srf_Out%Fcs1b      = 0.d+00
     Srf_Out%Fcs1c      = 0.d+00
     Srf_Out%FWhm1a     = 0.d+00
     Srf_Out%FWhm1b     = 0.d+00
     Srf_Out%FWhm1c     = 0.d+00
!
     return
   end subroutine Srf_Header_Transfer
!
!
   subroutine readsrf( Srf,iostatus )
   implicit none
     type(type_Srf)    , intent(inout) :: Srf
!
     integer(kind=LONG), intent(out)   :: iostatus
     integer(kind=DOUBLE)              :: ifile
     integer(kind=LONG)                :: offset
     integer(kind=LONG)                :: Type
     integer(kind=LONG)                :: Size
!
     iostatus = 0
     call open_file_r(Srf%filename                    &
                      (1:len_trim(Srf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call read_field( ifile, Srf%NsPixel,                  &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%NsWn0,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%NsSDWn1a,                 &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%NsSDWn1b,                 &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%NsSDWn1c,                 &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call read_field( ifile, Srf%SDWnMax1a,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%SDWnMax1b,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%SDWnMax1c,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%dWn1a,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%dWn1b,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%dWn1c,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%Opd_effective,            &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4
        call read_field( ifile, Srf%SigI,                     &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call alloc_Srf( Srf )
        Type = r8Type
        Size = 8*Srf%NsWn0
        call read_field( ifile, Srf%Wn0,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1a
        call read_field( ifile, Srf%SDWn1a,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1b
        call read_field( ifile, Srf%SDWn1b,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1c
        call read_field( ifile, Srf%SDWn1c,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1a*Srf%NsWn0*(Srf%NsPixel+1)
        call read_field( ifile, Srf%L1a,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1b*Srf%NsWn0*(Srf%NsPixel+1)
        call read_field( ifile, Srf%L1b,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1c*Srf%NsWn0*(Srf%NsPixel+1)
        call read_field( ifile, Srf%L1c,                      &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1c
        call read_field( ifile, Srf%L1cTF,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsWn0*(Srf%NsPixel+1)
        call read_field( ifile, Srf%WnShift1a,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%WnShift1b,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%WnShift1c,                &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%Fcs1a,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%Fcs1b,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%Fcs1c,                    &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%FWhm1a,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%FWhm1b,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call read_field( ifile, Srf%FWhm1c,                   &
                         %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Srf%filename              &
                        (1:len_trim(Srf%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine readsrf
!
!
   subroutine writesrf( Srf,iostatus )
   implicit none
     type(type_Srf)    , intent(in) :: Srf
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
     iostatus = 0
     call open_file_w(Srf%filename                    &
                      (1:len_trim(Srf%filename))      &
                      // char(0), ifile)
     if ( ifile .eq. 0 ) then
        iostatus = 1
     else
        offset = 0
        Type = i4Type
        Size = 4
        call write_field( ifile, Srf%NsPixel,                  &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%NsWn0,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%NsSDWn1a,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%NsSDWn1b,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%NsSDWn1c,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8
        call write_field( ifile, Srf%SDWnMax1a,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%SDWnMax1b,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%SDWnMax1c,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%dWn1a,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%dWn1b,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%dWn1c,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%Opd_effective,            &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = i4Type
        Size = 4
        call write_field( ifile, Srf%SigI,                     &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Type = r8Type
        Size = 8*Srf%NsWn0
        call write_field( ifile, Srf%Wn0,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1a
        call write_field( ifile, Srf%SDWn1a,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1b
        call write_field( ifile, Srf%SDWn1b,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1c
        call write_field( ifile, Srf%SDWn1c,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1a*Srf%NsWn0*(Srf%NsPixel+1)
        call write_field( ifile, Srf%L1a,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1b*Srf%NsWn0*(Srf%NsPixel+1)
        call write_field( ifile, Srf%L1b,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1c*Srf%NsWn0*(Srf%NsPixel+1)
        call write_field( ifile, Srf%L1c,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsSDWn1c
        call write_field( ifile, Srf%L1cTF,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        Size = 8*Srf%NsWn0*(Srf%NsPixel+1)
        call write_field( ifile, Srf%WnShift1a,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%WnShift1b,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%WnShift1c,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%Fcs1a,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%Fcs1b,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%Fcs1c,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%FWhm1a,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%FWhm1b,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call write_field( ifile, Srf%FWhm1c,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
        offset = offset + Size
        call close_file(Srf%filename              &
                        (1:len_trim(Srf%filename))&
                        // char(0), ifile)
     end if
     return
  end subroutine writesrf
!
!
!>  writesrf_netcdf -- Public
!!
!! * Purpose
!!
!!     Write Srf type
!!
!! * Description
!!
!!     Write Srf type
!!
!! * Inputs
!!
!!     - Srf  : type_Srf / type for declaration and allocation of srf parameters
!!
!! * Outputs
!!
!!     - iostatus : integer / error management varaible
!!
!! * InOut
!!
!!     - init_file : integer / flag to initialize the netcdf file
!!
!! * References
!!
!!   SPS
!!
subroutine writesrf_netcdf( Srf,iostatus, init_file )

     implicit none

     type(type_Srf), intent(in) :: Srf
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=SHORT), intent(inout) :: init_file

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! file creation and definition
     if (init_file == 0) then
        call init_srf_data(Srf, ncid)
        init_file = 1
     endif

     ! open netCDF dataset
     status = nf90_open(path = trim(Srf%TopLevelSrf%header%filename), mode = nf90_write, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! writes variables for srf group
     ! current spectral band
     i_band = Srf%TopLevelSrf%Band
     call write_srf_data(ncid, i_band, Srf)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine writesrf_netcdf

   subroutine init_srf_data(Srf, ncid)

     implicit none

     type(type_Srf), intent(in) :: Srf
     integer(kind=LONG), intent(out) :: ncid

     integer(kind=LONG) :: status
     integer(kind=LONG) :: varid, dimid

     ! create netCDF dataset
     status = nf90_create(path = trim(Srf%TopLevelSrf%header%filename), cmode = NF90_NETCDF4, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! define and write global variables

     !
     ! ColFov(NcolFov)
     status = nf90_def_dim(ncid, "NcolFov", Srf%TopLevelSrf%NcolFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "ColFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "column Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Srf%TopLevelSrf%ColFov)
     if (status /= nf90_noerr) call handle_err(status)
     ! LinFov(NlinFov)
     status = nf90_def_dim(ncid, "NlinFov", Srf%TopLevelSrf%NlinFov, dimid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_def_var(ncid, "LinFov", NF90_INT, (/dimid/), varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "units", "N.A.")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_att(ncid, varid, "long_name", "line Fov value")
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(ncid, varid, Srf%TopLevelSrf%LinFov)
     if (status /= nf90_noerr) call handle_err(status)

     ! define global attributes
     ! filename
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "filename", values = trim(Srf%TopLevelSrf%header%filename))
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument name
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsName", values = Srf%TopLevelSrf%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)
     ! instrument mode
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "InsMode", values = Srf%TopLevelSrf%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)
     ! S/W version
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "SoftWareVersion", values = Srf%TopLevelSrf%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)
     ! Operator
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "Operator", values = Srf%TopLevelSrf%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)
     ! Processing step
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ProcessingStep", values = Srf%TopLevelSrf%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)
     ! Conf ID
     status = nf90_put_att(ncid, NF90_GLOBAL, name = "ConfId", values = trim(Srf%TopLevelSrf%header%ConfId))
     if (status /= nf90_noerr) call handle_err(status)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine init_srf_data

   !
   !
   subroutine write_srf_data(ncid, band, Srf)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid, band
     type(type_Srf), intent(in) :: Srf

     integer(kind=LONG),dimension(5) :: start_w, count_w1a, count_w1b, count_w1c
     integer(kind=LONG),dimension(4) :: start_w_agg, count_w_agg1a, count_w_agg1b, count_w_agg1c
     integer(kind=LONG),dimension(4) :: start_w4, count_w4
     integer(kind=LONG),dimension(3) :: start_wtf, count_wtf, start_w3, count_w3

     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid, varid, flag_grp
     integer(kind=LONG) :: nsw_dimid, nsf_dimid, ns1a_dimid, ns1b_dimid, ns1c_dimid, ndims
     integer(kind=LONG), dimension(:), allocatable :: dimids
     character(len=1) :: band_str
     integer(kind=LONG) :: parent
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: nswn0, nssdwn1a, nssdwn1b, nssdwn1c

     ! NsWn0
     nswn0 = Srf%NsWn0
     ! NsSDWn1a
     nssdwn1a = Srf%NsSDWn1a
     ! NsSDWn1b
     nssdwn1b = Srf%NsSDWn1b
     ! NsSDWn1c
     nssdwn1c = Srf%NsSDWn1c

     ! NlinFov,NcolFov,NsubFov,NsWn0,NsSDWn1a
     ! start index
     start_w = (/1,1,1,1,1/)
     ! count index
     count_w1a = (/1,1,Srf%TopLevelSrf%NSubfov,nswn0,nssdwn1a/)
     ! NlinFov,NcolFov,NsubFov,NsWn0,NsSDWn1b
     count_w1b = (/1,1,Srf%TopLevelSrf%NSubfov,nswn0,nssdwn1b/)
     ! NlinFov,NcolFov,NsubFov,NsWn0,NsSDWn1c
     count_w1c = (/1,1,Srf%TopLevelSrf%NSubfov,nswn0,nssdwn1c/)

     ! NlinFov,NcolFov,NsWn0,NsSDWn1a
     start_w_agg = (/1,1,1,1/)
     ! count index
     count_w_agg1a = (/1,1,nswn0,nssdwn1a/)
     count_w_agg1b = (/1,1,nswn0,nssdwn1b/)
     count_w_agg1c = (/1,1,nswn0,nssdwn1c/)

     ! NlinFov,NcolFov,NsSDWn1c
     start_wtf = (/1,1,1/)
     ! count index
     count_wtf = (/1,1,nssdwn1c/)

     ! NlinFov,NcolFov,NsubFov,NsWn0
     start_w4 = (/1,1,1,1/)
     ! count index
     count_w4 = (/1,1,Srf%TopLevelSrf%NSubfov,nswn0/)

     ! NlinFov,NcolFov,NsWn0
     start_w3= (/1,1,1/)
     ! count index
     count_w3 = (/1,1,nswn0/)

     ! definition of the group (one group for one spectral band)
     write(band_str,'(i1)') band

     ! check the group existence
     flag_grp = nf90_inq_ncid(ncid, 'srf_sb'//band_str, grpid)

     if (flag_grp /= nf90_noerr) then
        ! group creation and initialization
        status = nf90_def_grp(ncid, 'srf_sb'//band_str, grpid)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the dimensions
        ! NsWn0
        status = nf90_def_dim(grpid, "NsWn0", nswn0, nsw_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsSDWn1a
        status = nf90_def_dim(grpid, "NsSDWn1a", nssdwn1a, ns1a_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsSDWn1b
        status = nf90_def_dim(grpid, "NsSDWn1b", nssdwn1b, ns1b_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NsSDWn1c
        status = nf90_def_dim(grpid, "NsSDWn1c", nssdwn1c, ns1c_dimid)
        if (status /= nf90_noerr) call handle_err(status)
        ! NSubFov
        status = nf90_def_dim(grpid, "NsubFov", Srf%TopLevelSrf%NsubFov, nsf_dimid)
        if (status /= nf90_noerr) call handle_err(status)

       ! get the ids of global dimensions (NcolFov, NlinFov)
        status = nf90_inquire(ncid, nDimensions=ndims)
        allocate(dimids(ndims), stat=ErrCode)
        parent=0
        ! dimids(1):NcolFov, dimids(2):NlinFov
        status = nf90_inq_dimids(ncid, ndims, dimids, parent)
        if (status /= nf90_noerr) call handle_err(status)

        ! definition of the variables
        ! SubFov
        call write_def_variable(grpid, "SubFov", NF90_INT, (/nsf_dimid/), "N.A.", "subFov value")
        ! Wn0
        call write_def_variable(grpid, "Wn0", NF90_DOUBLE, (/nsw_dimid/), "m-1", "reference wavenumber")
        ! SDWn1a
        call write_def_variable(grpid, "SDWn1a", NF90_DOUBLE, (/ns1a_dimid/), "m-1", "level 1a wavenumber base")
        ! SDWn1b
        call write_def_variable(grpid, "SDWn1b", NF90_DOUBLE, (/ns1b_dimid/), "m-1", "level 1b wavenumber base")
        ! SDWn1c
        call write_def_variable(grpid, "SDWn1c", NF90_DOUBLE, (/ns1c_dimid/), "m-1", "level 1c wavenumber base")
        ! L1a
        call write_def_variable(grpid, "L1a", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,ns1a_dimid/),&
                                 "N.A.", "spectral response function level 1a")
        ! L1a_agg
        call write_def_variable(grpid, "L1a_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,ns1a_dimid/),&
                                 "N.A.", "aggregated spectral response function level 1a")
        ! L1b
        call write_def_variable(grpid, "L1b", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,ns1b_dimid/),&
                                 "N.A.", "spectral response function level 1b")
        ! L1b_agg
        call write_def_variable(grpid, "L1b_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,ns1b_dimid/),&
                                 "N.A.", "aggregated spectral response function level 1b")
        ! L1c
        call write_def_variable(grpid, "L1c", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid,ns1c_dimid/),&
                                 "N.A.", "spectral response function level 1c")
        ! L1c_agg
        call write_def_variable(grpid, "L1c_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid,ns1c_dimid/),&
                                 "N.A.", "aggregated spectral response function level 1c")
        ! L1cTF
        call write_def_variable(grpid, "L1cTF", NF90_DOUBLE, (/ns1c_dimid/),&
                                 "N.A.", "spectral response function level 1c in Fourier space")
        ! WnShift1a
        call write_def_variable(grpid, "WnShift1a", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "m-1", "wavenumber shift for level 1a")
        ! WnShift1a_agg
        call write_def_variable(grpid, "WnShift1a_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "m-1", "aggregated wavenumber shift for level 1a")
        ! WnShift1b
        call write_def_variable(grpid, "WnShift1b", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "m-1", "wavenumber shift for level 1b")
        ! WnShift1b_agg
        call write_def_variable(grpid, "WnShift1b_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "m-1", "aggregated wavenumber shift for level 1b")
        ! WnShift1c
        call write_def_variable(grpid, "WnShift1c", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "m-1", "wavenumber shift for level 1c")
        ! WnShift1c_agg
        call write_def_variable(grpid, "WnShift1c_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "m-1", "aggregated wavenumber shift for level 1c")
        ! Fcs1a
        call write_def_variable(grpid, "Fcs1a", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "N.A.", "spectral calibration function for level 1a")
        ! Fcs1a_agg
        call write_def_variable(grpid, "Fcs1a_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "N.A.", "level 1a aggregated spectral calibration function")
        ! Fcs1b
        call write_def_variable(grpid, "Fcs1b", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "N.A.", "spectral calibration function for level 1b")
        ! Fcs1b_agg
        call write_def_variable(grpid, "Fcs1b_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "N.A.", "level 1b aggregated spectral calibration function")
        ! Fcs1c
        call write_def_variable(grpid, "Fcs1c", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "N.A.", "spectral calibration function for level 1c")
        ! Fcs1c_agg
        call write_def_variable(grpid, "Fcs1c_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "N.A.", "level 1c aggregated spectral calibration function")
        ! Fwhm1a
        call write_def_variable(grpid, "FWhm1a", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "m-1", "apodisation function parameter for level 1a")
        ! FWhm1a_agg
        call write_def_variable(grpid, "FWhm1a_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "m-1", "aggregated apodisation function parameter for level 1a")
        ! Fwhm1b
        call write_def_variable(grpid, "FWhm1b", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "m-1", "apodisation function parameter for level 1b")
        ! FWhm1b_agg
        call write_def_variable(grpid, "FWhm1b_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "m-1", "aggregated apodisation function parameter for level 1b")
        ! Fwhm1c
        call write_def_variable(grpid, "FWhm1c", NF90_DOUBLE, (/dimids(2),dimids(1),nsf_dimid,nsw_dimid/),&
                                 "m-1", "apodisation function parameter for level 1c")
        ! FWhm1c_agg
        call write_def_variable(grpid, "FWhm1c_agg", NF90_DOUBLE, (/dimids(2),dimids(1),nsw_dimid/),&
                                 "m-1", "aggregated apodisation function parameter for level 1c")

        ! write group attributes
        ! Band
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "Band", values = band)
        if (status /= nf90_noerr) call handle_err(status)
        ! SDWnMax1a
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "SDWnMax1a", values = Srf%SDWnMax1a)
        if (status /= nf90_noerr) call handle_err(status)
        ! SDWnMax1b
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "SDWnMax1b", values = Srf%SDWnMax1b)
        if (status /= nf90_noerr) call handle_err(status)
        ! SDWnMax1c
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "SDWnMax1c", values = Srf%SDWnMax1c)
        if (status /= nf90_noerr) call handle_err(status)
        ! dWn1a
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "dWn1a", values = Srf%dWn1a)
        if (status /= nf90_noerr) call handle_err(status)
        ! dWn1b
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "dWn1b", values = Srf%dWn1b)
        if (status /= nf90_noerr) call handle_err(status)
        ! dWn1c
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "dWn1c", values = Srf%dWn1c)
        if (status /= nf90_noerr) call handle_err(status)
        ! Opd_effective
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "Opd_effective", values = Srf%Opd_effective)
        if (status /= nf90_noerr) call handle_err(status)
        ! SigI
        status = nf90_put_att(grpid, NF90_GLOBAL, name= "SigI", values = Srf%SigI)
        if (status /= nf90_noerr) call handle_err(status)

     end if ! end group creation and initialization

     ! write variables
     ! Srf%TopLevelSrf%SubFov is the list of sub-pixel index
     status = nf90_inq_varid(grpid, "SubFov", varid)
     if (status /= nf90_noerr) call handle_err(status)
     status = nf90_put_var(grpid, varid, Srf%TopLevelSrf%SubFov, start=(/1/), count=(/Srf%TopLevelSrf%NSubFov/))
     if (status /= nf90_noerr) call handle_err(status)

     ! Wn0
     call write_variable(grpid, "Wn0", Srf%Wn0)
     ! SDWn1a
     call write_variable(grpid, "SDWn1a", Srf%SDWn1a)
     ! SDWn1b
     call write_variable(grpid, "SDWn1b", Srf%SDWn1b)
     ! SDWn1c
     call write_variable(grpid, "SDWn1c", Srf%SDWn1c)
     ! L1a
     call write_variable(grpid, "L1a", Srf%L1a(:,:,1:Srf%NsPixel), start_w, count_w1a)
     ! L1a_agg
     call write_variable(grpid, "L1a_agg", Srf%L1a(:,:,Srf%NsPixel+1), start_w_agg, count_w_agg1a)
     ! L1b
     call write_variable(grpid, "L1b", Srf%L1b(:,:,1:Srf%NsPixel), start_w, count_w1b)
     ! L1b_agg
     call write_variable(grpid, "L1b_agg", Srf%L1b(:,:,Srf%NsPixel+1), start_w_agg, count_w_agg1b)
     ! L1c
     call write_variable(grpid, "L1c", Srf%L1c(:,:,1:Srf%NsPixel), start_w, count_w1c)
     ! L1c_agg
     call write_variable(grpid, "L1c_agg", Srf%L1c(:,:,Srf%NsPixel+1), start_w_agg, count_w_agg1c)
     ! L1cTF
     call write_variable(grpid, "L1cTF", Srf%L1cTF)
     ! WnShift1a
     call write_variable(grpid, "WnShift1a", Srf%WnShift1a(:,1:Srf%NsPixel), start_w4, count_w4)
     ! WnShift1a_agg
     call write_variable(grpid, "WnShift1a_agg", Srf%WnShift1a(:,Srf%NsPixel+1), start_w3, count_w3)
     ! WnShift1b
     call write_variable(grpid, "WnShift1b", Srf%WnShift1b(:,1:Srf%NsPixel), start_w4, count_w4)
     ! WnShift1b_agg
     call write_variable(grpid, "WnShift1b_agg", Srf%WnShift1b(:,Srf%NsPixel+1), start_w3, count_w3)
     ! WnShift1c
     call write_variable(grpid, "WnShift1c", Srf%WnShift1c(:,1:Srf%NsPixel), start_w4, count_w4)
     ! WnShift1c_agg
     call write_variable(grpid, "WnShift1c_agg", Srf%WnShift1c(:,Srf%NsPixel+1), start_w3, count_w3)
     ! Fcs1a
     call write_variable(grpid, "Fcs1a", Srf%Fcs1a(:,1:Srf%NsPixel), start_w4, count_w4)
     ! Fcs1a_agg
     call write_variable(grpid, "Fcs1a_agg", Srf%Fcs1a(:,Srf%NsPixel+1), start_w3, count_w3)
     ! Fcs1b
     call write_variable(grpid, "Fcs1b", Srf%Fcs1b(:,1:Srf%NsPixel), start_w4, count_w4)
     ! Fcs1b_agg
     call write_variable(grpid, "Fcs1b_agg", Srf%Fcs1b(:,Srf%NsPixel+1), start_w3, count_w3)
     ! Fcs1c
     call write_variable(grpid, "Fcs1c", Srf%Fcs1c(:,1:Srf%NsPixel), start_w4, count_w4)
     ! Fcs1c_agg
     call write_variable(grpid, "Fcs1c_agg", Srf%Fcs1c(:,Srf%NsPixel+1), start_w3, count_w3)
     ! FWhm1a
     call write_variable(grpid, "FWhm1a", Srf%FWhm1a(:,1:Srf%NsPixel), start_w4, count_w4)
     ! FWhm1a_agg
     call write_variable(grpid, "FWhm1a_agg", Srf%FWhm1a(:,Srf%NsPixel+1), start_w3, count_w3)
     ! FWhm1b
     call write_variable(grpid, "FWhm1b", Srf%FWhm1b(:,1:Srf%NsPixel), start_w4, count_w4)
     ! FWhm1b_agg
     call write_variable(grpid, "FWhm1b_agg", Srf%FWhm1b(:,Srf%NsPixel+1), start_w3, count_w3)
     ! FWhm1c
     call write_variable(grpid, "FWhm1c", Srf%FWhm1c(:,1:Srf%NsPixel), start_w4, count_w4)
     ! FWhm1c_agg
     call write_variable(grpid, "FWhm1c_agg", Srf%FWhm1c(:,Srf%NsPixel+1), start_w3, count_w3)

     return

   end subroutine write_srf_data
!
!
!>  readsrf_netcdf -- Public
!!
!! * Purpose
!!
!!     Read srf type
!!
!! * Description
!!
!!     Read srf type
!!
!! * Inputs
!!
!!     - Srf  : type_srf / type for declaration and allocation of srf parameters
!!
!! * Outputs
!!
!!     - Srf  : type_srf / type for declaration and allocation of srf parameters
!!     - iostatus : integer / error management varaible
!!
!! * References
!!
!!   SPS
!!
   subroutine readsrf_netcdf( Srf,iostatus )

     implicit none

     type(type_Srf), intent(inout) :: Srf
     integer(kind=LONG), intent(out) :: iostatus

     integer(kind=LONG) :: ncid, status, i_band

     iostatus = 0

     ! open existing netCDF dataset
     status = nf90_open(path = trim(Srf%TopLevelSrf%header%filename), mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get global data (variables and attributes)
     call get_srf_data(ncid, Srf)

     ! reads variables for srf group
     ! current spectral band
     i_band = Srf%TopLevelSrf%Band
     call read_srf_data(ncid, i_band, Srf)

     ! close access to netCDF dataset
     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine readsrf_netcdf
!
!
subroutine read_srf_data(ncid, band, Srf)

     implicit none

     integer(kind=LONG), intent(in)    :: ncid, band
     type(type_Srf),     intent(inout) :: Srf

     integer(kind=LONG),dimension(5) :: start_w, count_w1a, count_w1b, count_w1c
     integer(kind=LONG),dimension(4) :: start_w_agg, count_w_agg1a, count_w_agg1b, count_w_agg1c
     integer(kind=LONG),dimension(4) :: start_w4, count_w4
     integer(kind=LONG),dimension(3) :: start_wtf, count_wtf, start_w3, count_w3

     integer(kind=LONG) :: status
     integer(kind=LONG) :: grpid

     integer(kind=LONG) :: ndimGrp, nvarGrp, nattGrp, nvar
     integer(kind=LONG), dimension(:), allocatable :: varidsGrp
     integer(kind=LONG) :: ErrCode
     integer(kind=LONG) :: dimid_nswn0, dimid_nssdwn1a, dimid_nssdwn1b, dimid_nssdwn1c
     character(len=1)   :: band_str

     ! get the group id
     write(band_str,'(i1)') band
     status = nf90_inq_ncid(ncid, 'srf_sb'//band_str, grpid)
     if (status /= nf90_noerr) call handle_err(status)

     ! get information about the group dataset
     status = nf90_inquire(grpid, nDimensions=ndimGrp, nVariables=nvarGrp, nAttributes=nattGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! get variable ids from group dataset
     allocate(varidsGrp(nvarGrp), stat=ErrCode)
     status = nf90_inq_varids(grpid, nvar, varidsGrp)
     if (status /= nf90_noerr) call handle_err(status)

     ! read values for group attributes
     ! SDWnMax1a
     status = nf90_get_att(grpid, NF90_GLOBAL, name="SDWnMax1a", values=Srf%SDWnMax1a)
     if (status /= nf90_noerr) call handle_err(status)
     ! SDWnMax1b
     status = nf90_get_att(grpid, NF90_GLOBAL, name="SDWnMax1b", values=Srf%SDWnMax1b)
     if (status /= nf90_noerr) call handle_err(status)
     ! SDWnMax1c
     status = nf90_get_att(grpid, NF90_GLOBAL, name="SDWnMax1c", values=Srf%SDWnMax1c)
     if (status /= nf90_noerr) call handle_err(status)
     ! dWn1a
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dWn1a", values=Srf%dWn1a)
     if (status /= nf90_noerr) call handle_err(status)
     ! dWn1b
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dWn1b", values=Srf%dWn1b)
     if (status /= nf90_noerr) call handle_err(status)
     ! dWn1c
     status = nf90_get_att(grpid, NF90_GLOBAL, name="dWn1c", values=Srf%dWn1c)
     if (status /= nf90_noerr) call handle_err(status)
     ! Opd_effective
     status = nf90_get_att(grpid, NF90_GLOBAL, name="Opd_effective", values=Srf%Opd_effective)
     if (status /= nf90_noerr) call handle_err(status)
     ! SigI
     status = nf90_get_att(grpid, NF90_GLOBAL, name="SigI", values=Srf%SigI)
     if (status /= nf90_noerr) call handle_err(status)

     ! get NsWn0
     status = nf90_inq_dimid(grpid, "NsWn0", dimid_nswn0)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nswn0, len = Srf%NsWn0)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NsSDWn1a
     status = nf90_inq_dimid(grpid, "NsSDWn1a", dimid_nssdwn1a)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nssdwn1a, len = Srf%NsSDWn1a)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NsSDWn1b
     status = nf90_inq_dimid(grpid, "NsSDWn1b", dimid_nssdwn1b)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nssdwn1b, len = Srf%NsSDWn1b)
     if(status /= nf90_NoErr) call handle_err(status)

     ! get NsSDWn1c
     status = nf90_inq_dimid(grpid, "NsSDWn1c", dimid_nssdwn1c)
     if (status /= nf90_noerr) call handle_err(status)

     status = nf90_inquire_dimension(grpid, dimid_nssdwn1c, len = Srf%NsSDWn1c)
     if(status /= nf90_NoErr) call handle_err(status)

     Srf%NsPixel = Srf%TopLevelSrf%NsubFov
     ! Srf allocation
     call alloc_Srf( Srf )

     ! Wn0
     status = nf90_get_var(grpid, varidsGrp(2), Srf%Wn0)
     if (status /= nf90_noerr) call handle_err(status)

    ! NlinFov,NcolFov,NsubFov,NsWn0,NsSDWn1a
     ! start index
     start_w = (/1,1,1,1,1/)
     ! count index
     count_w1a = (/1,1,Srf%TopLevelSrf%NsubFov,Srf%NsWn0,Srf%NsSDWn1a/)
     ! NlinFov,NcolFov,NsubFov,NsWn0,NsSDWn1b
     count_w1b = (/1,1,Srf%TopLevelSrf%NsubFov,Srf%NsWn0,Srf%NsSDWn1b/)
     ! NlinFov,NcolFov,NsubFov,NsWn0,NsSDWn1c
     count_w1c = (/1,1,Srf%TopLevelSrf%NsubFov,Srf%NsWn0,Srf%NsSDWn1c/)

     ! NlinFov,NcolFov,NsWn0,NsSDWn1a
     start_w_agg = (/1,1,1,1/)
     ! count index
     count_w_agg1a = (/1,1,Srf%NsWn0,Srf%NsSDWn1a/)
     count_w_agg1b = (/1,1,Srf%NsWn0,Srf%NsSDWn1b/)
     count_w_agg1c = (/1,1,Srf%NsWn0,Srf%NsSDWn1c/)

     ! NlinFov,NcolFov,NsSDWn1c
     start_wtf = (/1,1,1/)
     ! count index
     count_wtf = (/1,1,Srf%NsSDWn1c/)

     ! NlinFov,NcolFov,NsubFov,NsWn0
     start_w4 = (/1,1,1,1/)
     ! count index
     count_w4 = (/1,1,Srf%TopLevelSrf%NsubFov,Srf%NsWn0/)

     ! NlinFov,NcolFov,NsWn0
     start_w3= (/1,1,1/)
     ! count index
     count_w3 = (/1,1,Srf%NsWn0/)

     ! SDWn1a
     status = nf90_get_var(grpid, varidsGrp(3), Srf%SDWn1a)
     if (status /= nf90_noerr) call handle_err(status)
     ! SDWn1b
     status = nf90_get_var(grpid, varidsGrp(4), Srf%SDWn1b)
     if (status /= nf90_noerr) call handle_err(status)
     ! SDWn1c
     status = nf90_get_var(grpid, varidsGrp(5), Srf%SDWn1c)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1a
     status = nf90_get_var(grpid, varidsGrp(6), Srf%L1a(:,:,1:Srf%NsPixel), start=start_w, count=count_w1a)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1a_agg
     status = nf90_get_var(grpid, varidsGrp(7), Srf%L1a(:,:,Srf%NsPixel+1), start=start_w_agg, count=count_w_agg1a)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1b
     status = nf90_get_var(grpid, varidsGrp(8), Srf%L1b(:,:,1:Srf%NsPixel), start=start_w, count=count_w1b)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1b_agg
     status = nf90_get_var(grpid, varidsGrp(9), Srf%L1b(:,:,Srf%NsPixel+1), start=start_w_agg, count=count_w_agg1b)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1c
     status = nf90_get_var(grpid, varidsGrp(10), Srf%L1c(:,:,1:Srf%NsPixel), start=start_w, count=count_w1c)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1c_agg
     status = nf90_get_var(grpid, varidsGrp(11), Srf%L1c(:,:,Srf%NsPixel+1), start=start_w_agg, count=count_w_agg1c)
     if (status /= nf90_noerr) call handle_err(status)
     ! L1cTF
     status = nf90_get_var(grpid, varidsGrp(12), Srf%L1cTF)
     if (status /= nf90_noerr) call handle_err(status)
     ! WnShift1a
     status = nf90_get_var(grpid, varidsGrp(13), Srf%WnShift1a(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! WnShift1a_agg
     status = nf90_get_var(grpid, varidsGrp(14), Srf%WnShift1a(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! WnShift1b
     status = nf90_get_var(grpid, varidsGrp(15), Srf%WnShift1b(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! WnShift1b_agg
     status = nf90_get_var(grpid, varidsGrp(16), Srf%WnShift1b(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! WnShift1c
     status = nf90_get_var(grpid, varidsGrp(17), Srf%WnShift1c(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! WnShift1c_agg
     status = nf90_get_var(grpid, varidsGrp(18), Srf%WnShift1c(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! Fcs1a
     status = nf90_get_var(grpid, varidsGrp(19), Srf%Fcs1a(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! Fcs1a_agg
     status = nf90_get_var(grpid, varidsGrp(20), Srf%Fcs1a(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! Fcs1b
     status = nf90_get_var(grpid, varidsGrp(21), Srf%Fcs1b(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! Fcs1b_agg
     status = nf90_get_var(grpid, varidsGrp(22), Srf%Fcs1b(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! Fcs1c
     status = nf90_get_var(grpid, varidsGrp(23), Srf%Fcs1c(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! Fcs1c_agg
     status = nf90_get_var(grpid, varidsGrp(24), Srf%Fcs1c(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! FWhm1a
     status = nf90_get_var(grpid, varidsGrp(25), Srf%FWhm1a(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! FWhm1a_agg
     status = nf90_get_var(grpid, varidsGrp(26), Srf%FWhm1a(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! FWhm1b
     status = nf90_get_var(grpid, varidsGrp(27), Srf%FWhm1b(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! FWhm1b_agg
     status = nf90_get_var(grpid, varidsGrp(28), Srf%FWhm1b(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)
     ! FWhm1c
     status = nf90_get_var(grpid, varidsGrp(29), Srf%FWhm1c(:,1:Srf%NsPixel), start=start_w4, count=count_w4)
     if (status /= nf90_noerr) call handle_err(status)
     ! FWhm1c_agg
     status = nf90_get_var(grpid, varidsGrp(30), Srf%FWhm1c(:,Srf%NsPixel+1), start=start_w3, count=count_w3)
     if (status /= nf90_noerr) call handle_err(status)

     ! deallocate local variables
     deallocate(varidsGrp, stat=ErrCode)

     return

   end subroutine read_srf_data

   subroutine get_srf_data(ncid, Srf)

     implicit none

     integer(kind=LONG), intent(in)  :: ncid
     type(type_Srf), intent(inout) :: Srf

     integer(kind=LONG) :: status

     ! note: ColFov(NcolFov), LinFov(NlinFov) are known before reading

     ! read values for global attributes
     ! instrument name
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsName", values=Srf%TopLevelSrf%header%InsName)
     if (status /= nf90_noerr) call handle_err(status)

     ! instrument mode
     status = nf90_get_att(ncid, NF90_GLOBAL, name="InsMode", values=Srf%TopLevelSrf%header%InsMode)
     if (status /= nf90_noerr) call handle_err(status)

     ! S/W version
     status = nf90_get_att(ncid, NF90_GLOBAL, name="SoftWareVersion", values=Srf%TopLevelSrf%header%SoftWareVersion)
     if (status /= nf90_noerr) call handle_err(status)

     ! Operator
     status = nf90_get_att(ncid, NF90_GLOBAL, name="Operator", values=Srf%TopLevelSrf%header%Operator)
     if (status /= nf90_noerr) call handle_err(status)

     ! Processing step
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ProcessingStep", values=Srf%TopLevelSrf%header%ProcessingStep)
     if (status /= nf90_noerr) call handle_err(status)

     ! Conf ID
     status = nf90_get_att(ncid, NF90_GLOBAL, name="ConfId", values=Srf%TopLevelSrf%header%ConfId)
     if (status /= nf90_noerr) call handle_err(status)

     return

   end subroutine get_srf_data
!
!
end module srf_type

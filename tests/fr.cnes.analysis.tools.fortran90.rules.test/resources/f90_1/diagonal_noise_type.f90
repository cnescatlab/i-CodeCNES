!!* diagonal_noise_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> diagonal_noise_type -- Module
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
!!     * type_Diagonal_noise     : type for declaration and allocation of Diagonal_noise
!!     * alloc_Diagonal_Noise    : type_Diagonal_noise allocation
!!     * dalloc_Diagonal_Noise   : type_Diagonal_noise deallocation
!!     * readdiagonal_noise      : type Radiometry_ststa reading
!!     * writediagonal_noise     : type Radiometry_ststa writing
!! * References
!!     
!!      SPS ATBD

module diagonal_noise_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Diagonal_Noise   &
            ,alloc_Diagonal_Noise  &
            ,dalloc_Diagonal_Noise &
            ,readdiagonal_noise    &
            ,writediagonal_noise   

!
   type :: type_Diagonal_Noise
     character(len=500)                               :: filename !< spectrum file name
     real(kind=DOUBLE)                                :: Date !< date
     character(len=2)                                 :: Type !< spectrum type
     integer(kind=LONG)                               :: N_Sample !< spectrum samples number
     integer(kind=LONG)                               :: Ns_First !< first sample
     integer(kind=LONG)                               :: Ns_Last !< last sample
     real(kind=DOUBLE)                                :: Wn_First !< first wavenumber
     real(kind=DOUBLE)                                :: Wn_Last !< last wavenumber
     real(kind=DOUBLE)                                :: WnMax !< maximum wavenumber
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Spectrum_Avg_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: NoiseNedl_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: NoiseNedt_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: DriftT_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothSpectrum_Avg_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothNoiseNedl_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothNoiseNedt_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothDriftT_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: Spectrum_Avg_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: NoiseNedl_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: NoiseNedt_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: DriftT_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothSpectrum_Avg_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothNoiseNedl_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothNoiseNedt_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: SmoothDriftT_I
   end type type_Diagonal_Noise
!
   contains
!
!
   subroutine alloc_Diagonal_Noise( Spectrum )
     implicit none
     type(type_Diagonal_Noise)  , intent(inout) :: Spectrum
     integer(kind=LONG)                         :: ErrCode
     integer(kind=LONG)                         :: ioalloc
!
     ioalloc = 0
     allocate( Spectrum%Wn(Spectrum%N_Sample),                      stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'R' ) then
       allocate( Spectrum%Spectrum_Avg_R(Spectrum%N_Sample),        stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%NoiseNedl_R(Spectrum%N_Sample),           stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%NoiseNedt_R(Spectrum%N_Sample),           stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%DriftT_R(Spectrum%N_Sample),              stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothSpectrum_Avg_R(Spectrum%N_Sample),  stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothNoiseNedl_R(Spectrum%N_Sample),     stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothNoiseNedt_R(Spectrum%N_Sample),     stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothDriftT_R(Spectrum%N_Sample),        stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     end if
     if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'I' ) then
       allocate( Spectrum%Spectrum_Avg_I(Spectrum%N_Sample),        stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%NoiseNedl_I(Spectrum%N_Sample),           stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%NoiseNedt_I(Spectrum%N_Sample),           stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%DriftT_I(Spectrum%N_Sample),              stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothSpectrum_Avg_I(Spectrum%N_Sample),  stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothNoiseNedl_I(Spectrum%N_Sample),     stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothNoiseNedt_I(Spectrum%N_Sample),     stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       allocate( Spectrum%SmoothDriftT_I(Spectrum%N_Sample),        stat=ErrCode )
       if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     end if
     if( ioalloc /= 0 ) then
        write(*,*) 'alloc_Diagonal_Noise Error', ioalloc
        call exit(1)
     end if
     return
   end subroutine alloc_Diagonal_Noise
!
!
   subroutine dalloc_Diagonal_Noise( Spectrum )
     implicit none
     type(type_Diagonal_Noise)  , intent(inout) :: Spectrum
!
     deallocate( Spectrum%Wn )
     if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'R' ) then
       deallocate( Spectrum%Spectrum_Avg_R )
       deallocate( Spectrum%NoiseNedl_R )
       deallocate( Spectrum%NoiseNedt_R )
       deallocate( Spectrum%DriftT_R )
       deallocate( Spectrum%SmoothSpectrum_Avg_R )
       deallocate( Spectrum%SmoothNoiseNedl_R )
       deallocate( Spectrum%SmoothNoiseNedt_R )
       deallocate( Spectrum%SmoothDriftT_R )
     end if
     if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'I' ) then
       deallocate( Spectrum%Spectrum_Avg_I )
       deallocate( Spectrum%NoiseNedl_I )
       deallocate( Spectrum%NoiseNedt_I )
       deallocate( Spectrum%DriftT_I )
       deallocate( Spectrum%SmoothSpectrum_Avg_I )
       deallocate( Spectrum%SmoothNoiseNedl_I )
       deallocate( Spectrum%SmoothNoiseNedt_I )
       deallocate( Spectrum%SmoothDriftT_I )
     end if

     return
   end subroutine dalloc_Diagonal_Noise
!
!
   subroutine readdiagonal_noise( Spectrum,iostatus )
     implicit none
     type(type_Diagonal_Noise), intent(inout) :: Spectrum
!
     integer(kind=LONG)       , intent(out)   :: iostatus
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
      iostatus = 0
      call open_file_r(Spectrum%filename(1:len_trim(Spectrum%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Date, %VAL(Type),                  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call read_field( ifile, Spectrum%Type, %VAL(Type),                  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Spectrum%N_Sample, %VAL(Type),              &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_First, %VAL(Type),              &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Ns_Last, %VAL(Type),               &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Spectrum%Wn_First, %VAL(Type),              &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%Wn_Last, %VAL(Type),               &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%WnMax, %VAL(Type),                 &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Spectrum%dWn, %VAL(Type),                   &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Diagonal_Noise( Spectrum )
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call read_field( ifile, Spectrum%Wn, %VAL(Type),                    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'R' ) then
           call read_field( ifile, Spectrum%Spectrum_Avg_R, %VAL(Type),        &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%NoiseNedl_R, %VAL(Type),           &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%NoiseNedt_R, %VAL(Type),           &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%DriftT_R, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothSpectrum_Avg_R, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothNoiseNedl_R, %VAL(Type),     &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothNoiseNedt_R, %VAL(Type),     &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothDriftT_R, %VAL(Type),        &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'I' ) then
           call read_field( ifile, Spectrum%Spectrum_Avg_I, %VAL(Type),        &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%NoiseNedl_I, %VAL(Type),           &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%NoiseNedt_I, %VAL(Type),           &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%DriftT_I, %VAL(Type),              &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothSpectrum_Avg_I, %VAL(Type),  &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothNoiseNedl_I, %VAL(Type),     &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothNoiseNedt_I, %VAL(Type),     &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Spectrum%SmoothDriftT_I, %VAL(Type),        &
                                   %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
!
         call close_file(Spectrum%filename(1:len_trim(Spectrum%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine readdiagonal_noise
!
!
   subroutine writediagonal_noise( Spectrum,iostatus )
     implicit none
     type(type_Diagonal_Noise), intent(inout) :: Spectrum
!
     integer(kind=LONG)       , intent(out)   :: iostatus
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
      iostatus = 0
      call open_file_w(Spectrum%filename(1:len_trim(Spectrum%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum%Date, %VAL(Type),                  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call write_field( ifile, Spectrum%Type, %VAL(Type),                  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Spectrum%N_Sample, %VAL(Type),              &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Ns_First, %VAL(Type),              &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Ns_Last, %VAL(Type),               &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Spectrum%Wn_First, %VAL(Type),              &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%Wn_Last, %VAL(Type),               &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%WnMax, %VAL(Type),                 &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Spectrum%dWn, %VAL(Type),                   &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Spectrum%N_Sample
         call write_field( ifile, Spectrum%Wn, %VAL(Type),                    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'R' ) then
           call write_field( ifile, Spectrum%Spectrum_Avg_R, %VAL(Type),        &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%NoiseNedl_R, %VAL(Type),           &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%NoiseNedt_R, %VAL(Type),           &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%DriftT_R, %VAL(Type),              &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothSpectrum_Avg_R, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothNoiseNedl_R, %VAL(Type),     &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothNoiseNedt_R, %VAL(Type),     &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothDriftT_R, %VAL(Type),        &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( trim(Spectrum%Type) == 'RI' .or. trim(Spectrum%Type) == 'I' ) then
           call write_field( ifile, Spectrum%Spectrum_Avg_I, %VAL(Type),        &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%NoiseNedl_I, %VAL(Type),           &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%NoiseNedt_I, %VAL(Type),           &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%DriftT_I, %VAL(Type),              &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothSpectrum_Avg_I, %VAL(Type),  &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothNoiseNedl_I, %VAL(Type),     &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothNoiseNedt_I, %VAL(Type),     &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Spectrum%SmoothDriftT_I, %VAL(Type),        &
                                    %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
!
         call close_file(Spectrum%filename(1:len_trim(Spectrum%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine writediagonal_noise
!
!
end module diagonal_noise_type

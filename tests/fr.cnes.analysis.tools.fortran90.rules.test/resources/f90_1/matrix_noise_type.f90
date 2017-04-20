!!* matrix_noise_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> matrix_noise_type -- Module
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
!!     * type_Matrix_noise     : type for declaration and allocation of Matrix_noise
!!     * alloc_Matrix_noise    : type_Matrix_noise allocation
!!     * dalloc_Matrix_noise   : type_Matrix_noise deallocation
!!     * readmatrix_noise      : type Radiometry_ststa reading
!!     * writematrix_noise     : type Radiometry_ststa writing
!! * References
!!     
!!      SPS ATBD

module matrix_noise_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Matrix_Noise   &
            ,alloc_Matrix_Noise  &
            ,dalloc_Matrix_Noise &
            ,readmatrix_noise    &
            ,writematrix_noise   

!
   type :: type_Matrix_Noise
     character(len=500)                               :: filename !< spectrum file name
     real(kind=DOUBLE)                                :: Date !< date
     character(len=2)                                 :: Type !< spectrum type
     character(len=100)                               :: Matrix_Type
     integer(kind=LONG)                               :: N_Sample !< spectrum samples number
     integer(kind=LONG)                               :: Ns_First !< first sample
     integer(kind=LONG)                               :: Ns_Last !< last sample
     real(kind=DOUBLE)                                :: Wn_First !< first wavenumber
     real(kind=DOUBLE)                                :: Wn_Last !< last wavenumber
     real(kind=DOUBLE)                                :: WnMax !< maximum wavenumber
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Wn !< wavenumber sampling
     real(kind=DOUBLE)    , dimension(:), allocatable :: Covariance_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: Correlation_R
     real(kind=DOUBLE)    , dimension(:), allocatable :: Covariance_I
     real(kind=DOUBLE)    , dimension(:), allocatable :: Correlation_I
   end type type_Matrix_Noise
!
   contains
!
!
   subroutine alloc_Matrix_Noise( Matrix )
     implicit none
     type(type_Matrix_noise)    , intent(inout) :: Matrix
     integer(kind=LONG)                         :: ErrCode
     integer(kind=LONG)                         :: ioalloc
     integer(kind=LONG)                         :: vector_size
!
     ioalloc = 0
     vector_size = Matrix%N_Sample*(Matrix%N_Sample+1)/2
     allocate( Matrix%Wn(Matrix%N_Sample),                stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'R' ) then
       if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
         allocate( Matrix%Covariance_R(vector_size),          stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
         allocate( Matrix%Correlation_R(vector_size),         stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
         allocate( Matrix%Covariance_R(vector_size),          stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
         allocate( Matrix%Correlation_R(vector_size),         stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       else
         write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
         call exit(1)
       end if
     end if
     if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'I' ) then
       if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
         allocate( Matrix%Covariance_I(vector_size),          stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
         allocate( Matrix%Correlation_I(vector_size),         stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
         allocate( Matrix%Covariance_I(vector_size),          stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
         allocate( Matrix%Correlation_I(vector_size),         stat=ErrCode )
         if( ErrCode /= 0 ) ioalloc = ioalloc + 1
       else
         write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
         call exit(1)
       end if
     end if
     if( ioalloc /= 0 ) then
        write(*,*) 'alloc_Matrix_Noise Error', ioalloc
        call exit(1)
     end if
     return
   end subroutine alloc_Matrix_Noise
!
!
   subroutine dalloc_Matrix_Noise( Matrix )
     implicit none
     type(type_Matrix_noise)    , intent(inout) :: Matrix
!
     deallocate( Matrix%Wn )
     if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'R' ) then
       if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
         deallocate( Matrix%Covariance_R )
         deallocate( Matrix%Correlation_R )
       else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
         deallocate( Matrix%Covariance_R )
       else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
         deallocate( Matrix%Correlation_R )
       else
         write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
         call exit(1)
       end if
     end if
     if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'I' ) then
       if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
         deallocate( Matrix%Covariance_I )
         deallocate( Matrix%Correlation_I )
       else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
         deallocate( Matrix%Covariance_I )
       else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
         deallocate( Matrix%Correlation_I )
       else
         write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
         call exit(1)
       end if
     end if

     return
   end subroutine dalloc_Matrix_Noise
!
!
   subroutine readmatrix_noise( Matrix,iostatus )
     implicit none
     type(type_Matrix_noise)  , intent(inout) :: Matrix
!
     integer(kind=LONG)       , intent(out)   :: iostatus
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
      iostatus = 0
      call open_file_r(Matrix%filename(1:len_trim(Matrix%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call read_field( ifile, Matrix%Date, %VAL(Type),                  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call read_field( ifile, Matrix%Type, %VAL(Type),                  &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch1Type
         Size = 1*100
         call read_field( ifile, Matrix%Matrix_Type, %VAL(Type),           &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Matrix%N_Sample, %VAL(Type),              &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Matrix%Ns_First, %VAL(Type),              &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Matrix%Ns_Last, %VAL(Type),               &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Matrix%Wn_First, %VAL(Type),              &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Matrix%Wn_Last, %VAL(Type),               &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Matrix%WnMax, %VAL(Type),                 &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Matrix%dWn, %VAL(Type),                   &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Matrix_noise( Matrix )
         Type = r8Type
         Size = 8*Matrix%N_Sample
         call read_field( ifile, Matrix%Wn, %VAL(Type),                    &
                                 %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'R' ) then
            Size = 8*Matrix%N_Sample*(Matrix%N_Sample+1)/2
            if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
              call read_field( ifile, Matrix%Covariance_R, %VAL(Type),     &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
              call read_field( ifile, Matrix%Correlation_R, %VAL(Type),    &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
              call read_field( ifile, Matrix%Covariance_R, %VAL(Type),     &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
              call read_field( ifile, Matrix%Correlation_R, %VAL(Type),    &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else
              write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
              call exit(1)
            end if
         end if
         if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'I' ) then
            Size = 8*Matrix%N_Sample*(Matrix%N_Sample+1)/2
            if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
              call read_field( ifile, Matrix%Covariance_I, %VAL(Type),     &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
              call read_field( ifile, Matrix%Correlation_I, %VAL(Type),    &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
              call read_field( ifile, Matrix%Covariance_I, %VAL(Type),     &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
              call read_field( ifile, Matrix%Correlation_I, %VAL(Type),    &
                                      %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else
              write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
              call exit(1)
            end if
         end if
!
         call close_file(Matrix%filename(1:len_trim(Matrix%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine readmatrix_noise
!
!
   subroutine writematrix_noise( Matrix,iostatus )
     implicit none
     type(type_Matrix_noise)  , intent(inout) :: Matrix
!
     integer(kind=LONG)       , intent(out)   :: iostatus
     integer(kind=DOUBLE)                     :: ifile
     integer(kind=LONG)                       :: offset
     integer(kind=LONG)                       :: Type
     integer(kind=LONG)                       :: Size
!
      iostatus = 0
      call open_file_w(Matrix%filename(1:len_trim(Matrix%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = r8Type
         Size = 8
         call write_field( ifile, Matrix%Date, %VAL(Type),                  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch2Type
         Size = 2
         call write_field( ifile, Matrix%Type, %VAL(Type),                  &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = ch1Type
         Size = 1*100
         call write_field( ifile, Matrix%Matrix_Type, %VAL(Type),           &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Matrix%N_Sample, %VAL(Type),              &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Matrix%Ns_First, %VAL(Type),              &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Matrix%Ns_Last, %VAL(Type),               &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Matrix%Wn_First, %VAL(Type),              &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Matrix%Wn_Last, %VAL(Type),               &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Matrix%WnMax, %VAL(Type),                 &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Matrix%dWn, %VAL(Type),                   &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8*Matrix%N_Sample
         call write_field( ifile, Matrix%Wn, %VAL(Type),                    &
                                  %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'R' ) then
            Size = 8*Matrix%N_Sample*(Matrix%N_Sample+1)/2
            if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
              call write_field( ifile, Matrix%Covariance_R, %VAL(Type),     &
                                       %VAL(Size), %VAL(offset) )
              offset = offset + Size
              call write_field( ifile, Matrix%Correlation_R, %VAL(Type),    &
                                       %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
            else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
            else
              write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
              call exit(1)
            end if
         end if
         if( trim(Matrix%Type) == 'RI' .or. trim(Matrix%Type) == 'I' ) then
            Size = 8*Matrix%N_Sample*(Matrix%N_Sample+1)/2
            if( trim(Matrix%Matrix_Type) == 'CovarCorrel' ) then
              call write_field( ifile, Matrix%Covariance_I, %VAL(Type),     &
                                       %VAL(Size), %VAL(offset) )
              offset = offset + Size
              call write_field( ifile, Matrix%Correlation_I, %VAL(Type),    &
                                       %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Covariance' ) then
              call write_field( ifile, Matrix%Covariance_I, %VAL(Type),     &
                                       %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else if( trim(Matrix%Matrix_Type) == 'Correlation' ) then
              call write_field( ifile, Matrix%Correlation_I, %VAL(Type),    &
                                       %VAL(Size), %VAL(offset) )
              offset = offset + Size
            else
              write(*,*) 'Matrix_Type Error ',Matrix%Matrix_Type
              call exit(1)
            end if
         end if
!
         call close_file(Matrix%filename(1:len_trim(Matrix%filename))    &
                         // char(0), ifile)
      end if
!
   return
   end subroutine writematrix_noise
!
!
end module matrix_noise_type
